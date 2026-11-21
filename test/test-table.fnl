;;; Schema, record codec, and table-layer tests.

(local t (require :harness))
(local schema (require :umbel.schema))
(local table-m (require :umbel.table))

(local tmpdir (os.getenv :TMPDIR))
(local dir (if (and tmpdir (not= tmpdir "")) tmpdir :/tmp))
(local dbfile (.. dir :/umbel-test-table.db))
(os.remove dbfile)

;; ---- schema -------------------------------------------------------------

(local pay-schema [{:name :id :type :uint64}
                   {:name :amount :type :int32}
                   {:name :label :type :string :size 12}])

(t.deftest "schema: record size and round trip"
  (fn []
    (t.assert= 24 (schema.record-size pay-schema) "8 + 4 + 12 bytes")
    (local rec (schema.encode pay-schema {:id 7 :amount -42 :label :lunch}))
    (t.assert= 24 (length rec) "packed record matches schema size")
    (local v (schema.decode pay-schema rec))
    (t.assert= 7 v.id "u64 round trips")
    (t.assert= -42 v.amount "negative i32 round trips")
    (t.assert= :lunch v.label "string round trips zero-trimmed")))

(t.deftest "schema: binary-safe strings and mid-record string offset"
  (fn []
    (local rec (schema.encode pay-schema {:id 1 :amount 0 :label "\000\001ab"}))
    (t.assert= "\000\001ab" (. (schema.decode pay-schema rec) :label)
               "embedded leading NULs survive")
    (local mid-schema [{:name :tag :type :string :size 8}
                       {:name :n :type :int64}])
    (local mrec (schema.encode mid-schema {:tag :hi :n 999999999}))
    (local mv (schema.decode mid-schema mrec))
    (t.assert= :hi mv.tag "leading string round trips")
    (t.assert= 999999999 mv.n "field after string decodes at right offset")))

(t.deftest "schema: catalog round trip"
  (fn []
    (local cat (schema.catalog-to-page pay-schema))
    (t.assert= 4096 (length cat) "catalog page is one page")
    (local back (schema.catalog-from-page cat))
    (t.assert= 3 (length back) "field count round trips")
    (t.assert= :id (. (. back 1) :name))
    (t.assert= :uint64 (. (. back 1) :type))
    (t.assert= 12 (. (. back 3) :size))
    (t.assert= :string (. (. back 3) :type))))

(t.deftest "schema: rejects bad input"
  (fn []
    (t.assert-error (fn [] (schema.encode pay-schema {:id 1 :amount 1}))
                    "missing field errors")
    (t.assert-error (fn []
                      (schema.encode pay-schema
                                     {:id 1
                                      :amount 1
                                      :label "way too long string"}))
                    "oversized string errors")))

;; ---- table layer --------------------------------------------------------

(t.deftest "table: create, insert across pages, read by tid"
  (fn []
    (local tbl (table-m.create dbfile pay-schema))
    (t.assert= 0 (table-m.count tbl) "table starts empty")
    ;; 200 records of 24 bytes; a fresh page holds ~127 (24+8 bytes each)
    (local tids [])
    (for [i 1 200]
      (local rec
             (schema.encode pay-schema
                            {:id i :amount (* i 3) :label (.. :row i)}))
      (table.insert tids (table-m.insert tbl rec)))
    (t.assert= 200 (table-m.count tbl) "all 200 records live")
    (each [i tid (ipairs tids)]
      (local bytes (table-m.read tbl tid))
      (t.assert= (.. :row i) (. (schema.decode pay-schema bytes) :label)
                 (.. "record " i " round trips")))
    (t.assert= table-m.FIRST-DATA-PAGE (. (. tids 1) :page)
               "first record lands on the first data page")
    (t.assert= true (> (. (. tids 200) :page) (. (. tids 1) :page))
               "records spill onto later pages")
    (table-m.close tbl)))

(t.deftest "table: reopen loads schema and records"
  (fn []
    (local tbl (table-m.open dbfile nil))
    (t.assert= 200 (table-m.count tbl) "records survive reopen")
    (t.assert= :label (. (. tbl.schema 3) :name) "schema reloaded from catalog")
    (table-m.close tbl)))

(t.deftest "table: delete and compact-page"
  (fn []
    (local tbl (table-m.open dbfile nil))
    (var target nil)
    (each [tid bytes (table-m.scan tbl)]
      (when (= (. (schema.decode pay-schema bytes) :id) 100)
        (set target tid)))
    (t.assert= 100 (. (schema.decode pay-schema (table-m.read tbl target)) :id)
               "found record 100")
    (table-m.delete tbl target)
    (t.assert-nil (table-m.read tbl target) "deleted record reads nil")
    (t.assert= 199 (table-m.count tbl) "count drops")
    (table-m.compact-page tbl target.page)
    (t.assert= 199 (table-m.count tbl) "compact keeps the rest alive")
    (table-m.close tbl)))

(t.deftest "table: tiny pool still correct through eviction"
  (fn []
    ;; pool of 2, 60 pages of access — forces constant eviction
    (local bufmgr (require :umbel.bufmgr))
    (local heapfile (require :umbel.heapfile))
    (local page (require :umbel.page))
    (local pagedir (require :umbel.pagedir))
    (local db2 (.. dir :/umbel-test-pool.db))
    (os.remove db2)
    (local hf (heapfile.open db2))
    (heapfile.extend hf (schema.catalog-to-page pay-schema))
    (heapfile.extend hf (pagedir.to-page (pagedir.init)))
    (local buf (bufmgr.new 2))
    (local tbl (table-m.load hf buf))
    (for [i 1 60]
      (table-m.insert tbl
                      (schema.encode pay-schema
                                     {:id i :amount i :label :tiny-pool})))
    (table-m.flush tbl)
    (var bad 0)
    (each [tid bytes (table-m.scan tbl)]
      (local v (schema.decode pay-schema bytes))
      (when (not= v.label :tiny-pool) (set bad (+ bad 1))))
    (t.assert= 0 bad "every record intact after eviction churn")
    (t.assert= 60 (table-m.count tbl) "all 60 accounted for")
    (table-m.close tbl)
    (os.remove db2)))
