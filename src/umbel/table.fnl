;;; umbel.table — the table layer ties everything together.
;;
;; File layout (roadmap task 4.3):
;;
;;   page 0  catalog  — the table's persisted schema
;;   page 1  page directory — free space per data page
;;   page 2+ data pages — slotted pages of packed records
;;
;; All data-page traffic goes through the buffer manager. Inserts route
;; through the page directory (first fit), extending the heap file when
;; no page has room. Reads address records by TupleId {page slot} with
;; 1-based slot numbers, matching the page layer.

(local page (require :umbel.page))
(local heapfile (require :umbel.heapfile))
(local pagedir (require :umbel.pagedir))
(local bufmgr (require :umbel.bufmgr))
(local schema-m (require :umbel.schema))
(local wal (require :umbel.wal))

(local CATALOG-PAGE 0)
(local DIR-PAGE 1)
(local FIRST-DATA-PAGE 2)

(fn load [hf buf wal-handle]
  (local catalog-slot (bufmgr.get-page buf hf CATALOG-PAGE))
  (local schema (schema-m.catalog-from-page catalog-slot.data))
  (bufmgr.release buf catalog-slot false)
  (local dir-slot (bufmgr.get-page buf hf DIR-PAGE))
  (local dir (pagedir.from-page dir-slot.data))
  (bufmgr.release buf dir-slot false)
  {:hf hf :buf buf :schema schema :dir dir :wal wal-handle :active-txn nil})

(fn create [path schema opts]
  "Create a new table file at path with the given schema."
  (when (= schema nil) (error "table: create needs a schema"))
  (local hf (heapfile.open path))
  (heapfile.extend hf (schema-m.catalog-to-page schema))
  (heapfile.extend hf (pagedir.to-page (pagedir.init)))
  (local w (when (and opts opts.wal)
             (let [wh (wal.open opts.wal)]
               (wal.checkpoint wh)
               wh)))
  (load hf (bufmgr.new 16) w))

(fn file-exists? [path]
  (let [f (io.open path "r")]
    (if f (do (f:close) true) false)))

(fn open* [path schema opts]
  "Open a table, creating it from schema when the file is absent.
When opts.wal is provided, replays any uncheckpointed WAL records on open."
  (local existed (file-exists? path))
  (if existed
      (let [hf (heapfile.open path)
            w (when (and opts opts.wal)
                (wal.recover opts.wal hf)
                (wal.open opts.wal))]
        (load hf (bufmgr.new 16) w))
      (create path schema opts)))

;; -- transactions & WAL helpers -------------------------------------------

(fn begin-txn [tbl]
  (when (= tbl.wal nil) (error "table: transactions require WAL"))
  (when (not= tbl.active-txn nil) (error "table: transaction already active"))
  (set tbl.active-txn (wal.begin tbl.wal))
  tbl.active-txn)

(fn commit-txn [tbl]
  (when (= tbl.active-txn nil) (error "table: no active transaction"))
  (wal.commit tbl.wal tbl.active-txn)
  (set tbl.active-txn nil))

(fn abort-txn [tbl]
  (when (= tbl.active-txn nil) (error "table: no active transaction"))
  (wal.abort tbl.wal tbl.active-txn)
  (set tbl.active-txn nil))

(fn with-wal [tbl f]
  "Run f(txn-id) under the active WAL txn, or an auto-committed one."
  (if (= tbl.wal nil)
      (f nil)
      (let [auto? (= tbl.active-txn nil)
            txn (if auto? (wal.begin tbl.wal) tbl.active-txn)]
        (let [res [(f txn)]]
          (if auto?
              (wal.commit tbl.wal txn)
              (wal.flush tbl.wal))
          (table.unpack res)))))

;; -- directory persistence ------------------------------------------------

(fn write-dir [tbl]
  "Write the in-memory directory back to its page through the pool."
  (local s (bufmgr.get-page tbl.buf tbl.hf DIR-PAGE))
  (set s.data (pagedir.to-page tbl.dir))
  (bufmgr.release tbl.buf s true))

;; -- operations -----------------------------------------------------------

(fn required-space [record]
  "Bytes of page gap a record insert needs (data + a fresh slot)."
  (+ (length record) page.SLOT-SIZE))

(fn insert [tbl record]
  "Insert packed record bytes; return the TupleId {page slot}."
  (with-wal tbl
    (fn [txn]
      (local old-dir (when txn (pagedir.to-page tbl.dir)))
      (local page-id (or (pagedir.find-free tbl.dir (required-space record))
                         (let [id (heapfile.extend tbl.hf (page.page-init))]
                           (pagedir.add-entry tbl.dir id (page.free-space (page.page-init)))
                           id)))
      (local slot (bufmgr.get-page tbl.buf tbl.hf page-id))
      (local old-page slot.data)
      (local (data n) (page.add-item old-page record))
      (pagedir.update-free tbl.dir page-id (page.free-space data))
      (when txn
        (wal.append tbl.wal txn page-id 0 old-page data)
        (wal.append tbl.wal txn DIR-PAGE 0 old-dir (pagedir.to-page tbl.dir)))
      (set slot.data data)
      (bufmgr.release tbl.buf slot true)
      {:page page-id :slot n})))

(fn read [tbl tid]
  "Read record bytes by TupleId; nil when the slot is dead."
  (local slot (bufmgr.get-page tbl.buf tbl.hf tid.page))
  (local bytes (page.get-item slot.data tid.slot))
  (bufmgr.release tbl.buf slot false)
  bytes)

(fn delete [tbl tid]
  "Tombstone the record at TupleId."
  (with-wal tbl
    (fn [txn]
      (local slot (bufmgr.get-page tbl.buf tbl.hf tid.page))
      (local old-page slot.data)
      (local new-page (page.delete-item old-page tid.slot))
      (when txn
        (wal.append tbl.wal txn tid.page 0 old-page new-page))
      (set slot.data new-page)
      (bufmgr.release tbl.buf slot true))))

(fn compact-page [tbl page-id]
  "Defragment one page; reclaims tombstoned record bytes."
  (with-wal tbl
    (fn [txn]
      (local old-dir (when txn (pagedir.to-page tbl.dir)))
      (local slot (bufmgr.get-page tbl.buf tbl.hf page-id))
      (local old-page slot.data)
      (local new-page (page.compact old-page))
      (pagedir.update-free tbl.dir page-id (page.free-space new-page))
      (when txn
        (wal.append tbl.wal txn page-id 0 old-page new-page)
        (wal.append tbl.wal txn DIR-PAGE 0 old-dir (pagedir.to-page tbl.dir)))
      (set slot.data new-page)
      (bufmgr.release tbl.buf slot true))))

(fn scan [tbl]
  "Iterate every live record as tid, bytes across all data pages."
  (coroutine.wrap
   (fn []
     (for [page-id FIRST-DATA-PAGE (- (heapfile.num-pages tbl.hf) 1)]
       (local slot (bufmgr.get-page tbl.buf tbl.hf page-id))
       (local data slot.data)
       (bufmgr.release tbl.buf slot false)
       (page.each-item data
                       (fn [n bytes]
                         (coroutine.yield {:page page-id :slot n} bytes)))))))

(fn count [tbl]
  "Number of live records."
  (var n 0)
  (each [_ _ (scan tbl)] (set n (+ n 1)))
  n)

(fn flush [tbl]
  "Persist the directory and all dirty pages."
  (write-dir tbl)
  (bufmgr.flush-all tbl.buf tbl.hf))

(fn close* [tbl]
  "Flush everything, write a WAL checkpoint when enabled, and close."
  (flush tbl)
  (when tbl.wal
    (wal.checkpoint tbl.wal)
    (wal.close tbl.wal))
  (heapfile.close tbl.hf))

(fn field [tbl name]
  "Return the schema field with this name, or nil."
  (var found nil)
  (each [_ f (ipairs tbl.schema)]
    (when (and (= found nil) (= f.name name)) (set found f)))
  found)

{:CATALOG-PAGE CATALOG-PAGE :DIR-PAGE DIR-PAGE :FIRST-DATA-PAGE FIRST-DATA-PAGE
 :create create :open open* :load load :insert insert :read read :delete delete
 :compact-page compact-page :scan scan :count count :flush flush :close close*
 :field field :write-dir write-dir :begin-txn begin-txn :commit-txn commit-txn
 :abort-txn abort-txn}
