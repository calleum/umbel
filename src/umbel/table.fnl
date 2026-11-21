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

(local CATALOG-PAGE 0)
(local DIR-PAGE 1)
(local FIRST-DATA-PAGE 2)

(fn load [hf buf]
  (local catalog-slot (bufmgr.get-page buf hf CATALOG-PAGE))
  (local schema (schema-m.catalog-from-page catalog-slot.data))
  (bufmgr.release buf catalog-slot false)
  (local dir-slot (bufmgr.get-page buf hf DIR-PAGE))
  (local dir (pagedir.from-page dir-slot.data))
  (bufmgr.release buf dir-slot false)
  {:hf hf :buf buf :schema schema :dir dir})

(fn create [path schema]
  "Create a new table file at path with the given schema."
  (when (= schema nil) (error "table: create needs a schema"))
  (local hf (heapfile.open path))
  (heapfile.extend hf (schema-m.catalog-to-page schema))
  (heapfile.extend hf (pagedir.to-page (pagedir.init)))
  (local buf (bufmgr.new 16))
  (load hf buf))

(fn file-exists? [path]
  (let [f (io.open path "r")]
    (if f (do (f:close) true) false)))

(fn open* [path schema]
  "Open a table, creating it from schema when the file is absent."
  (local existed (file-exists? path))
  (if existed
      (load (heapfile.open path) (bufmgr.new 16))
      (create path schema)))

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
  (local page-id (or (pagedir.find-free tbl.dir (required-space record))
                     (let [id (heapfile.extend tbl.hf (page.page-init))]
                       (pagedir.add-entry tbl.dir id (page.free-space (page.page-init)))
                       id)))
  (local slot (bufmgr.get-page tbl.buf tbl.hf page-id))
  (local (data n) (page.add-item slot.data record))
  (set slot.data data)
  (bufmgr.release tbl.buf slot true)
  (pagedir.update-free tbl.dir page-id (page.free-space data))
  {:page page-id :slot n})

(fn read [tbl tid]
  "Read record bytes by TupleId; nil when the slot is dead."
  (local slot (bufmgr.get-page tbl.buf tbl.hf tid.page))
  (local bytes (page.get-item slot.data tid.slot))
  (bufmgr.release tbl.buf slot false)
  bytes)

(fn delete [tbl tid]
  "Tombstone the record at TupleId."
  (local slot (bufmgr.get-page tbl.buf tbl.hf tid.page))
  (set slot.data (page.delete-item slot.data tid.slot))
  (bufmgr.release tbl.buf slot true))

(fn compact-page [tbl page-id]
  "Defragment one page; reclaims tombstoned record bytes."
  (local slot (bufmgr.get-page tbl.buf tbl.hf page-id))
  (set slot.data (page.compact slot.data))
  (bufmgr.release tbl.buf slot true)
  (pagedir.update-free tbl.dir page-id (page.free-space slot.data)))

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
  "Flush everything and close the heap file."
  (flush tbl)
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
 :field field :write-dir write-dir}
