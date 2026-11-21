;;; umbel.table — table layer tying catalog, page dir, bufmgr, WAL, and 2PL.
;;
;; File layout (roadmap task 4.3):
;;
;;   page 0  catalog        — persisted schema
;;   page 1  page directory — free space per data page (rebuilt on recovery)
;;   page 2+ data pages     — slotted pages with u64 pageLSN headers
;;
;; Concurrency & durability:
;;   - Strict 2PL (umbel.lockmgr) at tuple granularity for read/insert/delete,
;;     and page granularity for compact-page, supporting interleaved concurrent
;;     transactions (explicit txn ids or coroutine-driven).
;;   - POSIX flock (heapfile.lock) available for multi-process coordination.
;;   - ARIES WAL-before-data enforced via bufmgr's before-write hook
;;     (wal.flushed-lsn >= page.get-lsn before any dirty eviction or flush).
;;   - Clean close fsyncs data pages BEFORE writing the WAL checkpoint.

(local page (require :umbel.page))
(local heapfile (require :umbel.heapfile))
(local pagedir (require :umbel.pagedir))
(local bufmgr (require :umbel.bufmgr))
(local schema-m (require :umbel.schema))
(local wal (require :umbel.wal))
(local lockmgr (require :umbel.lockmgr))

(local CATALOG-PAGE 0)
(local DIR-PAGE 1)
(local FIRST-DATA-PAGE 2)

(fn tid-key [tid]
  (.. "tid:" tid.page ":" tid.slot))

(fn page-key [page-id]
  (.. "page:" page-id))

(fn make-bufmgr [pool-size wal-handle]
  (bufmgr.new (or pool-size 16)
              {:before-write (when wal-handle
                               (fn [page-id data]
                                 (when (>= page-id FIRST-DATA-PAGE)
                                   (wal.ensure-flushed wal-handle
                                                       (page.get-lsn data)))))}))

(fn rebuild-dir [hf]
  "Rebuild page directory accurately from all live data pages on disk."
  (local d (pagedir.init))
  (for [pid FIRST-DATA-PAGE (- (heapfile.num-pages hf) 1)]
    (local data (heapfile.read-page hf pid))
    (pagedir.add-entry d pid (page.free-space data)))
  (heapfile.write-page hf DIR-PAGE (pagedir.to-page d))
  (heapfile.sync hf)
  d)

(fn load [hf buf wal-handle]
  (local catalog-slot (bufmgr.get-page buf hf CATALOG-PAGE))
  (local schema (schema-m.catalog-from-page catalog-slot.data))
  (bufmgr.release buf catalog-slot false)
  (local dir-slot (bufmgr.get-page buf hf DIR-PAGE))
  (local dir (pagedir.from-page dir-slot.data))
  (bufmgr.release buf dir-slot false)
  {: hf
   : buf
   : schema
   : dir
   :wal wal-handle
   :lm (lockmgr.new)
   :active-txn nil
   :active-txns {}})

(fn create [path schema opts]
  "Create a new table file at path with the given schema."
  (when (= schema nil) (error "table: create needs a schema"))
  (local hf (heapfile.open path))
  (heapfile.extend hf (schema-m.catalog-to-page schema))
  (heapfile.extend hf (pagedir.to-page (pagedir.init)))
  (heapfile.sync hf)
  (local w (when (and opts opts.wal)
             (let [wh (wal.open opts.wal)]
               (wal.checkpoint wh)
               wh)))
  (load hf (make-bufmgr (when opts opts.pool-size) w) w))

(fn file-exists? [path]
  (let [f (io.open path :r)]
    (if f (do
            (f:close) true) false)))

(fn open* [path schema opts]
  "Open a table, creating it from schema when the file is absent.
When opts.wal is provided, runs ARIES 3-phase recovery and rebuilds the
page directory from recovered data pages before loading."
  (local existed (file-exists? path))
  (if existed
      (let [hf (heapfile.open path)
            w (when (and opts opts.wal)
                (local stats (wal.recover opts.wal hf opts))
                (when (or (> stats.redone 0) (> stats.undone 0))
                  (rebuild-dir hf))
                (wal.open opts.wal))]
        (load hf (make-bufmgr (when opts opts.pool-size) w) w))
      (create path schema opts)))

;; -- transactions & Strict 2PL --------------------------------------------

(fn begin-txn [tbl]
  "Start a new WAL-backed transaction; returns its txn-id.
Supports multiple concurrent transactions when passing `txn` explicitly to
insert/read/delete/commit-txn/abort-txn."
  (when (= tbl.wal nil) (error "table: transactions require WAL"))
  (local (txn) (wal.begin tbl.wal))
  (tset tbl.active-txns txn true)
  (when (= tbl.active-txn nil) (set tbl.active-txn txn))
  txn)

(fn resolve-txn [tbl txn]
  (or txn tbl.active-txn))

(fn commit-txn [tbl txn]
  "Commit `txn` (or the default active txn), fdatasync WAL, and release locks."
  (local id (resolve-txn tbl txn))
  (when (or (= id nil) (not (. tbl.active-txns id)))
    (error "table: no active transaction"))
  (wal.commit tbl.wal id)
  (tset tbl.active-txns id nil)
  (when (= tbl.active-txn id) (set tbl.active-txn nil))
  (lockmgr.release-all tbl.lm id))

(fn abort-txn [tbl txn]
  "Roll back `txn` online via ARIES CLRs, log ABORT, and release locks."
  (local id (resolve-txn tbl txn))
  (when (or (= id nil) (not (. tbl.active-txns id)))
    (error "table: no active transaction"))
  (wal.undo-txn-online tbl.wal id
                       (fn [page-id mutate-fn]
                         (local slot (bufmgr.get-page tbl.buf tbl.hf page-id))
                         (local next-data (mutate-fn slot.data))
                         (set slot.data next-data)
                         (bufmgr.release tbl.buf slot true)
                         (when (>= page-id FIRST-DATA-PAGE)
                           (when (pagedir.find-by-page tbl.dir page-id)
                             (pagedir.update-free tbl.dir page-id
                                                  (page.free-space next-data))))))
  (tset tbl.active-txns id nil)
  (when (= tbl.active-txn id) (set tbl.active-txn nil))
  (lockmgr.release-all tbl.lm id))

(fn with-txn [tbl explicit-txn f]
  "Execute f(txn-id, auto?) under explicit-txn / default active-txn, or an
auto-committed single-statement WAL transaction when WAL is enabled."
  (local existing (resolve-txn tbl explicit-txn))
  (if existing
      (f existing false)
      (if (= tbl.wal nil)
          (f nil false)
          (let [(txn) (wal.begin tbl.wal)
                (ok res) (pcall f txn true)]
            (if ok
                (do
                  (wal.commit tbl.wal txn)
                  (lockmgr.release-all tbl.lm txn)
                  res)
                (do
                  (wal.undo-txn-online tbl.wal txn
                                       (fn [page-id mutate-fn]
                                         (local slot
                                                (bufmgr.get-page tbl.buf tbl.hf
                                                                 page-id))
                                         (set slot.data (mutate-fn slot.data))
                                         (bufmgr.release tbl.buf slot true)))
                  (lockmgr.release-all tbl.lm txn)
                  (error res)))))))

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

(fn insert [tbl record txn]
  "Insert packed record bytes; return the TupleId {page slot}."
  (with-txn tbl
    txn
    (fn [tx]
      (local page-id (or (pagedir.find-free tbl.dir (required-space record))
                         (let [id (heapfile.extend tbl.hf (page.page-init))]
                           (pagedir.add-entry tbl.dir id
                                              (page.free-space (page.page-init)))
                           id)))
      ;; Respect any active page-level lock (e.g., compaction on this page).
      (when tx (lockmgr.acquire tbl.lm tx (page-key page-id) :shared))
      (local slot (bufmgr.get-page tbl.buf tbl.hf page-id))
      (local skip?
             (when tx
               (fn [n]
                 (lockmgr.locked-by-other? tbl.lm tx
                                           (tid-key {:page page-id :slot n})))))
      (local (data n) (page.add-item slot.data record skip?))
      (local tid {:page page-id :slot n})
      (when tx (lockmgr.acquire tbl.lm tx (tid-key tid) :exclusive))
      (local stamped (if tx
                         (page.set-lsn data
                                       (wal.log-page-add tbl.wal tx page-id n
                                                         record))
                         data))
      (set slot.data stamped)
      (bufmgr.release tbl.buf slot true)
      (pagedir.update-free tbl.dir page-id (page.free-space stamped))
      tid)))

(fn read [tbl tid txn]
  "Read record bytes by TupleId; nil when the slot is dead."
  (local tx (resolve-txn tbl txn))
  (when tx (lockmgr.acquire tbl.lm tx (tid-key tid) :shared))
  (local slot (bufmgr.get-page tbl.buf tbl.hf tid.page))
  (local bytes (page.get-item slot.data tid.slot))
  (bufmgr.release tbl.buf slot false)
  bytes)

(fn delete [tbl tid txn]
  "Tombstone the record at TupleId."
  (with-txn tbl
    txn
    (fn [tx]
      (when tx
        (lockmgr.acquire tbl.lm tx (page-key tid.page) :shared)
        (lockmgr.acquire tbl.lm tx (tid-key tid) :exclusive))
      (local slot (bufmgr.get-page tbl.buf tbl.hf tid.page))
      (local old-bytes (page.get-item slot.data tid.slot))
      (local new-page (page.delete-item slot.data tid.slot))
      (local stamped (if (and tx old-bytes)
                         (page.set-lsn new-page
                                       (wal.log-page-del tbl.wal tx tid.page
                                                         tid.slot old-bytes))
                         new-page))
      (set slot.data stamped)
      (bufmgr.release tbl.buf slot true))))

(fn compact-page [tbl page-id txn]
  "Defragment one page; reclaims tombstoned record bytes."
  (with-txn tbl
    txn
    (fn [tx]
      (when tx (lockmgr.acquire tbl.lm tx (page-key page-id) :exclusive))
      (local slot (bufmgr.get-page tbl.buf tbl.hf page-id))
      (local old-page slot.data)
      (local new-page (page.compact old-page))
      (local stamped (if tx
                         (page.set-lsn new-page
                                       (wal.append tbl.wal tx page-id 0
                                                   old-page new-page))
                         new-page))
      (set slot.data stamped)
      (bufmgr.release tbl.buf slot true)
      (pagedir.update-free tbl.dir page-id (page.free-space stamped)))))

(fn scan [tbl]
  "Iterate every live record as tid, bytes across all data pages."
  (coroutine.wrap (fn []
                    (for [page-id FIRST-DATA-PAGE (- (heapfile.num-pages tbl.hf)
                                                     1)]
                      (local slot (bufmgr.get-page tbl.buf tbl.hf page-id))
                      (local data slot.data)
                      (bufmgr.release tbl.buf slot false)
                      (page.each-item data
                                      (fn [n bytes]
                                        (coroutine.yield {:page page-id
                                                          :slot n}
                                                         bytes)))))))

(fn count [tbl]
  "Number of live records."
  (var n 0)
  (each [_ _ (scan tbl)] (set n (+ n 1)))
  n)

(fn flush [tbl durable?]
  "Persist the directory and all dirty pages (fsyncing when durable? is true)."
  (write-dir tbl)
  (bufmgr.flush-all tbl.buf tbl.hf durable?))

(fn checkpoint [tbl]
  "Flush and fsync all dirty data pages BEFORE writing a WAL checkpoint."
  (flush tbl true)
  (when tbl.wal (wal.checkpoint tbl.wal)))

(fn lock [tbl mode]
  "Acquire/release a POSIX flock on the underlying table file."
  (heapfile.lock tbl.hf mode))

(fn close* [tbl]
  "Flush, fsync data pages, write WAL checkpoint when enabled, and close."
  (flush tbl (not= tbl.wal nil))
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

{: CATALOG-PAGE
 : DIR-PAGE
 : FIRST-DATA-PAGE
 : create
 :open open*
 : load
 : insert
 : read
 : delete
 : compact-page
 : scan
 : count
 : flush
 : checkpoint
 : lock
 :close close*
 : field
 : write-dir
 : begin-txn
 : commit-txn
 : abort-txn}
