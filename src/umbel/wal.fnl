;;; umbel.wal — ARIES-style write-ahead log and crash recovery.
;;
;; Append-only, checksummed (FNV-1a 32-bit), fdatasync-backed WAL supporting:
;;   - Monotonic LSNs, per-txn prev_lsn undo chains, and CLR undo_next_lsn
;;   - Physiological slotted-page ops (:page-add / :page-del) so concurrent
;;     transactions on the SAME page can interleave, commit, and undo cleanly
;;   - Physical :write records for catalog/dir/compaction
;;   - Fuzzy checkpoints recording active transactions at checkpoint time
;;   - 3-phase ARIES recovery (Analysis -> Redo repeating history with pageLSN
;;     idempotence -> Undo writing CLRs, safe across crashes during recovery)
;;
;; Frame layout (LE):
;;   "UW" | payload-len u32 | fnv1a u32 | payload
;; Payload header (50 bytes):
;;   kind u8 | lsn u64 | prev_lsn u64 | undo_next_lsn u64 | txn_id u64
;;   | page_id u32 | offset u32 | clr_op u8 | old_len u32 | new_len u32
;;   | old_data | new_data

(local page (require :umbel.page))
(local heapfile (require :umbel.heapfile))
(local sys (require :umbel.sys))

(local pack string.pack)
(local unpack string.unpack)

(local MAGIC :UW)
(local FRAME-HDR 10)
(local PAYLOAD-FMT :<BI8I8I8I8I4I4BI4I4)
(local PAYLOAD-HDR (string.packsize PAYLOAD-FMT))

(local KIND {:begin 1
             :write 2
             :page-add 3
             :page-del 4
             :clr 5
             :commit 6
             :abort 7
             :checkpoint 8})

(local KIND-NAME {})
(each [k v (pairs KIND)] (tset KIND-NAME v k))

(local CLR-OP {:physical 0 :undo-add 1 :undo-del 2})

(fn fnv1a [s]
  "32-bit FNV-1a checksum over string s."
  (var h 2166136261)
  (for [i 1 (length s)]
    (set h (band (* (bxor h (string.byte s i)) 16777619) 4294967295)))
  h)

(fn encode-frame [kind
                  lsn
                  prev-lsn
                  undo-next
                  txn-id
                  page-id
                  offset
                  clr-op
                  old-data
                  new-data]
  (local odata (or old-data ""))
  (local ndata (or new-data ""))
  (local payload (.. (pack PAYLOAD-FMT kind lsn (or prev-lsn 0)
                           (or undo-next 0) txn-id (or page-id 0) (or offset 0)
                           (or clr-op 0) (length odata) (length ndata))
                     odata ndata))
  (.. MAGIC (pack :<I4I4 (length payload) (fnv1a payload)) payload))

(fn read-records [path]
  "Read all complete, checksum-valid WAL records from path.
Stops cleanly on EOF or a torn tail frame."
  (local f (io.open path :rb))
  (if (= f nil)
      []
      (let [data (or (f:read :*a) "")
            total (length data)
            out []]
        (f:close)
        (var pos 1)
        (var ok true)
        (while (and ok (<= (+ pos FRAME-HDR -1) total))
          (if (not= (data:sub pos (+ pos 1)) MAGIC)
              (set ok false)
              (let [(plen csum) (unpack :<I4I4 data (+ pos 2))
                    pstart (+ pos FRAME-HDR)
                    pend (+ pstart plen -1)]
                (if (or (< plen PAYLOAD-HDR) (> pend total))
                    (set ok false)
                    (let [payload (data:sub pstart pend)]
                      (if (not= (fnv1a payload) csum)
                          (set ok false)
                          (let [(k lsn prev undo-next txn pg off clr-op olen
                                   nlen) (unpack PAYLOAD-FMT
                                                                                 payload
                                                                                 1)
                                kind (. KIND-NAME k)]
                            (if (or (= kind nil)
                                    (not= (+ PAYLOAD-HDR olen nlen) plen))
                                (set ok false)
                                (let [odata (payload:sub (+ PAYLOAD-HDR 1)
                                                         (+ PAYLOAD-HDR olen))
                                      ndata (payload:sub (+ PAYLOAD-HDR olen 1)
                                                         (+ PAYLOAD-HDR olen
                                                            nlen))]
                                  (table.insert out
                                                {: kind
                                                 : lsn
                                                 :prev-lsn prev
                                                 :undo-next-lsn undo-next
                                                 :txn-id txn
                                                 :page-id pg
                                                 :offset off
                                                 : clr-op
                                                 :old-data odata
                                                 :new-data ndata})
                                  (set pos (+ pend 1)))))))))))
        out)))

(fn encode-ckpt-active [active-map]
  (local parts [])
  (var cnt 0)
  (each [tid lsn (pairs active-map)]
    (set cnt (+ cnt 1))
    (table.insert parts (pack :<I8I8 tid lsn)))
  (.. (pack :<I4 cnt) (table.concat parts)))

(fn decode-ckpt-active [buf]
  (local out {})
  (when (>= (length buf) 4)
    (let [(cnt) (unpack :<I4 buf 1)]
      (var pos 5)
      (for [_ 1 cnt]
        (let [(tid lsn next) (unpack :<I8I8 buf pos)]
          (tset out tid lsn)
          (set pos next)))))
  out)

(fn open [path]
  "Open (or create) an append-only fdatasync-backed WAL at path."
  (var max-txn 0)
  (var max-lsn 0)
  (each [_ r (ipairs (read-records path))]
    (when (> r.txn-id max-txn) (set max-txn r.txn-id))
    (when (> r.lsn max-lsn) (set max-lsn r.lsn)))
  (local existing (io.open path :r+b))
  (local f (if existing
               existing
               (let [nf (io.open path :w+b)]
                 (when (= nf nil) (error (.. "wal: cannot open " path)))
                 (sys.fsync nf)
                 (sys.sync-dir path)
                 nf)))
  (f:seek :end)
  {:file f
   : path
   :next-txn (+ max-txn 1)
   :next-lsn (+ max-lsn 1)
   :flushed-lsn max-lsn
   :txn-last-lsn {}
   :txn-records {}})

(fn write-frame [w frame]
  (w.file:seek :end)
  (w.file:write frame))

(fn flush [w]
  "Force all buffered WAL frames to non-volatile storage via fdatasync."
  (sys.fdatasync w.file)
  (set w.flushed-lsn (- w.next-lsn 1)))

(fn ensure-flushed [w lsn]
  "Enforce the WAL rule: guarantee records up to `lsn` are durable on disk."
  (when (> lsn w.flushed-lsn)
    (flush w)))

(fn alloc-lsn [w]
  (local lsn w.next-lsn)
  (set w.next-lsn (+ w.next-lsn 1))
  lsn)

(fn record-in-txn [w rec]
  (tset w.txn-last-lsn rec.txn-id rec.lsn)
  (when (= (. w.txn-records rec.txn-id) nil)
    (tset w.txn-records rec.txn-id []))
  (table.insert (. w.txn-records rec.txn-id) rec))

(fn begin [w]
  "Start a new transaction and log its BEGIN record; return (txn-id, lsn)."
  (local txn w.next-txn)
  (set w.next-txn (+ w.next-txn 1))
  (local lsn (alloc-lsn w))
  (write-frame w (encode-frame KIND.begin lsn 0 0 txn 0 0 0 "" ""))
  (record-in-txn w {:kind :begin : lsn :prev-lsn 0 :txn-id txn})
  (values txn lsn))

(fn append [w txn-id page-id offset old-data new-data]
  "Log a physical page modification; return its LSN."
  (local lsn (alloc-lsn w))
  (local prev (or (. w.txn-last-lsn txn-id) 0))
  (write-frame w (encode-frame KIND.write lsn prev 0 txn-id page-id offset 0
                               old-data new-data))
  (record-in-txn w {:kind :write
                    : lsn
                    :prev-lsn prev
                    : txn-id
                    : page-id
                    : offset
                    : old-data
                    : new-data})
  lsn)

(fn log-page-add [w txn-id page-id slot bytes]
  "Log a physiological slotted-page insert; return its LSN."
  (local lsn (alloc-lsn w))
  (local prev (or (. w.txn-last-lsn txn-id) 0))
  (write-frame w (encode-frame KIND.page-add lsn prev 0 txn-id page-id slot 0
                               "" bytes))
  (record-in-txn w {:kind :page-add
                    : lsn
                    :prev-lsn prev
                    : txn-id
                    : page-id
                    :offset slot
                    :old-data ""
                    :new-data bytes})
  lsn)

(fn log-page-del [w txn-id page-id slot old-bytes]
  "Log a physiological slotted-page delete; return its LSN."
  (local lsn (alloc-lsn w))
  (local prev (or (. w.txn-last-lsn txn-id) 0))
  (write-frame w (encode-frame KIND.page-del lsn prev 0 txn-id page-id slot 0
                               old-bytes ""))
  (record-in-txn w {:kind :page-del
                    : lsn
                    :prev-lsn prev
                    : txn-id
                    : page-id
                    :offset slot
                    :old-data old-bytes
                    :new-data ""})
  lsn)

(fn log-clr [w txn-id undo-next-lsn page-id offset clr-op data]
  "Log a Compensation Log Record (CLR) during undo; return its LSN."
  (local lsn (alloc-lsn w))
  (local prev (or (. w.txn-last-lsn txn-id) 0))
  (write-frame w (encode-frame KIND.clr lsn prev undo-next-lsn txn-id page-id
                               offset clr-op "" (or data "")))
  (tset w.txn-last-lsn txn-id lsn)
  lsn)

(fn commit [w txn-id]
  "Log COMMIT and force the WAL to durable storage via fdatasync."
  (local lsn (alloc-lsn w))
  (local prev (or (. w.txn-last-lsn txn-id) 0))
  (write-frame w (encode-frame KIND.commit lsn prev 0 txn-id 0 0 0 "" ""))
  (tset w.txn-last-lsn txn-id nil)
  (tset w.txn-records txn-id nil)
  (flush w)
  lsn)

(fn abort [w txn-id]
  "Log ABORT and force the WAL to durable storage via fdatasync."
  (local lsn (alloc-lsn w))
  (local prev (or (. w.txn-last-lsn txn-id) 0))
  (write-frame w (encode-frame KIND.abort lsn prev 0 txn-id 0 0 0 "" ""))
  (tset w.txn-last-lsn txn-id nil)
  (tset w.txn-records txn-id nil)
  (flush w)
  lsn)

(fn checkpoint [w]
  "Log a CHECKPOINT (including any currently active txns) and fdatasync WAL."
  (local lsn (alloc-lsn w))
  (local active-bytes (encode-ckpt-active w.txn-last-lsn))
  (write-frame w (encode-frame KIND.checkpoint lsn 0 0 0 0 0 0 "" active-bytes))
  (flush w)
  lsn)

(fn close [w]
  (flush w)
  (w.file:close))

;; -- ARIES page application & 3-phase recovery ----------------------------

(fn patch-page [data offset patch]
  (local plen (length patch))
  (when (> (+ offset plen) (length data))
    (error "wal: patch exceeds page size"))
  (.. (data:sub 1 offset) patch (data:sub (+ offset plen 1))))

(fn ensure-page [hf page-id]
  (while (<= (heapfile.num-pages hf) page-id)
    (heapfile.extend hf (page.page-init))))

(fn apply-redo-page [cur rec]
  "Apply a single WAL record's redo effect onto page bytes `cur`."
  (local stamped (case rec.kind
                   :write (patch-page cur rec.offset rec.new-data)
                   :page-add (page.restore-slot cur rec.offset rec.new-data)
                   :page-del (if (page.get-item cur rec.offset)
                                 (page.delete-item cur rec.offset)
                                 cur)
                   :clr (case rec.clr-op
                          0 (patch-page cur rec.offset rec.new-data)
                          1 (if (page.get-item cur rec.offset)
                                (page.delete-item cur rec.offset)
                                cur)
                          2 (page.restore-slot cur rec.offset rec.new-data)
                          _ cur)
                   _ cur))
  (if (>= rec.page-id 2)
      (page.set-lsn stamped rec.lsn)
      stamped))

(fn undo-spec-for [rec]
  "Return (clr-op, restore-data) for undoing `rec`, or nil if not undoable."
  (case rec.kind
    :page-add (values CLR-OP.undo-add "")
    :page-del (values CLR-OP.undo-del rec.old-data)
    :write (when (> (length rec.old-data) 0)
             (values CLR-OP.physical rec.old-data))
    _ nil))

(fn apply-undo-page [cur rec clr-lsn]
  "Apply the inverse of `rec` onto page bytes `cur` and stamp `clr-lsn`."
  (local undone (case rec.kind
                  :page-add (if (page.get-item cur rec.offset)
                                (page.delete-item cur rec.offset)
                                cur)
                  :page-del (page.restore-slot cur rec.offset rec.old-data)
                  :write (patch-page cur rec.offset rec.old-data)
                  _ cur))
  (if (>= rec.page-id 2)
      (page.set-lsn undone clr-lsn)
      undone))

(fn undo-txn-online [w txn-id apply-page-fn]
  "Online rollback for an active transaction: walks its logged records in
reverse order, writes a CLR for each step, applies undo through the caller's
buffer pool via `apply-page-fn(page-id, fn(cur-bytes) -> new-bytes)`, and
logs ABORT."
  (local recs (or (. w.txn-records txn-id) []))
  (var undone 0)
  (for [i (length recs) 1 -1]
    (let [rec (. recs i)
          (clr-op clr-data) (undo-spec-for rec)]
      (when (not= clr-op nil)
        (local clr-lsn (log-clr w txn-id rec.prev-lsn rec.page-id rec.offset
                                clr-op clr-data))
        (apply-page-fn rec.page-id (fn [cur] (apply-undo-page cur rec clr-lsn)))
        (set undone (+ undone 1)))))
  (abort w txn-id)
  undone)

(fn recover [wal-path hf opts]
  "ARIES 3-phase crash recovery (Analysis -> Redo -> Undo with CLRs).
Safe to re-run after a crash during recovery.
Optional opts.crash-after-undo (integer): simulate crash after N undo CLRs."
  (local records (read-records wal-path))
  (local by-lsn {})
  (each [_ r (ipairs records)] (tset by-lsn r.lsn r))
  ;; Phase 1: Analysis
  (var last-ckpt 0)
  (each [i r (ipairs records)]
    (when (= r.kind :checkpoint) (set last-ckpt i)))
  (local active (if (> last-ckpt 0)
                    (decode-ckpt-active (. (. records last-ckpt) :new-data))
                    {}))
  (var redo-from-lsn (if (> last-ckpt 0)
                         (+ (. (. records last-ckpt) :lsn) 1)
                         1))
  ;; If active txns existed at checkpoint, walk back to their earliest LSN so
  ;; repeating history covers uncommitted pages dirtied before the checkpoint.
  (each [_ last-lsn (pairs active)]
    (var cur-lsn last-lsn)
    (while (> cur-lsn 0)
      (when (< cur-lsn redo-from-lsn) (set redo-from-lsn cur-lsn))
      (let [r (. by-lsn cur-lsn)]
        (set cur-lsn (if r r.prev-lsn 0)))))
  (for [i (+ last-ckpt 1) (length records)]
    (let [r (. records i)]
      (when (> r.txn-id 0)
        (if (or (= r.kind :commit) (= r.kind :abort))
            (tset active r.txn-id nil)
            (tset active r.txn-id r.lsn)))))
  ;; Phase 2: Redo (repeating history, skipping pages with pageLSN >= rec.lsn)
  (var redone 0)
  (each [_ r (ipairs records)]
    (when (and (>= r.lsn redo-from-lsn)
               (or (= r.kind :write) (= r.kind :page-add) (= r.kind :page-del)
                   (= r.kind :clr)))
      (ensure-page hf r.page-id)
      (local cur (heapfile.read-page hf r.page-id))
      (local skip? (and (>= r.page-id 2) (>= (page.get-lsn cur) r.lsn)))
      (when (not skip?)
        (heapfile.write-page hf r.page-id (apply-redo-page cur r))
        (set redone (+ redone 1)))))
  ;; Phase 3: Undo loser transactions (highest-LSN first, writing CLRs)
  (local to-undo {})
  (var losers 0)
  (each [tid last-lsn (pairs active)]
    (tset to-undo tid last-lsn)
    (set losers (+ losers 1)))
  (var undone 0)
  (var clrs 0)
  (when (> losers 0)
    (local w (open wal-path))
    ;; Seed txn-last-lsn so newly written CLRs chain properly.
    (each [tid last-lsn (pairs active)]
      (tset w.txn-last-lsn tid last-lsn))
    (local crash-limit (when opts opts.crash-after-undo))
    (var crashed false)
    (var running true)
    (while (and running (not crashed))
      ;; Pick the loser with the highest next-lsn.
      (var pick-tid nil)
      (var pick-lsn 0)
      (each [tid lsn (pairs to-undo)]
        (when (> lsn pick-lsn)
          (set pick-tid tid)
          (set pick-lsn lsn)))
      (if (= pick-tid nil)
          (set running false)
          (let [r (. by-lsn pick-lsn)]
            (if (= r nil)
                (do
                  (abort w pick-tid)
                  (tset to-undo pick-tid nil))
                (= r.kind :clr)
                ;; Already undone in a previous recovery pass: jump to undo-next-lsn!
                (if (= r.undo-next-lsn 0)
                    (do
                      (abort w pick-tid)
                      (tset to-undo pick-tid nil))
                    (tset to-undo pick-tid r.undo-next-lsn))
                (let [(clr-op clr-data) (undo-spec-for r)]
                  (when (not= clr-op nil)
                    (local clr-lsn
                           (log-clr w pick-tid r.prev-lsn r.page-id r.offset
                                    clr-op clr-data))
                    (flush w)
                    (ensure-page hf r.page-id)
                    (local cur (heapfile.read-page hf r.page-id))
                    (heapfile.write-page hf r.page-id
                                         (apply-undo-page cur r clr-lsn))
                    (set undone (+ undone 1))
                    (set clrs (+ clrs 1))
                    (when (and crash-limit (>= undone crash-limit))
                      (set crashed true)))
                  (when (not crashed)
                    (if (= r.prev-lsn 0)
                        (do
                          (abort w pick-tid)
                          (tset to-undo pick-tid nil))
                        (tset to-undo pick-tid r.prev-lsn))))))))
    (flush w)
    (close w))
  (when (or (> redone 0) (> undone 0))
    (heapfile.sync hf))
  {: redone : undone : clrs : losers})

{: KIND
 : CLR-OP
 : open
 : begin
 : append
 : log-page-add
 : log-page-del
 : log-clr
 : commit
 : abort
 : checkpoint
 : flush
 : ensure-flushed
 : close
 : read-records
 : undo-txn-online
 : recover}
