;;; umbel.wal — write-ahead log and crash recovery (roadmap tasks 5.1 & 5.2).
;;
;; Append-only binary log of page mutations. Every frame is length-prefixed
;; and checksummed (FNV-1a 32-bit) so a torn write at the tail after a crash
;; is detected and ignored cleanly:
;;
;;   frame:   "UW" | payload-len u32 | fnv1a u32 | payload
;;   payload: kind u8 | txn-id u64 | page-id u32 | offset u32
;;            | old-len u32 | new-len u32 | old-data | new-data
;;
;; Kinds: 1=begin 2=write 3=commit 4=abort 5=checkpoint.
;;
;; ponytail: stdio flush only (no C fsync binding) and non-interleaved
;; single-threaded transactions; add fsync + ARIES LSN/CLR headers if
;; power-loss or concurrent transactions are ever needed.

(local page (require :umbel.page))
(local heapfile (require :umbel.heapfile))

(local pack string.pack)
(local unpack string.unpack)

(local MAGIC "UW")
(local FRAME-HDR 10)   ; 2 magic + 4 len + 4 checksum
(local PAYLOAD-HDR 25) ; 1 kind + 8 txn + 4 page + 4 off + 4 old + 4 new

(local KIND {:begin 1 :write 2 :commit 3 :abort 4 :checkpoint 5})
(local KIND-NAME {})
(each [k v (pairs KIND)] (tset KIND-NAME v k))

(fn fnv1a [s]
  "32-bit FNV-1a checksum over string s."
  (var h 0x811c9dc5)
  (for [i 1 (length s)]
    (set h (band (* (bxor h (string.byte s i)) 0x01000193) 0xffffffff)))
  h)

(fn encode-frame [kind txn-id page-id offset old-data new-data]
  (local payload
    (.. (pack "<BI8I4I4I4I4"
              kind txn-id (or page-id 0) (or offset 0)
              (length (or old-data "")) (length (or new-data "")))
        (or old-data "")
        (or new-data "")))
  (.. MAGIC (pack "<I4I4" (length payload) (fnv1a payload)) payload))

(fn read-records [path]
  "Read all complete, checksum-valid WAL records from path.
Stops cleanly on EOF or a torn tail frame."
  (local f (io.open path "rb"))
  (if (= f nil)
      []
      (let [data (or (f:read "*a") "")
            total (length data)
            out []]
        (f:close)
        (var pos 1)
        (var ok true)
        (while (and ok (<= (+ pos FRAME-HDR -1) total))
          (if (not= (data:sub pos (+ pos 1)) MAGIC)
              (set ok false)
              (let [(plen csum) (unpack "<I4I4" data (+ pos 2))
                    pstart (+ pos FRAME-HDR)
                    pend (+ pstart plen -1)]
                (if (or (< plen PAYLOAD-HDR) (> pend total))
                    (set ok false)
                    (let [payload (data:sub pstart pend)]
                      (if (not= (fnv1a payload) csum)
                          (set ok false)
                          (let [(k txn pg off olen nlen)
                                (unpack "<BI8I4I4I4I4" payload 1)
                                kind (. KIND-NAME k)]
                            (if (or (= kind nil)
                                    (not= (+ PAYLOAD-HDR olen nlen) plen))
                                (set ok false)
                                (let [odata (payload:sub (+ PAYLOAD-HDR 1)
                                                         (+ PAYLOAD-HDR olen))
                                      ndata (payload:sub (+ PAYLOAD-HDR olen 1)
                                                         (+ PAYLOAD-HDR olen nlen))]
                                  (table.insert out
                                                {:kind kind :txn-id txn
                                                 :page-id pg :offset off
                                                 :old-data odata :new-data ndata})
                                  (set pos (+ pend 1)))))))))))
        out)))

(fn open [path]
  "Open (or create) an append-only WAL at path."
  (var max-txn 0)
  (each [_ r (ipairs (read-records path))]
    (when (> r.txn-id max-txn) (set max-txn r.txn-id)))
  (local f (or (io.open path "r+b") (io.open path "w+b")))
  (when (= f nil) (error (.. "wal: cannot open " path)))
  (f:seek "end")
  {:file f :path path :next-txn (+ max-txn 1)})

(fn write-frame [w frame]
  (w.file:seek "end")
  (w.file:write frame))

(fn flush [w]
  (w.file:flush))

(fn begin [w]
  "Start a new transaction and log its BEGIN record; return txn-id."
  (local txn w.next-txn)
  (set w.next-txn (+ w.next-txn 1))
  (write-frame w (encode-frame KIND.begin txn 0 0 "" ""))
  txn)

(fn append [w txn-id page-id offset old-data new-data]
  "Log a page modification before touching the data page."
  (write-frame w (encode-frame KIND.write txn-id page-id offset old-data new-data)))

(fn commit [w txn-id]
  "Log COMMIT and force the WAL to disk."
  (write-frame w (encode-frame KIND.commit txn-id 0 0 "" ""))
  (flush w))

(fn abort [w txn-id]
  "Log ABORT and force the WAL to disk."
  (write-frame w (encode-frame KIND.abort txn-id 0 0 "" ""))
  (flush w))

(fn checkpoint [w]
  "Log a CHECKPOINT marking all prior committed pages flushed to disk."
  (write-frame w (encode-frame KIND.checkpoint 0 0 0 "" ""))
  (flush w))

(fn close [w]
  (flush w)
  (w.file:close))

(fn patch-page [data offset patch]
  "Overwrite bytes in page string `data` at 0-based `offset` with `patch`."
  (local plen (length patch))
  (when (> (+ offset plen) (length data))
    (error "wal: patch exceeds page size"))
  (.. (data:sub 1 offset)
      patch
      (data:sub (+ offset plen 1))))

(fn ensure-page [hf page-id]
  (while (<= (heapfile.num-pages hf) page-id)
    (heapfile.extend hf (page.page-init))))

(fn recover [wal-path hf]
  "Replay WAL from the last checkpoint onto heapfile `hf`.
Redoes every committed transaction forward, then undoes uncommitted
transactions backward. Returns recovery counts."
  (local records (read-records wal-path))
  (var last-ckpt 0)
  (each [i r (ipairs records)]
    (when (= r.kind :checkpoint) (set last-ckpt i)))
  (local active [])
  (for [i (+ last-ckpt 1) (length records)]
    (table.insert active (. records i)))
  (local committed {})
  (local all-txns {})
  (each [_ r (ipairs active)]
    (when (> r.txn-id 0) (tset all-txns r.txn-id true))
    (when (= r.kind :commit) (tset committed r.txn-id true)))
  ;; REDO committed writes in forward order
  (var redone 0)
  (each [_ r (ipairs active)]
    (when (and (= r.kind :write) (. committed r.txn-id))
      (ensure-page hf r.page-id)
      (local cur (heapfile.read-page hf r.page-id))
      (heapfile.write-page hf r.page-id (patch-page cur r.offset r.new-data))
      (set redone (+ redone 1))))
  ;; UNDO uncommitted writes in reverse order
  (var undone 0)
  (for [i (length active) 1 -1]
    (let [r (. active i)]
      (when (and (= r.kind :write)
                 (not (. committed r.txn-id))
                 (< r.page-id (heapfile.num-pages hf))
                 (> (length r.old-data) 0))
        (local cur (heapfile.read-page hf r.page-id))
        (heapfile.write-page hf r.page-id (patch-page cur r.offset r.old-data))
        (set undone (+ undone 1)))))
  (var c-cnt 0)
  (var u-cnt 0)
  (each [tid _ (pairs all-txns)]
    (if (. committed tid)
        (set c-cnt (+ c-cnt 1))
        (set u-cnt (+ u-cnt 1))))
  {:redone redone :undone undone
   :committed-txns c-cnt :uncommitted-txns u-cnt})

{:KIND KIND :open open :begin begin :append append :commit commit :abort abort
 :checkpoint checkpoint :flush flush :close close :read-records read-records
 :recover recover}
