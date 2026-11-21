;;; umbel.bufmgr — buffer pool with clock-sweep eviction.
;;
;; A fixed pool of buffer slots caches heap-file pages. Callers pin a
;; page with get-page, mutate the BLKSZ-byte string, and release it with
;; dirty=true to have it written back on eviction or flush. The clock
;; hand sweeps slots with pin-count 0, decrementing usage counters until
;; it finds a cold victim — the classic clock-sweep of PostgreSQL's
;; bufmgr, minus the shared-memory fireworks.

(local heapfile (require :umbel.heapfile))
(local hf-read heapfile.read-page)
(local hf-write heapfile.write-page)

(fn new [pool-size opts]
  "Create a buffer pool with pool-size slots.
Optional opts.before-write(page-id, data) runs before any dirty write/eviction
so the WAL rule (flushed_lsn >= page_lsn) is enforced."
  (local slots [])
  (for [i 1 pool-size]
    (table.insert slots {:valid false
                         :page-id -1
                         :data ""
                         :dirty false
                         :pin 0
                         :usage 0}))
  {: pool-size :hand 0 : slots :before-write (when opts opts.before-write)})

(fn find-slot [buf page-id]
  (var found nil)
  (each [_ s (ipairs buf.slots)]
    (when (and (= found nil) s.valid (= s.page-id page-id))
      (set found s)))
  found)

(local MAX-USAGE 5)

(fn victim [buf]
  "Advance the clock hand to an unpinned, cold slot; return it."
  (local n (length buf.slots))
  (var found nil)
  (var sweeps 0)
  (while (and (= found nil) (< sweeps (* (+ MAX-USAGE 1) n)))
    (let [s (. buf.slots (+ buf.hand 1))]
      (when (= s.pin 0)
        (if (> s.usage 0)
            (set s.usage (- s.usage 1))
            (set found s)))
      (set buf.hand (% (+ buf.hand 1) n))
      (set sweeps (+ sweeps 1))))
  (or found (error "bufmgr: all buffers are pinned")))

(fn get-page [buf hf page-id]
  "Return a pinned buffer slot holding page-id of heap file hf."
  (let [s (find-slot buf page-id)]
    (if s
        (do
          (set s.pin (+ s.pin 1))
          (when (< s.usage MAX-USAGE) (set s.usage (+ s.usage 1)))
          s)
        (let [v (victim buf)]
          (when (and v.valid v.dirty)
            (when buf.before-write (buf.before-write v.page-id v.data))
            (hf-write hf v.page-id v.data))
          (set v.valid true)
          (set v.page-id page-id)
          (set v.data (hf-read hf page-id))
          (set v.dirty false)
          (set v.pin 1)
          (set v.usage 1)
          v))))

(fn release [buf slot dirty]
  "Unpin a slot; mark it dirty when it was modified."
  (when (< slot.pin 1) (error "bufmgr: release of unpinned slot"))
  (set slot.pin (- slot.pin 1))
  (when dirty (set slot.dirty true)))

(fn flush-slot [buf hf slot]
  (when (and slot.valid slot.dirty)
    (when buf.before-write (buf.before-write slot.page-id slot.data))
    (hf-write hf slot.page-id slot.data)
    (set slot.dirty false)))

(fn flush-all [buf hf durable?]
  "Write every dirty slot back to the heap file; fsync when durable? is true."
  (each [_ s (ipairs buf.slots)] (flush-slot buf hf s))
  (when durable? (heapfile.sync hf)))

(fn pinned-count [buf]
  (var n 0)
  (each [_ s (ipairs buf.slots)]
    (when (> s.pin 0) (set n (+ n 1))))
  n)

(fn dirty-count [buf]
  (var n 0)
  (each [_ s (ipairs buf.slots)]
    (when s.dirty (set n (+ n 1))))
  n)

(fn resident-ids [buf]
  (icollect [_ s (ipairs buf.slots)]
    (when s.valid s.page-id)))

{: new
 : get-page
 : release
 : flush-slot
 : flush-all
 : pinned-count
 : dirty-count
 : resident-ids}
