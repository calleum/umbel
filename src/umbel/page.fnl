;;; umbel.page — slotted page manager.
;;
;; A page is an immutable BLKSZ-byte Lua string. The layout mirrors the
;; PostgreSQL/SQLite-inspired design of the original C engine:
;;
;;   bytes 1..4   lower (u32 LE) — end of the slot array, start of free space
;;   bytes 5..8   upper (u32 LE) — start of item data (grows downward)
;;   bytes 9..    slot array: { offset u32, length u32 } pairs, grows upward
;;   bytes BLKSZ.. item data, grows downward from the end of the page
;;
;; All item numbers passed across the API are 1-based, as in the C original.
;; Slots with length 0 are tombstones left by delete-item; compact reclaims
;; them. Because Lua strings are immutable, every mutation returns a new page.

(local BLKSZ 4096)
(local HEADER-SIZE 8)
(local SLOT-SIZE 8)

(local pack string.pack)
(local unpack string.unpack)

(fn page-init []
  "Return a fresh empty page."
  (.. (pack "<I4I4" HEADER-SIZE BLKSZ) (string.rep "\0" (- BLKSZ HEADER-SIZE))))

(fn header [page]
  "Return lower and upper offsets of a page."
  (unpack "<I4I4" page 1))

(fn set-header [page lower upper]
  "Return page with its header rewritten."
  (.. (pack "<I4I4" lower upper) (page:sub (+ HEADER-SIZE 1))))

(fn slot-count [page]
  "Number of slots currently allocated in the slot array."
  (let [(lower) (header page)]
    (// (- lower HEADER-SIZE) SLOT-SIZE)))

(fn slot-at [page i]
  "Read slot i (0-based) as offset, length."
  (unpack "<I4I4" page (+ HEADER-SIZE (* i SLOT-SIZE) 1)))

(fn set-slot-at [page i offset item-len]
  "Return page with slot i (0-based) rewritten."
  (let [pos (+ HEADER-SIZE (* i SLOT-SIZE) 1)]
    (.. (page:sub 1 (- pos 1))
        (pack "<I4I4" offset item-len)
        (page:sub (+ pos SLOT-SIZE)))))

(fn free-space [page]
  "Bytes of usable free space (gap between the slot array and item data)."
  (let [(lower upper) (header page)]
    (- upper lower)))

(fn live-count [page]
  "Number of non-deleted items."
  (var n 0)
  (for [i 0 (- (slot-count page) 1)]
    (let [(_ len) (slot-at page i)]
      (when (> len 0) (set n (+ n 1)))))
  n)

(fn find-tombstone [page]
  "Return the 0-based index of the first tombstone slot, or nil."
  (var found nil)
  (for [i 0 (- (slot-count page) 1)]
    (when (and (= found nil) (= (select 2 (slot-at page i)) 0))
      (set found i)))
  found)

(fn add-item [page bytes]
  "Add bytes to the page; return new page and the 1-based item number.
Reuses a tombstone slot when one exists, otherwise appends a fresh slot."
  (let [len (length bytes)]
    (when (= len 0) (error "cannot add an empty item"))
    (let [(lower upper) (header page)
          tomb (find-tombstone page)]
      (when (if tomb
                (< (- upper len) lower)
                (< (- upper len) (+ lower SLOT-SIZE)))
        (error "page full: not enough free space"))
      (let [new-upper (- upper len)
            slot-idx (or tomb (slot-count page))
            new-lower (if tomb lower (+ lower SLOT-SIZE))
            slot-pos (+ HEADER-SIZE (* slot-idx SLOT-SIZE) 1)]
        (values (.. (pack "<I4I4" new-lower new-upper)
                    (page:sub (+ HEADER-SIZE 1) (- slot-pos 1))
                    (pack "<I4I4" new-upper len)
                    (page:sub (+ slot-pos SLOT-SIZE) new-upper)
                    bytes
                    (page:sub (+ upper 1)))
                (+ slot-idx 1))))))

(fn get-item [page n]
  "Return the bytes of item n (1-based), or nil when absent or deleted."
  (let [cnt (slot-count page)]
    (when (and (>= n 1) (<= n cnt))
      (let [(off len) (slot-at page (- n 1))]
        (when (> len 0)
          (page:sub (+ off 1) (+ off len)))))))

(fn delete-item [page n]
  "Mark item n (1-based) deleted; return the new page."
  (when (or (< n 1) (> n (slot-count page)))
    (error (.. "delete-item: no such item " n)))
  (let [(off) (slot-at page (- n 1))]
    (set-slot-at page (- n 1) off 0)))

(fn compact [page]
  "Defragment item data while keeping every slot in place, PG-style.
Live items are repacked contiguously at the top of the page with new
offsets; tombstone slots remain holes in the slot array. Item numbers
are preserved, and dead bytes become reusable free space."
  (let [cnt (slot-count page)
        (lower) (header page)]
    (var live [])  ;; [{:i slot-idx :len n :bytes s}] in slot order
    (for [i 0 (- cnt 1)]
      (let [(off len) (slot-at page i)]
        (when (> len 0)
          (table.insert live {:i i :len len :bytes (page:sub (+ off 1) (+ off len))}))))
    (var total 0)
    (each [_ it (ipairs live)] (set total (+ total it.len)))
    (let [new-upper (- BLKSZ total)
          offsets {}]
      (var cursor new-upper)
      (each [_ it (ipairs live)]
        (tset offsets it.i [cursor it.len])
        (set cursor (+ cursor it.len)))
      (var slots "")
      (for [i 0 (- cnt 1)]
        (let [pair (or (. offsets i) [0 0])]
          (set slots (.. slots (pack "<I4I4" (. pair 1) (. pair 2))))))
      (.. (pack "<I4I4" lower new-upper)
          slots
          (string.rep "\0" (- new-upper (+ HEADER-SIZE (length slots))))
          (table.concat (icollect [_ it (ipairs live)] it.bytes))))))

(fn each-item [page f]
  "Call f with each live item as item-number and bytes, in slot order."
  (for [i 0 (- (slot-count page) 1)]
    (let [(off len) (slot-at page i)]
      (when (> len 0)
        (f (+ i 1) (page:sub (+ off 1) (+ off len)))))))

{:BLKSZ BLKSZ :HEADER-SIZE HEADER-SIZE :SLOT-SIZE SLOT-SIZE
 :page-init page-init :header header :set-header set-header
 :slot-count slot-count :slot-at slot-at :set-slot-at set-slot-at
 :free-space free-space :live-count live-count :add-item add-item
 :get-item get-item :delete-item delete-item :compact compact
 :each-item each-item}
