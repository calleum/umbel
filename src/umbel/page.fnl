;;; umbel.page — slotted page manager.
;;
;; A page is an immutable BLKSZ-byte Lua string. Layout (all LE):
;;
;;   bytes 1..4    lower (u32) — end of the slot array, start of free space
;;   bytes 5..8    upper (u32) — start of item data (grows downward)
;;   bytes 9..16   lsn   (u64) — ARIES pageLSN of the last WAL record applied
;;   bytes 17..    slot array: { offset u32, length u32 } pairs, grows upward
;;   bytes BLKSZ.. item data, grows downward from the end of the page
;;
;; All item numbers passed across the API are 1-based. Slots with length 0 are
;; tombstones left by delete-item; compact reclaims dead bytes while keeping
;; every slot number stable.

(local BLKSZ 4096)
(local HEADER-SIZE 16)
(local SLOT-SIZE 8)

(local pack string.pack)
(local unpack string.unpack)

(fn page-init []
  "Return a fresh empty page with pageLSN 0."
  (.. (pack "<I4I4I8" HEADER-SIZE BLKSZ 0)
      (string.rep "\0" (- BLKSZ HEADER-SIZE))))

(fn header [page]
  "Return lower and upper offsets of a page."
  (unpack "<I4I4" page 1))

(fn get-lsn [page]
  "Return the u64 pageLSN stored in the page header."
  (let [(lsn) (unpack "<I8" page 9)] lsn))

(fn set-lsn [page lsn]
  "Return page with its pageLSN updated."
  (.. (page:sub 1 8) (pack "<I8" lsn) (page:sub 17)))

(fn set-header [page lower upper]
  "Return page with lower and upper rewritten (preserving pageLSN)."
  (.. (pack "<I4I4" lower upper) (page:sub 9)))

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

(fn find-tombstone [page skip-slot?]
  "Return the 0-based index of the first reusable tombstone slot, or nil."
  (var found nil)
  (for [i 0 (- (slot-count page) 1)]
    (when (and (= found nil)
               (= (select 2 (slot-at page i)) 0)
               (not (and skip-slot? (skip-slot? (+ i 1)))))
      (set found i)))
  found)

(fn add-item [page bytes skip-slot?]
  "Add bytes to the page; return new page and the 1-based item number.
Reuses an unlocked tombstone slot when one exists, otherwise appends a fresh slot."
  (let [len (length bytes)]
    (when (= len 0) (error "cannot add an empty item"))
    (let [(lower upper) (header page)
          lsn (get-lsn page)
          tomb (find-tombstone page skip-slot?)]
      (when (if tomb
                (< (- upper len) lower)
                (< (- upper len) (+ lower SLOT-SIZE)))
        (error "page full: not enough free space"))
      (let [new-upper (- upper len)
            slot-idx (or tomb (slot-count page))
            new-lower (if tomb lower (+ lower SLOT-SIZE))
            slot-pos (+ HEADER-SIZE (* slot-idx SLOT-SIZE) 1)]
        (values (.. (pack "<I4I4I8" new-lower new-upper lsn)
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
and pageLSN are preserved, and dead bytes become reusable free space."
  (let [cnt (slot-count page)
        (lower) (header page)
        lsn (get-lsn page)]
    (var live [])
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
      (.. (pack "<I4I4I8" lower new-upper lsn)
          slots
          (string.rep "\0" (- new-upper (+ HEADER-SIZE (length slots))))
          (table.concat (icollect [_ it (ipairs live)] it.bytes))))))

(fn restore-slot [page n bytes]
  "Physiological redo/undo helper: place `bytes` into exact 1-based slot `n`.
Extends the slot array with tombstones if needed and compacts if dead space
must be reclaimed to fit `bytes`."
  (when (= (get-item page n) bytes)
    (lua "return page"))
  (local len (length bytes))
  (when (= len 0) (error "restore-slot: cannot restore empty item"))
  (var p page)
  (let [cnt (slot-count p)
        (lower upper) (header p)
        need-slots (if (> n cnt) (* (- n cnt) SLOT-SIZE) 0)]
    (when (< (- upper len) (+ lower need-slots))
      (set p (compact p))))
  (let [cnt (slot-count p)
        (lower upper) (header p)
        lsn (get-lsn p)
        new-cnt (if (> n cnt) n cnt)
        new-lower (+ HEADER-SIZE (* new-cnt SLOT-SIZE))
        new-upper (- upper len)]
    (when (< new-upper new-lower)
      (error "restore-slot: page full"))
    (local slots-parts [])
    (for [i 0 (- new-cnt 1)]
      (if (= (+ i 1) n)
          (table.insert slots-parts (pack "<I4I4" new-upper len))
          (< i cnt)
          (let [(off slen) (slot-at p i)]
            (table.insert slots-parts (pack "<I4I4" off slen)))
          (table.insert slots-parts (pack "<I4I4" 0 0))))
    (.. (pack "<I4I4I8" new-lower new-upper lsn)
        (table.concat slots-parts)
        (string.rep "\0" (- new-upper new-lower))
        bytes
        (p:sub (+ upper 1)))))

(fn each-item [page f]
  "Call f with each live item as item-number and bytes, in slot order."
  (for [i 0 (- (slot-count page) 1)]
    (let [(off len) (slot-at page i)]
      (when (> len 0)
        (f (+ i 1) (page:sub (+ off 1) (+ off len)))))))

{:BLKSZ BLKSZ :HEADER-SIZE HEADER-SIZE :SLOT-SIZE SLOT-SIZE
 :page-init page-init :header header :set-header set-header
 :get-lsn get-lsn :set-lsn set-lsn
 :slot-count slot-count :slot-at slot-at :set-slot-at set-slot-at
 :free-space free-space :live-count live-count :add-item add-item
 :get-item get-item :delete-item delete-item :compact compact
 :restore-slot restore-slot :each-item each-item}
