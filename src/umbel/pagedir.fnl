;;; umbel.pagedir — single-page free space directory.
;;
;; The directory lives on its own page and tracks every data page of the
;; table with its current free space. Layout (all LE):
;;
;;   bytes 1..4   count u32
;;   then         count x { page-id u32, free-space u32 }
;;
;; Capacity is floor((BLKSZ - 4) / 8) entries — plenty for a toy engine.

(local page (require :umbel.page))
(local pack string.pack)
(local unpack string.unpack)

(local CAPACITY (// (- page.BLKSZ 4) 8))

(fn init []
  "Return a fresh empty directory state."
  {:entries [] :capacity CAPACITY})

(fn to-page [dir]
  "Encode directory state into its on-disk page."
  (.. (pack :<I4 (length dir.entries))
      (table.concat (icollect [_ e (ipairs dir.entries)]
                      (pack :<I4I4 e.page e.free)))
      (string.rep "\000" (- page.BLKSZ 4 (* 8 (length dir.entries))))))

(fn from-page [data]
  "Decode a directory page into directory state."
  (let [cnt (unpack :<I4 data 1)
        entries []]
    (for [i 1 cnt]
      (let [pos (+ 5 (* (- i 1) 8))
            (pg free) (unpack :<I4I4 data pos)]
        (table.insert entries {:page pg : free})))
    {: entries :capacity CAPACITY}))

(fn find-by-page [dir page-id]
  "Return the entry index for page-id, or nil."
  (var found nil)
  (each [i e (ipairs dir.entries)]
    (when (and (= found nil) (= e.page page-id)) (set found i)))
  found)

(fn add-entry [dir page-id free]
  "Register a new page with its free space."
  (when (find-by-page dir page-id)
    (error (.. "pagedir: page " page-id " already registered")))
  (when (>= (length dir.entries) dir.capacity)
    (error "pagedir: directory page full"))
  (table.insert dir.entries {:page page-id : free}))

(fn find-free [dir required]
  "Return the first page id with at least `required` free bytes, or nil."
  (var found nil)
  (each [_ e (ipairs dir.entries)]
    (when (and (= found nil) (>= e.free required)) (set found e.page)))
  found)

(fn update-free [dir page-id free]
  "Update the recorded free space of page-id."
  (let [i (or (find-by-page dir page-id)
              (error (.. "pagedir: no entry for page " page-id)))]
    (tset (. dir.entries i) :free free)))

(fn total-free [dir]
  "Sum of free space over all pages."
  (var sum 0)
  (each [_ e (ipairs dir.entries)] (set sum (+ sum e.free)))
  sum)

{: CAPACITY
 : init
 : to-page
 : from-page
 : add-entry
 : find-by-page
 : find-free
 : update-free
 : total-free}
