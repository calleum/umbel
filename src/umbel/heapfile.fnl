;;; umbel.heapfile — multi-page heap file on top of stdio.
;;
;; A heap file is a plain file of BLKSZ-byte pages addressed by 0-based
;; page id. Open creates the file when absent. All bounds are checked:
;; reading or writing a page id >= num-pages is an error; extend always
;; appends at the end and returns the new page id.

(local page (require :umbel.page))

(fn open [path]
  "Open (or create) a heap file at path; return the handle."
  (let [f (or (io.open path "r+b") (io.open path "w+b"))]
    (when (= f nil) (error (.. "heapfile: cannot open " path)))
    {:file f : path :blksz page.BLKSZ}))

(fn num-pages [hf]
  (math.floor (/ (hf.file:seek "end") hf.blksz)))

(fn close [hf]
  (hf.file:close))

(fn page-aligned? [hf]
  (= (% (hf.file:seek "end") hf.blksz) 0))

(fn read-page [hf page-id]
  "Read page page-id; return the BLKSZ-byte string."
  (when (>= page-id (num-pages hf))
    (error (.. "heapfile: invalid page id " page-id)))
  (hf.file:seek "set" (* page-id hf.blksz))
  (let [data (hf.file:read hf.blksz)]
    (when (or (= data nil) (< (length data) hf.blksz))
      (error (.. "heapfile: short read on page " page-id)))
    data))

(fn write-page [hf page-id data]
  "Write a BLKSZ-byte page in place."
  (when (>= page-id (num-pages hf))
    (error (.. "heapfile: invalid page id " page-id)))
  (when (not= (length data) hf.blksz)
    (error "heapfile: page writes must be exactly BLKSZ bytes"))
  (hf.file:seek "set" (* page-id hf.blksz))
  (hf.file:write data)
  (hf.file:flush))

(fn extend [hf data]
  "Append a new page; return its page id."
  (when (not= (length data) hf.blksz)
    (error "heapfile: page writes must be exactly BLKSZ bytes"))
  (when (not (page-aligned? hf))
    (error "heapfile: file is not page aligned"))
  (local page-id (num-pages hf))
  (hf.file:seek "end")
  (hf.file:write data)
  (hf.file:flush)
  page-id)

{:open open :close close :num-pages num-pages :page-aligned? page-aligned?
 :read-page read-page :write-page write-page :extend extend}
