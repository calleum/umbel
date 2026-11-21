;;; Heap file, page directory, and buffer manager tests.

(local t (require :harness))
(local page (require :umbel.page))
(local heapfile (require :umbel.heapfile))
(local pagedir (require :umbel.pagedir))
(local bufmgr (require :umbel.bufmgr))

(local tmpdir (os.getenv "TMPDIR"))
(local dir (if (and tmpdir (not= tmpdir "")) tmpdir "/tmp"))
(local dbfile (.. dir "/umbel-test-heap.db"))

(os.remove dbfile)

;; ---- heap file ----------------------------------------------------------

(t.deftest "heapfile: open creates, extend appends"
  (fn []
    (local hf (heapfile.open dbfile))
    (t.assert= 0 (heapfile.num-pages hf) "new file has no pages")
    (local p0 (page.page-init))
    (t.assert= 0 (heapfile.extend hf p0) "first extend returns page 0")
    (t.assert= 1 (heapfile.extend hf p0) "second extend returns page 1")
    (t.assert= 2 (heapfile.num-pages hf) "file now has two pages")
    (heapfile.close hf)))

(t.deftest "heapfile: write then read round trip"
  (fn []
    (local hf (heapfile.open dbfile))
    (var p (page.page-init))
    (local (p1 _) (page.add-item p "heap round trip"))
    (heapfile.write-page hf 1 p1)
    (local back (heapfile.read-page hf 1))
    (t.assert= "heap round trip" (page.get-item back 1) "item survives disk")
    (t.assert= p1 back "page bytes identical after round trip")
    (t.assert-error (fn [] (heapfile.read-page hf 2))
                    "reading past end is an error")
    (t.assert-error (fn [] (heapfile.write-page hf 9 p))
                    "writing past end is an error")
    (heapfile.close hf)))

(t.deftest "heapfile: reopen keeps pages"
  (fn []
    (local hf (heapfile.open dbfile))
    (t.assert= 2 (heapfile.num-pages hf) "page count survives close/open")
    (heapfile.close hf)
    (os.remove dbfile)))

;; ---- page directory -----------------------------------------------------

(t.deftest "pagedir: encode/decode round trip"
  (fn []
    (local d (pagedir.init))
    (pagedir.add-entry d 0 4088)
    (pagedir.add-entry d 1 2048)
    (pagedir.add-entry d 2 16)
    (local d2 (pagedir.from-page (pagedir.to-page d)))
    (t.assert= 3 (length d2.entries) "entry count round trips")
    (t.assert= 0 (. (. d2.entries 1) :page))
    (t.assert= 4088 (. (. d2.entries 1) :free))
    (t.assert= 2 (. (. d2.entries 3) :page))
    (t.assert= 16 (. (. d2.entries 3) :free))))

(t.deftest "pagedir: find-free and update-free"
  (fn []
    (local d (pagedir.init))
    (pagedir.add-entry d 0 10)
    (pagedir.add-entry d 1 500)
    (pagedir.add-entry d 2 100)
    (t.assert= 1 (pagedir.find-free d 100) "first fit picks page 1")
    (t.assert-nil (pagedir.find-free d 900) "nothing fits a huge request")
    (pagedir.update-free d 1 0)
    (t.assert= 2 (pagedir.find-free d 100) "update shifts the answer")
    (t.assert-error (fn [] (pagedir.update-free d 9 1))
                    "updating an unknown page errors")
    (t.assert-error (fn [] (pagedir.add-entry d 0 1))
                    "duplicate registration errors")
    (t.assert= 110 (pagedir.total-free d) "total free sums entries")))

(t.deftest "pagedir: capacity is bounded"
  (fn []
    (local d (pagedir.init))
    (for [i 0 (- d.capacity 1)] (pagedir.add-entry d i 1))
    (t.assert-error (fn [] (pagedir.add-entry d 999 1))
                    "directory page holds at most CAPACITY entries")))

;; ---- buffer manager -----------------------------------------------------

(t.deftest "bufmgr: pin, release, dirty write-back"
  (fn []
    (os.remove dbfile)
    (local hf (heapfile.open dbfile))
    (var seed (page.page-init))
    (local (seed _) (page.add-item seed "seeded"))
    (heapfile.extend hf seed)
    (heapfile.extend hf (page.page-init))
    (local buf (bufmgr.new 2))
    (local s0 (bufmgr.get-page buf hf 0))
    (t.assert= 1 (bufmgr.pinned-count buf) "get-page pins")
    (t.assert= "seeded" (page.get-item s0.data 1) "slot loaded page 0")
    (bufmgr.release buf s0 true)
    (t.assert= 1 (bufmgr.dirty-count buf) "released dirty")
    (local s1 (bufmgr.get-page buf hf 1))
    (bufmgr.release buf s1 false)
    (bufmgr.flush-all buf hf)
    (t.assert= 0 (bufmgr.dirty-count buf) "flush clears dirty flags")
    (heapfile.close hf)))

(t.deftest "bufmgr: eviction with a tiny pool"
  (fn []
    (os.remove dbfile)
    (local hf (heapfile.open dbfile))
    (local buf (bufmgr.new 2))
    ;; six pages, each tagged with its page id
    (for [i 0 5]
      (var p (page.page-init))
      (local (p2 _) (page.add-item p (string.pack "<I4" i)))
      (heapfile.extend hf p2))
    ;; cycle through all six with a 2-slot pool
    (for [round 1 3]
      (for [i 0 5]
        (local s (bufmgr.get-page buf hf i))
        (t.assert= i (string.unpack "<I4" (page.get-item s.data 1))
                   (.. "round " round " page " i " holds its tag"))
        (bufmgr.release buf s false)))
    (heapfile.close hf)))

(t.deftest "bufmgr: dirty eviction writes through"
  (fn []
    (os.remove dbfile)
    (local hf (heapfile.open dbfile))
    (local buf (bufmgr.new 2))
    (for [i 0 4] (heapfile.extend hf (page.page-init)))
    ;; dirty page 0 with marker, then force eviction by touching 1,2,3
    (local s0 (bufmgr.get-page buf hf 0))
    (local (p _) (page.add-item s0.data "evicted-dirty"))
    (set s0.data p)
    (bufmgr.release buf s0 true)
    (each [_ i (ipairs [1 2 3])]
      (let [s (bufmgr.get-page buf hf i)]
        (bufmgr.release buf s false)))
    ;; fresh read straight from disk bypassing the pool
    (local from-disk (page.get-item (heapfile.read-page hf 0) 1))
    (t.assert= "evicted-dirty" from-disk "dirty page reached the disk")
    (heapfile.close hf)
    (os.remove dbfile)))

(t.deftest "bufmgr: hot page usage is capped and still evictable"
  (fn []
    (os.remove dbfile)
    (local hf (heapfile.open dbfile))
    (local buf (bufmgr.new 2))
    (for [i 0 2] (heapfile.extend hf (page.page-init)))
    (for [_ 1 12]
      (let [s (bufmgr.get-page buf hf 0)] (bufmgr.release buf s false))
      (let [s (bufmgr.get-page buf hf 1)] (bufmgr.release buf s false)))
    (local s2 (bufmgr.get-page buf hf 2))
    (bufmgr.release buf s2 false)
    (t.assert= 0 (bufmgr.pinned-count buf) "hot pages evicted without error")
    (heapfile.close hf)
    (os.remove dbfile)))
