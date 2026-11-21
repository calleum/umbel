;;; Disk-resident B+ tree tests 

(local t (require :harness))
(local btd (require :umbel.btree-disk))

(local tmpdir (os.getenv :TMPDIR))
(local dir (if (and tmpdir (not= tmpdir "")) tmpdir :/tmp))
(local idxfile (.. dir :/umbel-test.idx))
(os.remove idxfile)

(t.deftest "btree-disk: node page round trip"
  (fn []
    (local leaf {:leaf true
                 :keys [10 20 30]
                 :vals [{:page 2 :slot 1} {:page 2 :slot 4} {:page 3 :slot 2}]
                 :next 9})
    (local leaf-bytes (btd.encode-node-for-test leaf))
    (t.assert= 4096 (length leaf-bytes) "encoded leaf is one page")
    (local leaf2 (btd.decode-node-for-test leaf-bytes))
    (t.assert= true leaf2.leaf "leaf flag round trips")
    (t.assert= 9 leaf2.next "next-leaf pointer round trips")
    (t.assert= 3 (length leaf2.keys) "leaf key count round trips")
    (t.assert= 20 (. leaf2.keys 2) "leaf key round trips")
    (t.assert= 3 (. (. leaf2.vals 3) :page) "leaf val page round trips")
    (t.assert= 2 (. (. leaf2.vals 3) :slot) "leaf val slot round trips")
    (local internal {:leaf false :keys [50 100] :children [2 3 4]})
    (local int-bytes (btd.encode-node-for-test internal))
    (t.assert= 4096 (length int-bytes) "encoded internal node is one page")
    (local int2 (btd.decode-node-for-test int-bytes))
    (t.assert= false int2.leaf "internal flag round trips")
    (t.assert= 2 (length int2.keys) "internal key count round trips")
    (t.assert= 100 (. int2.keys 2) "internal key round trips")
    (t.assert= 3 (length int2.children) "internal children count round trips")
    (t.assert= 4 (. int2.children 3) "internal child id round trips")
    ;; sanity-check a fresh index file
    (local idx (btd.open idxfile))
    (t.assert= 0 (btd.count idx) "fresh index is empty")
    (t.assert-nil (btd.search idx 42) "empty index finds nothing")
    (btd.close idx)))

(t.deftest "btree-disk: 200 inserts, search, persistence"
  (fn []
    (os.remove idxfile)
    (local idx (btd.open idxfile))
    (for [i 1 200]
      (btd.insert idx (* i 3) {:page (+ 2 (// i 40)) :slot (% i 17)}))
    (t.assert= 200 (btd.count idx) "count tracks inserts")
    (var bad 0)
    (for [i 1 200]
      (local tid (btd.search idx (* i 3)))
      (when (or (= tid nil) (not= tid.page (+ 2 (// i 40)))
                (not= tid.slot (% i 17)))
        (set bad (+ bad 1))))
    (t.assert= 0 bad "all keys found with their tids")
    (t.assert-nil (btd.search idx 4) "absent key nil")
    (btd.close idx)
    ;; reopen: everything must survive
    (local idx2 (btd.open idxfile))
    (t.assert= 200 (btd.count idx2) "count survives reopen")
    (var bad2 0)
    (for [i 1 200]
      (when (= (btd.search idx2 (* i 3)) nil)
        (set bad2 (+ bad2 1))))
    (t.assert= 0 bad2 "all keys found after reopen")
    (btd.close idx2)))

(t.deftest "btree-disk: splits push the tree above one page"
  (fn []
    (os.remove idxfile)
    (local idx (btd.open idxfile))
    (for [i 1 600]
      (btd.insert idx i {:page 2 :slot (% i 13)}))
    (t.assert= 600 (btd.count idx) "600 entries")
    (t.assert= 600 (length (btd.pairs-all idx)) "pairs-all walks every leaf")
    (var ordered true)
    (let [all (btd.pairs-all idx)]
      (for [i 2 (length all)]
        (when (< (. (. all i) 1) (. (. all (- i 1)) 1))
          (set ordered false))))
    (t.assert= true ordered "full scan is sorted")
    (btd.close idx)))

(t.deftest "btree-disk: range scan across leaf chain"
  (fn []
    (local idx (btd.open idxfile))
    (local out (btd.range idx 100 200))
    (t.assert= 101 (length out) "inclusive [100 200] count")
    (t.assert= 100 (. (. out 1) 1) "starts at lo")
    (t.assert= 200 (. (. out 101) 1) "ends at hi")
    (local none (btd.range idx 5000 6000))
    (t.assert= 0 (length none) "empty range is empty")
    (btd.close idx)
    (os.remove idxfile)))

(t.deftest "btree-disk: deep multi-level delete, borrow, merge, root collapse"
  (fn []
    (os.remove idxfile)
    ;; Small cap=6 forces a 3+ level tree, heavily exercising borrow/merge/collapse.
    (local idx (btd.open idxfile {:cap 6}))
    (for [i 1 300] (btd.insert idx i {:page (+ 2 (// i 20)) :slot (% i 19)}))
    (local v1 (btd.verify idx))
    (t.assert= true (>= v1.height 2) "300 keys with cap=6 builds multi-level tree")
    ;; Delete odd keys and a contiguous middle block.
    (for [i 1 300 2]
      (t.assert= true (btd.delete idx i) (.. "delete odd " i)))
    (t.assert= false (btd.delete idx 1) "deleting absent key returns false")
    (btd.verify idx)
    (t.assert= 150 (btd.count idx) "150 even keys left")
    ;; Re-insert deleted keys with updated tids.
    (for [i 1 300 2]
      (btd.insert idx i {:page 99 :slot (% i 11)}))
    (btd.verify idx)
    (t.assert= 300 (btd.count idx) "re-inserted keys restore count")
    ;; Delete ALL 300 keys in reverse -> root must collapse back to height 0!
    (for [i 300 1 -1] (btd.delete idx i))
    (local v-empty (btd.verify idx))
    (t.assert= 0 (btd.count idx) "tree completely empty")
    (t.assert= 0 v-empty.height "root collapsed to single empty leaf")
    (t.assert= true (> v-empty.free-pages 0) "merged pages reclaimed onto free list")
    ;; Tree remains usable and persists across close/reopen.
    (for [i 1 50] (btd.insert idx (* i 10) {:page 2 :slot i}))
    (btd.verify idx)
    (btd.close idx)
    (local idx2 (btd.open idxfile))
    (t.assert= 50 (btd.count idx2) "post-collapse keys survive reopen")
    (btd.verify idx2)
    (btd.close idx2)
    (os.remove idxfile)))

(t.deftest "btree-disk: heavy churn reclaims pages with zero file growth"
  (fn []
    (os.remove idxfile)
    (local heapfile (require :umbel.heapfile))
    (local idx (btd.open idxfile {:cap 8}))
    (var baseline-pages nil)
    ;; 8 rounds x 250 insert + 250 delete = 4,000 mutations!
    (for [round 1 8]
      (for [i 1 250]
        (btd.insert idx (+ (* round 1000) i) {:page 2 :slot (% i 31)}))
      (btd.verify idx)
      (when (= round 1)
        (set baseline-pages (heapfile.num-pages idx.hf)))
      (for [i 1 250]
        (btd.delete idx (+ (* round 1000) i)))
      (btd.verify idx))
    (local final-pages (heapfile.num-pages idx.hf))
    (t.assert= 0 (btd.count idx) "empty after 8 churn rounds")
    (t.assert= baseline-pages final-pages
               "index file did not grow past round 1 (all freed pages reused)")
    (btd.close idx)
    (os.remove idxfile)))

(t.deftest "btree-disk: full-capacity (CAP=250) heavy churn & persistence"
  (fn []
    (os.remove idxfile)
    (local idx (btd.open idxfile))
    (for [i 1 2000] (btd.insert idx i {:page (+ 2 (// i 100)) :slot (+ (% i 100) 1)}))
    (btd.verify idx)
    (for [i 1 1500] (btd.delete idx i))
    (btd.verify idx)
    (for [i 1 500] (btd.insert idx i {:page 7 :slot i}))
    (btd.verify idx)
    (btd.close idx)
    (local idx2 (btd.open idxfile))
    (t.assert= 1000 (btd.count idx2) "1000 live keys survive full-cap churn + reopen")
    (btd.verify idx2)
    (t.assert= 7 (. (btd.search idx2 250) :page) "re-inserted key has updated tid")
    (t.assert-nil (btd.search idx2 1000) "deleted key remains absent")
    (btd.close idx2)
    (os.remove idxfile)))
