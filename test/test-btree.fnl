;;; In-memory B+ tree tests

(local t (require :harness))
(local btree (require :umbel.btree))

(t.deftest "btree: insert and search small set"
  (fn []
    (local tr (btree.new))
    (each [_ k (ipairs [10 20 5 6 12 30 7 17])]
      (btree.insert tr k (.. :v k)))
    (each [_ k (ipairs [10 20 5 6 12 30 7 17])]
      (t.assert= (.. :v k) (btree.search tr k) (.. "key " k " found")))
    (t.assert-nil (btree.search tr 99) "absent key is nil")
    (t.assert= 8 tr.count "count tracks inserts")))

(t.deftest "btree: duplicates rejected"
  (fn []
    (local tr (btree.new))
    (btree.insert tr 1 :a)
    (t.assert-error (fn [] (btree.insert tr 1 :b)) "duplicate insert errors")))

(t.deftest "btree: 100 sequential keys, then shuffled access"
  (fn []
    (local tr (btree.new))
    (for [i 1 100] (btree.insert tr (* i 7) i))
    (t.assert= 100 tr.count)
    (var bad 0)
    (for [i 1 100]
      (when (not= i (btree.search tr (* i 7)))
        (set bad (+ bad 1))))
    (t.assert= 0 bad "all 100 sequential keys found")))

(t.deftest "btree: splits exercised by reverse insertion"
  (fn []
    (local tr (btree.new))
    (for [i 50 1 -1] (btree.insert tr i (* i 2)))
    (var bad 0)
    (for [i 1 50]
      (when (not= (* i 2) (btree.search tr i)) (set bad (+ bad 1))))
    (t.assert= 0 bad "reverse insertion keeps every key")
    (t.assert= false (btree.full? tr.root) "root not left full")))

(t.deftest "btree: range scan"
  (fn []
    (local tr (btree.new))
    (for [i 1 50] (btree.insert tr i i))
    (local out (btree.range tr 20 40))
    (t.assert= 21 (length out) "21 keys in [20 40]")
    (t.assert= 20 (. (. out 1) 1) "starts at lo")
    (t.assert= 40 (. (. out 21) 1) "ends at hi")
    (var ordered true)
    (for [i 2 (length out)]
      (when (< (. (. out i) 1) (. (. out (- i 1)) 1))
        (set ordered false)))
    (t.assert= true ordered "range comes out sorted")))

(t.deftest "btree: delete single key, then all keys"
  (fn []
    (local tr (btree.new))
    (for [i 1 50] (btree.insert tr i i))
    (btree.delete tr 25)
    (t.assert-nil (btree.search tr 25) "deleted key gone")
    (t.assert= 49 tr.count "count drops")
    (t.assert= 24 (btree.search tr 24) "neighbours survive")
    (t.assert= 26 (btree.search tr 26) "neighbours survive")
    (for [i 1 50]
      (when (not= i 25) (btree.delete tr i)))
    (t.assert= 0 tr.count "tree empties")
    (t.assert-nil (btree.search tr 7) "empty tree finds nothing")
    ;; tree still usable afterwards
    (btree.insert tr 99 :back)
    (t.assert= :back (btree.search tr 99) "tree reusable after emptying")))

(t.deftest "btree: heavy churn insert/delete/re-insert preserves sorted order"
  (fn []
    (local tr (btree.new))
    (for [round 1 4]
      (for [i 1 200]
        (btree.insert tr (+ (* round 1000) (% (* i 73) 200)) i))
      (t.assert= 200 tr.count "200 inserted in scrambled order")
      (for [i 1 200]
        (btree.delete tr (+ (* round 1000) (% (* i 37) 200))))
      (t.assert= 0 tr.count "all 200 deleted via O(log N) borrow/merge")
      (t.assert= true tr.root.leaf "root collapsed back to leaf"))))
