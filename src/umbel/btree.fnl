;;; umbel.btree — in-memory B+ tree (min degree 2) with O(log N) delete.
;;
;; Keys are integers, values are opaque. All data lives in leaves; internal
;; levels hold separator keys only. Supports preemptive splits on insert and
;; full sibling borrow, merge, and root collapse on delete.

(local T 2)

;; min degree: non-root nodes hold [T-1 .. 2T-1] keys
(local MAX-KEYS (- (* 2 T) 1))
(local MIN-KEYS (- T 1))

(fn new-leaf []
  {:leaf true :keys [] :vals [] :next nil})

(fn new []
  {:root (new-leaf) :count 0})

(fn full? [node]
  (>= (length node.keys) MAX-KEYS))

(fn find-index [keys key]
  "First index i with keys[i] >= key (1-based), else past the end."
  (var i 1)
  (while (and (<= i (length keys)) (< (. keys i) key))
    (set i (+ i 1)))
  i)

(fn descend-index [keys key]
  "First index i with keys[i] > key — descent past copy-up separators."
  (var i 1)
  (while (and (<= i (length keys)) (<= (. keys i) key))
    (set i (+ i 1)))
  i)

(fn take [arr from to]
  (local out [])
  (for [i from to] (table.insert out (. arr i)))
  out)

(fn split-child [parent i]
  "Split the full child parent.children[i]; attach the right half."
  (local child (. parent.children i))
  (local sep (. child.keys T))
  (local right (if child.leaf (new-leaf) {:leaf false :keys [] :children []}))
  (if child.leaf
      (do
        (set right.keys (take child.keys T MAX-KEYS))
        (set right.vals (take child.vals T MAX-KEYS))
        (set child.keys (take child.keys 1 (- T 1)))
        (set child.vals (take child.vals 1 (- T 1)))
        (set right.next child.next)
        (set child.next right))
      (do
        (set right.keys (take child.keys (+ T 1) MAX-KEYS))
        (set right.children (take child.children (+ T 1) (* 2 T)))
        (set child.keys (take child.keys 1 (- T 1)))
        (set child.children (take child.children 1 T))))
  (table.insert parent.keys i sep)
  (table.insert parent.children (+ i 1) right))

(fn insert-nonfull [node key val]
  (if node.leaf
      (let [i (find-index node.keys key)]
        (when (and (<= i (length node.keys)) (= (. node.keys i) key))
          (error (.. "btree: duplicate key " key)))
        (table.insert node.keys i key)
        (table.insert node.vals i val))
      (let [i0 (descend-index node.keys key)]
        (var i i0)
        (when (full? (. node.children i))
          (split-child node i)
          (when (>= key (. node.keys i)) (set i (+ i 1))))
        (insert-nonfull (. node.children i) key val))))

(fn insert [tree key val]
  "Insert key -> val; errors on duplicate keys."
  (when (full? tree.root)
    (local new-root {:leaf false :keys [] :children [tree.root]})
    (split-child new-root 1)
    (set tree.root new-root))
  (insert-nonfull tree.root key val)
  (set tree.count (+ tree.count 1))
  tree)

(fn leaf-for [node key]
  (if node.leaf
      node
      (leaf-for (. node.children (descend-index node.keys key)) key)))

(fn search [tree key]
  "Return the value stored at key, or nil."
  (local leaf (leaf-for tree.root key))
  (var found nil)
  (each [i k (ipairs leaf.keys)]
    (when (and (= found nil) (= k key))
      (set found (. leaf.vals i))))
  found)

(fn range [tree lo hi]
  "Collect [key value] pairs with lo <= key <= hi in key order."
  (local out [])
  (var leaf (leaf-for tree.root lo))
  (var stop false)
  (while (and (not stop) (not= leaf nil))
    (each [i k (ipairs leaf.keys)]
      (when (> k hi) (set stop true))
      (when (and (not stop) (>= k lo) (<= k hi))
        (table.insert out [k (. leaf.vals i)])))
    (when (not stop) (set leaf leaf.next)))
  out)

(fn pairs-all [tree]
  "Collect every [key value] pair in key order."
  (range tree math.mininteger math.maxinteger))

;; -- O(log N) delete with borrow, merge & root collapse -------------------

(fn rebalance-child [parent i]
  (local child (. parent.children i))
  (when (< (length child.keys) MIN-KEYS)
    (local left (when (> i 1) (. parent.children (- i 1))))
    (local right (when (<= i (length parent.keys)) (. parent.children (+ i 1))))
    (if (and left (> (length left.keys) MIN-KEYS))
        ;; borrow from left
        (if child.leaf
            (do
              (table.insert child.keys 1 (table.remove left.keys))
              (table.insert child.vals 1 (table.remove left.vals))
              (tset parent.keys (- i 1) (. child.keys 1)))
            (do
              (table.insert child.keys 1 (. parent.keys (- i 1)))
              (table.insert child.children 1 (table.remove left.children))
              (tset parent.keys (- i 1) (table.remove left.keys))))
        (and right (> (length right.keys) MIN-KEYS))
        ;; borrow from right
        (if child.leaf
            (do
              (table.insert child.keys (table.remove right.keys 1))
              (table.insert child.vals (table.remove right.vals 1))
              (tset parent.keys i (. right.keys 1)))
            (do
              (table.insert child.keys (. parent.keys i))
              (table.insert child.children (table.remove right.children 1))
              (tset parent.keys i (table.remove right.keys 1))))
        ;; merge with sibling
        (let [lidx (if (> i 1) (- i 1) i)
              l (. parent.children lidx)
              r (. parent.children (+ lidx 1))
              sep (table.remove parent.keys lidx)]
          (table.remove parent.children (+ lidx 1))
          (if l.leaf
              (do
                (each [j k (ipairs r.keys)]
                  (table.insert l.keys k)
                  (table.insert l.vals (. r.vals j)))
                (set l.next r.next))
              (do
                (table.insert l.keys sep)
                (each [_ k (ipairs r.keys)] (table.insert l.keys k))
                (each [_ c (ipairs r.children)] (table.insert l.children c))))))))

(fn delete-at [node key]
  (if node.leaf
      (do
        (var pos nil)
        (each [i k (ipairs node.keys)]
          (when (and (= pos nil) (= k key)) (set pos i)))
        (if (= pos nil) false
            (do
              (table.remove node.keys pos)
              (table.remove node.vals pos)
              true)))
      (let [i (descend-index node.keys key)
            deleted? (delete-at (. node.children i) key)]
        (when deleted? (rebalance-child node i))
        deleted?)))

(fn delete [tree key]
  "Remove key with O(log N) borrow/merge/collapse; errors if absent."
  (when (not (delete-at tree.root key))
    (error (.. "btree: no such key " key)))
  (set tree.count (- tree.count 1))
  (when (and (not tree.root.leaf) (= (length tree.root.keys) 0))
    (set tree.root (. tree.root.children 1)))
  tree)

{: T
 : MAX-KEYS
 : new
 : insert
 : search
 : range
 : pairs-all
 : delete
 : full?
 : find-index
 : descend-index}
