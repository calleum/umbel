;;; umbel.btree — in-memory B+ tree (min degree 2).
;;
;; Keys are integers, values are opaque. All data lives in leaves; the
;; internal levels hold separator keys only. Leaf splits COPY the middle
;; key up (it stays in the right sibling), internal splits MOVE it up.
;; Insertion is the preemptive CLRS strategy: any full child is split on
;; the way down, so a split never has to bubble back up.
;;
;; Deletion is deliberately a rebuild: filter the leaf pairs and reinsert.
;; Correct and simple; rebalancing redistribution is left as a ladder
;; rung, exactly like the C engine's roadmap.

(local T 2)                ; min degree: nodes hold between t-1 and 2t-1 keys
(local MAX-KEYS (- (* 2 T) 1))

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
  "First index i with keys[i] > key — descent past copy-up separators,
because a separator key also lives in the child to its right."
  (var i 1)
  (while (and (<= i (length keys)) (<= (. keys i) key))
    (set i (+ i 1)))
  i)

(fn take [arr from to]
  "Slice arr[from..to] into a fresh table."
  (local out [])
  (for [i from to] (table.insert out (. arr i)))
  out)

(fn split-child [parent i]
  "Split the full child parent.children[i]; attach the right half."
  (local child (. parent.children i))
  (local sep (. child.keys T))
  (local right (if child.leaf (new-leaf) {:leaf false :keys [] :children []}))
  (if child.leaf
      (do ;; copy-up: the separator stays in the right leaf too
        (set right.keys (take child.keys T MAX-KEYS))
        (set right.vals (take child.vals T MAX-KEYS))
        (set child.keys (take child.keys 1 (- T 1)))
        (set child.vals (take child.vals 1 (- T 1)))
        (set right.next child.next)
        (set child.next right))
      (do ;; move-up: the separator leaves the children
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
          ;; separator was copied up: equal keys live right
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

(fn delete [tree key]
  "Remove key by rebuild; nil when the key was absent."
  (when (= (search tree key) nil)
    (error (.. "btree: no such key " key)))
  (local kept (icollect [_ p (ipairs (pairs-all tree))]
                (when (not= (. p 1) key) p)))
  (local fresh (new))
  (each [_ p (ipairs kept)]
    (insert fresh (. p 1) (. p 2)))
  (set tree.root fresh.root)
  (set tree.count fresh.count)
  tree)

{: T : MAX-KEYS : new : insert : search : range : pairs-all : delete
 : full? : find-index : descend-index}
