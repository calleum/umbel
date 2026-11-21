;;; umbel.btree-disk — disk-resident B+ tree with churn-safe delete & page reuse.
;;
;; Every node is one 4 KiB page of a dedicated index heap file, paged through
;; the buffer manager. Layout (all LE):
;;
;;   page 0    meta: "UBT2" | root u32 | first-leaf u32 | count u64
;;                   | free-head u32 | free-count u32 | cap u16
;;   page 1+   nodes & free pages:
;;     byte 1        flags u8 (0 = internal, 1 = leaf, 2 = free)
;;     bytes 2..3    nkeys u16
;;     bytes 4..7    next u32 (next-leaf for leaves, next-free for free pages)
;;     byte 13..     keys: i64 x cap
;;     then          leaf values (page u32, slot u32) x cap
;;                   or internal children u32 x (cap + 1)
;;
;; Supports O(log N) insert (preemptive split), search, range scan, and
;; delete with sibling redistribution (borrow), merge, root collapse, and
;; on-disk free-page reclamation so heavy insert/delete churn never leaks
;; pages or degenerates tree height.

(local page (require :umbel.page))
(local heapfile (require :umbel.heapfile))
(local bufmgr (require :umbel.bufmgr))

(local MAGIC :UBT2)
(local DEFAULT-CAP 250)
(local KEYS-OFF 13)

(local FLAG-INTERNAL 0)
(local FLAG-LEAF 1)
(local FLAG-FREE 2)

(local pack string.pack)
(local unpack string.unpack)

(fn vals-off [cap]
  (+ KEYS-OFF (* cap 8)))

(fn check-cap [cap]
  (when (or (< cap 4) (> cap DEFAULT-CAP) (not= (% cap 2) 0))
    (error (.. "btree-disk: cap must be even in [4, " DEFAULT-CAP "], got "
               (tostring cap))))
  cap)

;; -- node & free-page codecs ----------------------------------------------

(fn encode-node [n cap]
  "Serialize a node table into its 4 KiB page image."
  (local c (or cap DEFAULT-CAP))
  (local head (.. (pack :<BI2I4 (if n.leaf FLAG-LEAF FLAG-INTERNAL)
                        (length n.keys) (if n.leaf (or n.next 0) 0))
                  (string.rep "\000" 5)))
  (local key-parts (icollect [_ k (ipairs n.keys)] (pack :<i8 k)))
  (local keys (table.concat key-parts))
  (local tail (if n.leaf
                  (table.concat (icollect [_ v (ipairs n.vals)]
                                  (pack :<I4I4 v.page v.slot)))
                  (table.concat (icollect [_ ch (ipairs n.children)]
                                  (pack :<I4 ch)))))
  (.. head keys (string.rep "\000" (- (* c 8) (length keys))) tail
      (string.rep "\000" (- page.BLKSZ 12 (* c 8) (length tail)))))

(fn decode-node [data cap]
  "Parse a 4 KiB page image back into a node table."
  (local c (or cap DEFAULT-CAP))
  (local voff (vals-off c))
  (local (flags nkeys next) (unpack :<BI2I4 data 1))
  (local n {:leaf (= flags FLAG-LEAF) :keys []})
  (when n.leaf (set n.next next))
  (for [i 1 nkeys]
    (let [(k) (unpack :<i8 data (+ KEYS-OFF (* (- i 1) 8)))]
      (table.insert n.keys k)))
  (if n.leaf
      (do
        (set n.vals [])
        (for [i 1 nkeys]
          (let [(pg slot) (unpack :<I4I4 data (+ voff (* (- i 1) 8)))]
            (table.insert n.vals {:page pg : slot}))))
      (do
        (set n.children [])
        (for [i 1 (+ nkeys 1)]
          (let [(cid) (unpack :<I4 data (+ voff (* (- i 1) 4)))]
            (table.insert n.children cid)))))
  n)

(fn encode-free [next-free]
  (.. (pack :<BI2I4 FLAG-FREE 0 (or next-free 0))
      (string.rep "\000" (- page.BLKSZ 7))))

(fn decode-free-next [data]
  (local (flags _ next) (unpack :<BI2I4 data 1))
  (when (not= flags FLAG-FREE)
    (error "btree-disk: corrupt free-page list entry"))
  next)

;; -- meta page ------------------------------------------------------------

(fn encode-meta [m]
  (.. MAGIC (pack :<I4I4i8I4I4I2 m.root m.first-leaf m.count m.free-head
                  m.free-count m.cap)
      (string.rep "\000" (- page.BLKSZ 30))))

(fn decode-meta [data]
  (when (not= (data:sub 1 4) MAGIC)
    (error "btree-disk: not an umbel index file"))
  (let [(root first-leaf count free-head free-count cap) (unpack :<I4I4i8I4I4I2
                                                                 data 5)]
    {: root : first-leaf : count : free-head : free-count : cap}))

;; -- engine lifecycle & page allocation -----------------------------------

(fn file-exists? [path]
  (let [f (io.open path :r)]
    (if f (do
            (f:close) true) false)))

(fn open [path opts]
  "Open or create the index file at path.
Optional opts.cap sets node capacity (even, 4..250, default 250)."
  (local existed (file-exists? path))
  (local hf (heapfile.open path))
  (local buf (bufmgr.new (or (when opts opts.pool-size) 32)))
  (local meta (if existed
                  (decode-meta (heapfile.read-page hf 0))
                  (let [cap (check-cap (or (when opts opts.cap) DEFAULT-CAP))
                        m {:root 1
                           :first-leaf 1
                           :count 0
                           :free-head 0
                           :free-count 0
                           : cap}]
                    (heapfile.extend hf (encode-meta m))
                    (heapfile.extend hf
                                     (encode-node {:leaf true
                                                   :keys []
                                                   :vals []
                                                   :next 0}
                                                  cap))
                    (heapfile.sync hf)
                    m)))
  (local half (// meta.cap 2))
  {: hf
   : buf
   : meta
   : path
   :cap meta.cap
   : half
   :min-leaf half
   :min-internal (- half 1)})

(fn node-load [idx node-id]
  (local s (bufmgr.get-page idx.buf idx.hf node-id))
  (local n (decode-node s.data idx.cap))
  (bufmgr.release idx.buf s false)
  n)

(fn node-save [idx node-id n]
  (local s (bufmgr.get-page idx.buf idx.hf node-id))
  (set s.data (encode-node n idx.cap))
  (bufmgr.release idx.buf s true))

(fn node-alloc [idx n]
  "Allocate a page for node `n`, popping from the on-disk free list first."
  (if (> idx.meta.free-head 0)
      (let [pid idx.meta.free-head
            s (bufmgr.get-page idx.buf idx.hf pid)
            next-free (decode-free-next s.data)]
        (set idx.meta.free-head next-free)
        (set idx.meta.free-count (- idx.meta.free-count 1))
        (set s.data (encode-node n idx.cap))
        (bufmgr.release idx.buf s true)
        pid)
      (heapfile.extend idx.hf (encode-node n idx.cap))))

(fn node-free [idx node-id]
  "Push `node-id` onto the on-disk free-page list for reuse."
  (local s (bufmgr.get-page idx.buf idx.hf node-id))
  (set s.data (encode-free idx.meta.free-head))
  (bufmgr.release idx.buf s true)
  (set idx.meta.free-head node-id)
  (set idx.meta.free-count (+ idx.meta.free-count 1)))

(fn save-meta [idx]
  (heapfile.write-page idx.hf 0 (encode-meta idx.meta)))

(fn flush [idx]
  "Durably flush all dirty nodes (fsync), then meta page (fsync)."
  (bufmgr.flush-all idx.buf idx.hf true)
  (save-meta idx)
  (heapfile.sync idx.hf))

(fn close [idx]
  "Durably persist all nodes and meta page, then close."
  (flush idx)
  (heapfile.close idx.hf))

(fn count [idx]
  idx.meta.count)

(fn full? [idx n]
  (>= (length n.keys) idx.cap))

(fn take [arr from to]
  (local out [])
  (for [k from to] (table.insert out (. arr k)))
  out)

;; -- insert & preemptive split --------------------------------------------

(fn split-child [idx parent i]
  "Split full child parent.children[i] in place (caller saves parent).
Leaves split symmetrically [1..half] and [half+1..cap], copying keys[half+1] up;
internal nodes move keys[half] up leaving [1..half-1] and [half+1..cap]."
  (local left-id (. parent.children i))
  (local child (node-load idx left-id))
  (local half idx.half)
  (local cap idx.cap)
  (var sep nil)
  (local right (if child.leaf
                   {:leaf true :keys [] :vals [] :next child.next}
                   {:leaf false :keys [] :children []}))
  (if child.leaf
      (do
        (set sep (. child.keys (+ half 1)))
        (set right.keys (take child.keys (+ half 1) cap))
        (set right.vals (take child.vals (+ half 1) cap))
        (set child.keys (take child.keys 1 half))
        (set child.vals (take child.vals 1 half)))
      (do
        (set sep (. child.keys half))
        (set right.keys (take child.keys (+ half 1) cap))
        (set right.children (take child.children (+ half 1) (+ cap 1)))
        (set child.keys (take child.keys 1 (- half 1)))
        (set child.children (take child.children 1 half))))
  (local right-id (node-alloc idx right))
  (when child.leaf (set child.next right-id))
  (node-save idx left-id child)
  (table.insert parent.keys i sep)
  (table.insert parent.children (+ i 1) right-id))

(fn leaf-insert [n key tid]
  (var i 1)
  (while (and (<= i (length n.keys)) (< (. n.keys i) key))
    (set i (+ i 1)))
  (when (and (<= i (length n.keys)) (= (. n.keys i) key))
    (error (.. "btree-disk: duplicate key " key)))
  (table.insert n.keys i key)
  (table.insert n.vals i tid))

(fn descend [n key]
  "Child index for key; equal separator keys live to the right."
  (var i 1)
  (while (and (<= i (length n.keys)) (<= (. n.keys i) key))
    (set i (+ i 1)))
  i)

(fn insert-nonfull [idx node-id key tid]
  (local n (node-load idx node-id))
  (if n.leaf
      (do
        (leaf-insert n key tid)
        (node-save idx node-id n))
      (do
        (var i (descend n key))
        (when (full? idx (node-load idx (. n.children i)))
          (split-child idx n i)
          (node-save idx node-id n)
          (when (>= key (. n.keys i)) (set i (+ i 1))))
        (insert-nonfull idx (. n.children i) key tid))))

(fn insert [idx key tid]
  "Insert key -> {page slot} TupleId."
  (when (full? idx (node-load idx idx.meta.root))
    (local new-root {:leaf false :keys [] :children [idx.meta.root]})
    (split-child idx new-root 1)
    (set idx.meta.root (node-alloc idx new-root)))
  (insert-nonfull idx idx.meta.root key tid)
  (set idx.meta.count (+ idx.meta.count 1)))

;; -- delete with borrow, merge, root collapse & page reuse ----------------

(fn borrow-from-left [idx parent i left-id left child-id child]
  (if child.leaf
      (let [k (table.remove left.keys)
            v (table.remove left.vals)]
        (table.insert child.keys 1 k)
        (table.insert child.vals 1 v)
        (tset parent.keys (- i 1) (. child.keys 1)))
      (let [sep (. parent.keys (- i 1))
            lk (table.remove left.keys)
            lc (table.remove left.children)]
        (table.insert child.keys 1 sep)
        (table.insert child.children 1 lc)
        (tset parent.keys (- i 1) lk)))
  (node-save idx left-id left)
  (node-save idx child-id child)
  (node-save idx parent._id parent))

(fn borrow-from-right [idx parent i child-id child right-id right]
  (if child.leaf
      (let [k (table.remove right.keys 1)
            v (table.remove right.vals 1)]
        (table.insert child.keys k)
        (table.insert child.vals v)
        (tset parent.keys i (. right.keys 1)))
      (let [sep (. parent.keys i)
            rk (table.remove right.keys 1)
            rc (table.remove right.children 1)]
        (table.insert child.keys sep)
        (table.insert child.children rc)
        (tset parent.keys i rk)))
  (node-save idx child-id child)
  (node-save idx right-id right)
  (node-save idx parent._id parent))

(fn merge-children [idx parent left-idx]
  "Merge parent.children[left-idx + 1] into parent.children[left-idx] and free
the right page."
  (local left-id (. parent.children left-idx))
  (local right-id (. parent.children (+ left-idx 1)))
  (local left (node-load idx left-id))
  (local right (node-load idx right-id))
  (local sep (table.remove parent.keys left-idx))
  (table.remove parent.children (+ left-idx 1))
  (if left.leaf
      (do
        (each [j k (ipairs right.keys)]
          (table.insert left.keys k)
          (table.insert left.vals (. right.vals j)))
        (set left.next right.next))
      (do
        (table.insert left.keys sep)
        (each [_ k (ipairs right.keys)] (table.insert left.keys k))
        (each [_ c (ipairs right.children)] (table.insert left.children c))))
  (node-save idx left-id left)
  (node-free idx right-id)
  (node-save idx parent._id parent))

(fn rebalance-child [idx parent i]
  (local child-id (. parent.children i))
  (local child (node-load idx child-id))
  (local min-k (if child.leaf idx.min-leaf idx.min-internal))
  (when (< (length child.keys) min-k)
    (local left-id (when (> i 1) (. parent.children (- i 1))))
    (local left (when left-id (node-load idx left-id)))
    (if (and left (> (length left.keys) min-k))
        (borrow-from-left idx parent i left-id left child-id child)
        (let [right-id (when (<= i (length parent.keys))
                         (. parent.children (+ i 1)))
              right (when right-id (node-load idx right-id))]
          (if (and right (> (length right.keys) min-k))
              (borrow-from-right idx parent i child-id child right-id right)
              (merge-children idx parent (if (> i 1) (- i 1) i)))))))

(fn delete-at [idx node-id key]
  (local n (node-load idx node-id))
  (set n._id node-id)
  (if n.leaf
      (do
        (var pos nil)
        (each [i k (ipairs n.keys)]
          (when (and (= pos nil) (= k key)) (set pos i)))
        (if (= pos nil) false
            (do
              (table.remove n.keys pos)
              (table.remove n.vals pos)
              (node-save idx node-id n)
              true)))
      (let [i (descend n key)
            deleted? (delete-at idx (. n.children i) key)]
        (when deleted?
          (rebalance-child idx n i))
        deleted?)))

(fn delete [idx key]
  "Delete `key` from the disk B+ tree with full borrow/merge/root-collapse.
Returns true when the key existed and was removed, false otherwise."
  (local deleted? (delete-at idx idx.meta.root key))
  (when deleted?
    (set idx.meta.count (- idx.meta.count 1))
    (local root (node-load idx idx.meta.root))
    (when (and (not root.leaf) (= (length root.keys) 0))
      (local old-root idx.meta.root)
      (set idx.meta.root (. root.children 1))
      (node-free idx old-root)))
  deleted?)

;; -- search, range scan & invariant verification --------------------------

(fn leaf-for [idx node-id key]
  (local n (node-load idx node-id))
  (if n.leaf n (leaf-for idx (. n.children (descend n key)) key)))

(fn search [idx key]
  "Return the {page slot} tid at key, or nil."
  (local leaf (leaf-for idx idx.meta.root key))
  (var found nil)
  (each [i k (ipairs leaf.keys)]
    (when (and (= found nil) (= k key))
      (set found (. leaf.vals i))))
  found)

(fn range [idx lo hi]
  "Collect [key tid] pairs with lo <= key <= hi, in key order."
  (local out [])
  (var leaf (leaf-for idx idx.meta.root lo))
  (var stop false)
  (while (and (not stop) (not= leaf nil) (> (length leaf.keys) 0))
    (each [i k (ipairs leaf.keys)]
      (when (> k hi) (set stop true))
      (when (and (not stop) (>= k lo) (<= k hi))
        (table.insert out [k (. leaf.vals i)])))
    (if (and (not stop) leaf.leaf (> (or leaf.next 0) 0))
        (do
          (set leaf (node-load idx leaf.next))
          (when (not leaf.leaf) (set leaf nil)))
        (set leaf nil)))
  out)

(fn pairs-all [idx]
  (range idx math.mininteger math.maxinteger))

(fn verify [idx]
  "Verify all structural B+ tree invariants (occupancy, key bounds, depth,
leaf-chain equivalence, and live/free page disjointness)."
  (local live-pages {})
  (local dfs-keys [])
  (var leaf-depth nil)

  (fn walk [pid depth lo hi is-root]
    (when (. live-pages pid)
      (error (.. "btree-disk.verify: duplicate page " pid)))
    (tset live-pages pid true)
    (local n (node-load idx pid))
    (when (> (length n.keys) idx.cap)
      (error (.. "btree-disk.verify: node " pid " exceeds cap")))
    (when (not is-root)
      (local min-k (if n.leaf idx.min-leaf idx.min-internal))
      (when (< (length n.keys) min-k)
        (error (.. "btree-disk.verify: underflow on page " pid " ("
                   (length n.keys) " < " min-k ")"))))
    (for [i 1 (length n.keys)]
      (local k (. n.keys i))
      (when (and (> i 1) (<= k (. n.keys (- i 1))))
        (error (.. "btree-disk.verify: unsorted keys on page " pid)))
      (when (and lo (< k lo))
        (error (.. "btree-disk.verify: key " k " < lower bound " lo)))
      (when (and hi (>= k hi))
        (error (.. "btree-disk.verify: key " k " >= upper bound " hi))))
    (if n.leaf
        (do
          (if (= leaf-depth nil)
              (set leaf-depth depth)
              (when (not= leaf-depth depth)
                (error "btree-disk.verify: unbalanced leaf depth")))
          (each [_ k (ipairs n.keys)] (table.insert dfs-keys k)))
        (do
          (when (not= (length n.children) (+ (length n.keys) 1))
            (error (.. "btree-disk.verify: bad child count on page " pid)))
          (for [i 1 (length n.children)]
            (local child-lo (if (> i 1) (. n.keys (- i 1)) lo))
            (local child-hi (if (<= i (length n.keys)) (. n.keys i) hi))
            (walk (. n.children i) (+ depth 1) child-lo child-hi false)))))

  (walk idx.meta.root 0 nil nil true)
  (when (not= (length dfs-keys) idx.meta.count)
    (error (.. "btree-disk.verify: count mismatch " (length dfs-keys) " vs "
               idx.meta.count)))
  ;; Verify leaf-chain matches DFS order exactly.
  (local chain (pairs-all idx))
  (when (not= (length chain) (length dfs-keys))
    (error "btree-disk.verify: leaf chain length mismatch"))
  (for [i 1 (length dfs-keys)]
    (when (not= (. (. chain i) 1) (. dfs-keys i))
      (error "btree-disk.verify: leaf chain order mismatch")))
  ;; Verify free-page list is acyclic, disjoint from live pages, and matches free-count.
  (local free-seen {})
  (var fpid idx.meta.free-head)
  (var fcnt 0)
  (while (> fpid 0)
    (when (or (. live-pages fpid) (. free-seen fpid))
      (error (.. "btree-disk.verify: free list corruption at page " fpid)))
    (tset free-seen fpid true)
    (local s (bufmgr.get-page idx.buf idx.hf fpid))
    (local nxt (decode-free-next s.data))
    (bufmgr.release idx.buf s false)
    (set fpid nxt)
    (set fcnt (+ fcnt 1)))
  (when (not= fcnt idx.meta.free-count)
    (error "btree-disk.verify: free-count mismatch"))
  {:height (or leaf-depth 0) :keys (length dfs-keys) :free-pages fcnt})

{:CAP DEFAULT-CAP
 : open
 : flush
 : close
 : count
 : insert
 : delete
 : search
 : range
 : pairs-all
 : verify
 :encode-node-for-test encode-node
 :decode-node-for-test decode-node}
