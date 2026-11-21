;;; umbel.btree-disk — disk-resident B+ tree over the buffer manager.
;;
;; file, paged through a buffer pool. Layout:
;;
;;   page 0    meta: "MBT1" | root u32 | first-leaf u32 | count u64
;;   page 1+   nodes:
;;     byte 1        flags u8 (1 = leaf)
;;     bytes 2..3    nkeys u16
;;     bytes 4..7    next-leaf u32 (leaves; 0 = end of chain)
;;     byte 13..     keys: i64 x CAP
;;     then          leaf values (page u32, slot u32) x CAP
;;                    or internal children u32 x (CAP + 1)
;;
;; Same preemptive-split algorithm as the in-memory tree: full children
;; split on the way down, leaf separators are copied up, internal ones
;; moved up. The index is insert-only at this rung — deleting rows from
;; the table does not edit the tree (documented limitation).

(local page (require :umbel.page))
(local heapfile (require :umbel.heapfile))
(local bufmgr (require :umbel.bufmgr))

(local MAGIC :UBT1)
(local CAP 250)
(local HALF (// CAP 2))

; split point: sep = keys[HALF]
;; 1-based Lua string offsets: 12-byte header, then keys, then values.
(local KEYS-OFF 13)
(local VALS-OFF (+ KEYS-OFF (* CAP 8)))

(local pack string.pack)
(local unpack string.unpack)

;; -- node codec -----------------------------------------------------------

(fn encode-node [n]
  "Serialize a node table into its page image."
  ;; fixed 12-byte header: flags u8 | nkeys u16 | next u32 | 5 pad bytes
  (local head (.. (pack :<BI2I4 (if n.leaf 1 0) (length n.keys)
                        (if n.leaf (or n.next 0) 0))
                  (string.rep "\000" 5)))
  (var keys "")
  (each [_ k (ipairs n.keys)]
    (set keys (.. keys (pack :<i8 k))))
  (var tail "")
  (if n.leaf
      (each [_ v (ipairs n.vals)]
        (set tail (.. tail (pack :<I4I4 v.page v.slot))))
      (each [_ c (ipairs n.children)]
        (set tail (.. tail (pack :<I4 c)))))
  ;; keys region is CAP*8 wide; tail follows it, page padded to BLKSZ
  (.. head keys (string.rep "\000" (- (* CAP 8) (length keys))) tail
      (string.rep "\000" (- page.BLKSZ 12 (* CAP 8) (length tail)))))

(fn decode-node [data]
  "Parse a page image back into a node table."
  (local (flags nkeys next) (unpack :<BI2I4 data 1))
  (local n {:leaf (= flags 1) :keys []})
  (when n.leaf (set n.next next))
  (for [i 1 nkeys]
    (let [(k) (unpack :<i8 data (+ KEYS-OFF (* (- i 1) 8)))]
      (table.insert n.keys k)))
  (if n.leaf
      (do
        (set n.vals [])
        (for [i 1 nkeys]
          (let [(pg slot) (unpack :<I4I4 data (+ VALS-OFF (* (- i 1) 8)))]
            (table.insert n.vals {:page pg : slot}))))
      (do
        (set n.children [])
        (for [i 1 (+ nkeys 1)]
          (let [(cid) (unpack :<I4 data (+ VALS-OFF (* (- i 1) 4)))]
            (table.insert n.children cid)))))
  n)

;; -- meta page ------------------------------------------------------------

(fn encode-meta [m]
  (.. MAGIC (pack :<I4I4i8 m.root m.first-leaf m.count)
      (string.rep "\000" (- page.BLKSZ 20))))

(fn decode-meta [data]
  (when (not= (data:sub 1 4) MAGIC)
    (error "btree-disk: not an umbel index file"))
  (let [(root first-leaf count) (unpack :<I4I4i8 data 5)]
    {: root : first-leaf : count}))

;; -- engine ---------------------------------------------------------------

(fn file-exists? [path]
  (let [f (io.open path :r)]
    (if f (do
            (f:close) true) false)))

(fn open [path]
  "Open or create the index file at path."
  (local existed (file-exists? path))
  (local hf (heapfile.open path))
  (local buf (bufmgr.new 16))
  (local meta (if existed
                  (decode-meta (heapfile.read-page hf 0))
                  (let [m {:root 1 :first-leaf 1 :count 0}]
                    (heapfile.extend hf (encode-meta m))
                    (heapfile.extend hf
                                     (encode-node {:leaf true
                                                   :keys []
                                                   :vals []
                                                   :next 0}))
                    m)))
  {: hf : buf : meta : path})

(fn node-load [idx node-id]
  (local s (bufmgr.get-page idx.buf idx.hf node-id))
  (local n (decode-node s.data))
  (bufmgr.release idx.buf s false)
  n)

(fn node-save [idx node-id n]
  (local s (bufmgr.get-page idx.buf idx.hf node-id))
  (set s.data (encode-node n))
  (bufmgr.release idx.buf s true))

(fn node-alloc [idx n]
  (heapfile.extend idx.hf (encode-node n)))

(fn save-meta [idx]
  (heapfile.write-page idx.hf 0 (encode-meta idx.meta)))

(fn close [idx]
  "Persist meta and all dirty nodes, then close."
  (save-meta idx)
  (bufmgr.flush-all idx.buf idx.hf)
  (heapfile.close idx.hf))

(fn count [idx]
  idx.meta.count)

(fn full? [n]
  (>= (length n.keys) CAP))

(fn take [arr from to]
  (local out [])
  (for [k from to] (table.insert out (. arr k)))
  out)

(fn split-child [idx parent i]
  "Split the full child parent.children[i] in place (caller saves parent)."
  (local left-id (. parent.children i))
  (local child (node-load idx left-id))
  (local sep (. child.keys HALF))
  (local right (if child.leaf
                   {:leaf true :keys [] :vals [] :next child.next}
                   {:leaf false :keys [] :children []}))
  (if child.leaf
      (do
        ;; copy-up: the separator stays in the right leaf too
        (set right.keys (take child.keys HALF CAP))
        (set right.vals (take child.vals HALF CAP))
        (set child.keys (take child.keys 1 (- HALF 1)))
        (set child.vals (take child.vals 1 (- HALF 1))))
      (do
        ;; move-up: the separator leaves this level
        (set right.keys (take child.keys (+ HALF 1) CAP))
        (set right.children (take child.children (+ HALF 1) (+ CAP 1)))
        (set child.keys (take child.keys 1 (- HALF 1)))
        (set child.children (take child.children 1 HALF))))
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
        (when (full? (node-load idx (. n.children i)))
          (split-child idx n i)
          (node-save idx node-id n)
          (when (>= key (. n.keys i)) (set i (+ i 1))))
        (insert-nonfull idx (. n.children i) key tid))))

(fn insert [idx key tid]
  "Insert key -> {page slot} TupleId."
  (when (full? (node-load idx idx.meta.root))
    (local new-root {:leaf false :keys [] :children [idx.meta.root]})
    (split-child idx new-root 1)
    (set idx.meta.root (node-alloc idx new-root)))
  (insert-nonfull idx idx.meta.root key tid)
  (set idx.meta.count (+ idx.meta.count 1)))

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

{: CAP
 : open
 : close
 : count
 : insert
 : search
 : range
 : pairs-all
 :encode-node-for-test encode-node
 :decode-node-for-test decode-node}
