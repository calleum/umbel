;;; Slotted page tests — mirrors the C/Rust page contracts.

(local t (require :harness))
(local page (require :umbel.page))

(t.deftest "page-init layout"
  (fn []
    (local p (page.page-init))
    (t.assert= page.BLKSZ (length p) "fresh page is BLKSZ bytes")
    (t.assert= (- page.BLKSZ page.HEADER-SIZE) (page.free-space p)
               "free space equals BLKSZ - header size")
    (t.assert= 0 (page.slot-count p) "fresh page has no slots")))

(t.deftest "add/get round trip"
  (fn []
    (local p (page.page-init))
    (local (p1 n1) (page.add-item p "hello"))
    (t.assert= 1 n1 "first item number is 1")
    (t.assert= "hello" (page.get-item p1 1) "item 1 round trips")
    (t.assert= (- (- page.BLKSZ page.HEADER-SIZE) 13) (page.free-space p1)
               "free space drops by len + slot size")
    (local (p2 n2) (page.add-item p1 "worlds"))
    (local (p3 n3) (page.add-item p2 "!"))
    (t.assert= 2 n2)
    (t.assert= 3 n3)
    (t.assert= "hello" (page.get-item p3 1) "item 1 survives later adds")
    (t.assert= "worlds" (page.get-item p3 2) "item 2 round trips")
    (t.assert= "!" (page.get-item p3 3) "item 3 round trips")))

(t.deftest "get-item bounds and binary safety"
  (fn []
    (local (p _) (page.add-item (page.page-init) "\0\1\255\0ab"))
    (t.assert= "\0\1\255\0ab" (page.get-item p 1) "binary bytes round trip")
    (t.assert-nil (page.get-item p 2) "absent item is nil")))

(t.deftest "delete keeps siblings readable"
  (fn []
    (var p (page.page-init))
    (each [_ s (ipairs ["one" "two" "three" "four" "five"])]
      (set p (page.add-item p s)))
    (set p (page.delete-item p 2))
    (t.assert-nil (page.get-item p 2) "deleted item reads as nil")
    (t.assert= "one" (page.get-item p 1) "item 1 survives delete")
    (t.assert= "three" (page.get-item p 3) "item 3 survives delete")
    (t.assert= "four" (page.get-item p 4) "item 4 survives delete")
    (t.assert= "five" (page.get-item p 5) "item 5 survives delete")
    (t.assert= 4 (page.live-count p) "live count drops after delete")))

(t.deftest "tombstone slot reuse"
  (fn []
    (var p (page.page-init))
    (each [_ s (ipairs ["aaaa" "bbbb" "cccc"])]
      (set p (page.add-item p s)))
    (set p (page.delete-item p 1))
    (local (p2 n) (page.add-item p "dddd"))
    (t.assert= 1 n "new item reuses the tombstone slot")
    (t.assert= "dddd" (page.get-item p2 1) "reused slot holds new bytes")
    (t.assert= 3 (page.slot-count p2) "slot array did not grow")
    (t.assert= "cccc" (page.get-item p2 3) "later items survive reuse")))

(t.deftest "compact reclaims dead space and keeps item numbers"
  (fn []
    (var p (page.page-init))
    (each [_ s (ipairs ["alpha" "beta" "gamma" "delta" "epsilon"])]
      (set p (page.add-item p s)))
    (set p (page.delete-item p 2))
    (set p (page.delete-item p 4))
    (local free-before (page.free-space p))
    (local c (page.compact p))
    (t.assert= "alpha" (page.get-item c 1) "item 1 survives compact")
    (t.assert-nil (page.get-item c 2) "tombstone stays a hole in numbering")
    (t.assert= "gamma" (page.get-item c 3) "item 3 survives compact")
    (t.assert= "epsilon" (page.get-item c 5) "item 5 survives compact")
    (t.assert= 3 (page.live-count c) "compact drops dead slots on rewrite")
    (t.assert= 5 (page.slot-count c) "compact keeps the slot array in place")
    (t.assert= (+ free-before 4 5)
               (page.free-space c) "compact reclaims dead data bytes")))

(t.deftest "page full is an error"
  (fn []
    (var p (page.page-init))
    ;; fill with 8-byte items: each consumes 16 bytes of the gap
    (var ok true)
    (while ok
      (let [(np err) (pcall page.add-item p "12345678")]
        (if np (set p err) (set ok false))))
    (t.assert= 8 (page.free-space p)
               "full page leaves an 8-byte gap that fits nothing")
    (t.assert-error (fn [] (page.add-item p "x")) "adding to a full page errors")))

(t.deftest "header helpers use LE u32"
  (fn []
    (local p (page.page-init))
    (t.assert= (string.pack "<I4I4" 8 4096) (p:sub 1 8) "header bytes are LE u32 pair")
    (t.assert= 8 (page.header p) "lower defaults to header size")
    (t.assert= 4096 (select 2 (page.header p)) "upper defaults to BLKSZ")))
