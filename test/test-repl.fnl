;;; Query layer & REPL tests 

(local t (require :harness))
(local repl (require :umbel.repl))

(local tmpdir (os.getenv :TMPDIR))
(local base (if (and tmpdir (not= tmpdir "")) tmpdir :/tmp))
(local dbdir (.. base :/umbel-test-repl))

(os.execute (.. "rm -rf '" (dbdir:gsub "'" "'\\''") "'"))

(t.deftest "repl: tokenizer handles quotes and escapes"
  (fn []
    (local toks
           (repl.tokenize "INSERT payments 1 \"Expensive Tuna\" 1682331745 2000"))
    (t.assert= 6 (length toks) "six tokens")
    (t.assert= "Expensive Tuna" (. toks 4) "quoted string keeps spaces")
    (local empty (repl.tokenize "   -- just a comment"))
    (t.assert= 0 (length empty) "comment line yields no tokens")
    (t.assert-error (fn [] (repl.tokenize "INSERT t \"unclosed"))
                    "unterminated quote errors")))

(t.deftest "repl: create, insert 20 records, scan, find, range, schema"
  (fn []
    (local db (repl.open dbdir))
    (local out-create
           (repl.eval db
                      "CREATE payments id:uint64 name:string:24 time:uint64 total:int32"))
    (t.assert= true (not= nil (out-create:find "created payments"))
               "CREATE confirms table")
    (t.assert= "id:uint64 name:string:24 time:uint64 total:int32"
               (repl.eval db "SCHEMA payments") "SCHEMA prints columns")
    (for [i 1 20]
      (repl.eval db
                 (string.format "INSERT payments %d \"Item %d\" %d %d" (* i 10)
                                i (+ 1700000000 i) (* i 100))))
    (local found (repl.eval db "FIND payments 50"))
    (t.assert= true (not= nil (found:find "name=\"Item 5\""))
               "FIND hits B+ tree and decodes record")
    (t.assert= "not found" (repl.eval db "FIND payments 999")
               "absent key reports not found")
    (local ranged (repl.eval db "RANGE payments 30 60"))
    (var lines 0)
    (each [_ (ranged:gmatch "[^\n]+")] (set lines (+ lines 1)))
    (t.assert= 4 lines "RANGE [30, 60] returns 4 rows (30,40,50,60)")
    (local scanned (repl.eval db "SCAN payments 2 3"))
    (var slines 0)
    (each [_ (scanned:gmatch "[^\n]+")] (set slines (+ slines 1)))
    (t.assert= 3 slines "SCAN with start+count slices output")
    (t.assert-error (fn [] (repl.eval db "INSERT payments 50 \"Dup\" 1 1"))
                    "duplicate primary key rejected")
    (local (_ quit?) (repl.eval db :.quit))
    (t.assert= true quit? ".quit signals exit")))

(t.deftest "repl: reopen reloads schema, table, and index from disk"
  (fn []
    (local db (repl.open dbdir))
    (t.assert= "id:uint64 name:string:24 time:uint64 total:int32"
               (repl.eval db "SCHEMA payments") "schema reloaded after restart")
    (local found (repl.eval db "FIND payments 200"))
    (t.assert= true (not= nil (found:find "name=\"Item 20\""))
               "index and table survive restart")
    (local del (repl.eval db "DELETE payments 50"))
    (t.assert= true (not= nil (del:find :deleted)) "DELETE tombstones row")
    (t.assert= "not found" (repl.eval db "FIND payments 50")
               "deleted row hidden from FIND")
    (local ranged (repl.eval db "RANGE payments 30 60"))
    (var lines 0)
    (each [_ (ranged:gmatch "[^\n]+")] (set lines (+ lines 1)))
    (t.assert= 3 lines "deleted row hidden from RANGE")
    ;; Deleting also removes from disk B+ tree, allowing clean key reuse.
    (repl.eval db "INSERT payments 50 \"Reborn 5\" 1700000999 555")
    (local reborn (repl.eval db "FIND payments 50"))
    (t.assert= true (not= nil (reborn:find "name=\"Reborn 5\""))
               "deleted primary key can be re-inserted cleanly")
    (repl.close db)
    (os.execute (.. "rm -rf '" (dbdir:gsub "'" "'\\''") "'"))))
