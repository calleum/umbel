;;; Write-ahead log and crash recovery tests — roadmap tasks 5.1 & 5.2.

(local t (require :harness))
(local wal (require :umbel.wal))
(local schema (require :umbel.schema))
(local table-m (require :umbel.table))
(local heapfile (require :umbel.heapfile))

(local tmpdir (os.getenv "TMPDIR"))
(local dir (if (and tmpdir (not= tmpdir "")) tmpdir "/tmp"))
(local dbfile (.. dir "/umbel-test-wal.db"))
(local walfile (.. dir "/umbel-test-wal.wal"))

(local item-schema
  [{:name "id" :type :uint64}
   {:name "name" :type :string :size 16}])

(fn cleanup []
  (os.remove dbfile)
  (os.remove walfile))

(t.deftest "wal: committed inserts survive crash without data flush (redo)"
  (fn []
    (cleanup)
    (local tbl (table-m.create dbfile item-schema {:wal walfile}))
    (for [i 1 5]
      (table-m.insert tbl (schema.encode item-schema {:id i :name (.. "row-" i)})))
    ;; Simulate crash: close raw handles without flushing dirty buffer pool.
    (wal.close tbl.wal)
    (heapfile.close tbl.hf)
    ;; Without recovery, the unflushed data pages on disk have 0 records.
    (local raw (table-m.open dbfile nil))
    (t.assert= 0 (table-m.count raw) "unflushed data lost before recovery")
    (table-m.close raw)
    ;; Reopening with WAL replays committed records.
    (local recovered (table-m.open dbfile nil {:wal walfile}))
    (t.assert= 5 (table-m.count recovered) "all 5 committed records recovered")
    (local names [])
    (each [_ bytes (table-m.scan recovered)]
      (table.insert names (. (schema.decode item-schema bytes) :name)))
    (t.assert= "row-1" (. names 1) "first recovered row intact")
    (t.assert= "row-5" (. names 5) "last recovered row intact")
    (table-m.close recovered)
    (cleanup)))

(t.deftest "wal: uncommitted transaction flushed before crash is undone"
  (fn []
    (cleanup)
    (local tbl (table-m.create dbfile item-schema {:wal walfile}))
    (table-m.insert tbl (schema.encode item-schema {:id 1 :name "committed-1"}))
    (table-m.insert tbl (schema.encode item-schema {:id 2 :name "committed-2"}))
    (table-m.flush tbl)
    (wal.checkpoint tbl.wal)
    ;; Start an explicit transaction, mutate + flush dirty pages, then crash!
    (table-m.begin-txn tbl)
    (table-m.insert tbl (schema.encode item-schema {:id 3 :name "ghost-3"}))
    (table-m.insert tbl (schema.encode item-schema {:id 4 :name "ghost-4"}))
    (table-m.flush tbl)
    (wal.close tbl.wal)
    (heapfile.close tbl.hf)
    ;; Verify uncommitted rows really hit disk before recovery.
    (local raw (table-m.open dbfile nil))
    (t.assert= 4 (table-m.count raw) "dirty uncommitted writes reached disk")
    (table-m.close raw)
    ;; Recovery must undo the uncommitted transaction.
    (local recovered (table-m.open dbfile nil {:wal walfile}))
    (t.assert= 2 (table-m.count recovered) "uncommitted rows rolled back")
    (local ids [])
    (each [_ bytes (table-m.scan recovered)]
      (table.insert ids (. (schema.decode item-schema bytes) :id)))
    (t.assert= 1 (. ids 1) "committed row 1 kept")
    (t.assert= 2 (. ids 2) "committed row 2 kept")
    (table-m.close recovered)
    (cleanup)))

(t.deftest "wal: checkpoint bounds replay and torn tail is ignored"
  (fn []
    (cleanup)
    (local tbl (table-m.create dbfile item-schema {:wal walfile}))
    (table-m.insert tbl (schema.encode item-schema {:id 1 :name "before-ckpt"}))
    (table-m.close tbl) ;; writes clean checkpoint
    ;; Append a torn/corrupt tail frame to simulate mid-write power loss.
    (local f (io.open walfile "ab"))
    (f:write "UW\153\0\0\0trunc")
    (f:close)
    (local hf (heapfile.open dbfile))
    (local stats (wal.recover walfile hf))
    (heapfile.close hf)
    (t.assert= 0 stats.redone "nothing replayed past clean checkpoint")
    (t.assert= 0 stats.undone "nothing undone past clean checkpoint")
    (local back (table-m.open dbfile nil {:wal walfile}))
    (t.assert= 1 (table-m.count back) "pre-checkpoint row intact")
    (table-m.close back)
    (cleanup)))
