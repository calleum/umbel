;;; Concurrency tests — Strict 2PL, deadlocks, online rollback, POSIX flock.

(local t (require :harness))
(local lockmgr (require :umbel.lockmgr))
(local schema (require :umbel.schema))
(local table-m (require :umbel.table))
(local heapfile (require :umbel.heapfile))

(local tmpdir (os.getenv :TMPDIR))
(local dir (if (and tmpdir (not= tmpdir "")) tmpdir :/tmp))
(local dbfile (.. dir :/umbel-test-conc.db))
(local walfile (.. dir :/umbel-test-conc.wal))

(local row-schema [{:name :id :type :uint64}
                   {:name :val :type :string :size 16}])

(fn cleanup []
  (os.remove dbfile)
  (os.remove walfile))

(t.deftest "concurrency: Strict 2PL blocks conflicting writer until commit"
  (fn []
    (cleanup)
    (local tbl (table-m.create dbfile row-schema {:wal walfile}))
    (local tid
           (table-m.insert tbl (schema.encode row-schema {:id 1 :val :init})))
    (local events [])
    (local results (lockmgr.run-concurrent tbl.lm
                                           [(fn []
                                              (local tx1
                                                     (table-m.begin-txn tbl))
                                              (table-m.read tbl tid tx1)
                                              ;; holds S-lock on tid
                                              (table.insert events :t1-read)
                                              (coroutine.yield)
                                              (table-m.delete tbl tid tx1)
                                              ;; upgrades S -> X while sole holder
                                              (table.insert events :t1-deleted)
                                              (coroutine.yield)
                                              (table-m.commit-txn tbl tx1)
                                              (table.insert events
                                                            :t1-committed)
                                              :ok-t1)
                                            (fn []
                                              (local tx2
                                                     (table-m.begin-txn tbl))
                                              (coroutine.yield)
                                              ;; let t1 acquire S-lock and upgrade to X-lock first
                                              (coroutine.yield)
                                              (table.insert events
                                                            :t2-wait-read)
                                              (local got
                                                     (table-m.read tbl tid tx2))
                                              ;; blocks while t1 holds X-lock!
                                              (table.insert events
                                                            (if got
                                                                :t2-saw-live
                                                                :t2-saw-deleted))
                                              (table-m.commit-txn tbl tx2)
                                              :ok-t2)]))
    (t.assert= true (. (. results 1) :ok) "t1 completed")
    (t.assert= true (. (. results 2) :ok) "t2 completed")
    ;; Verify t2 saw the post-commit deleted state only after t1 committed!
    (var saw-deleted false)
    (each [_ e (ipairs events)]
      (when (= e :t2-saw-deleted) (set saw-deleted true)))
    (t.assert= true saw-deleted
               "t2 observed t1's committed delete after release")
    (table-m.close tbl)
    (cleanup)))

(t.deftest "concurrency: wait-for graph detects deadlock and aborts victim cleanly"
  (fn []
    (cleanup)
    (local tbl (table-m.create dbfile row-schema {:wal walfile}))
    (local tid1 (table-m.insert tbl (schema.encode row-schema {:id 1 :val :a})))
    (local tid2 (table-m.insert tbl (schema.encode row-schema {:id 2 :val :b})))
    (var deadlocked 0)
    (var committed 0)
    (local results (lockmgr.run-concurrent tbl.lm
                                           [(fn []
                                              (local tx1
                                                     (table-m.begin-txn tbl))
                                              (local (ok err)
                                                     (pcall (fn []
                                                              (table-m.delete tbl
                                                                              tid1
                                                                              tx1)
                                                              ;; X-lock tid1
                                                              (coroutine.yield)
                                                              (table-m.delete tbl
                                                                              tid2
                                                                              tx1)
                                                              ;; wants X-lock tid2 -> deadlock!
                                                              (table-m.commit-txn tbl
                                                                                  tx1))))
                                              (if ok
                                                  (set committed
                                                       (+ committed 1))
                                                  (do
                                                    (when (string.find (tostring err)
                                                                       :deadlock)
                                                      (set deadlocked
                                                           (+ deadlocked 1)))
                                                    (table-m.abort-txn tbl tx1)))
                                              ok)
                                            (fn []
                                              (local tx2
                                                     (table-m.begin-txn tbl))
                                              (local (ok err)
                                                     (pcall (fn []
                                                              (table-m.delete tbl
                                                                              tid2
                                                                              tx2)
                                                              ;; X-lock tid2
                                                              (coroutine.yield)
                                                              (table-m.delete tbl
                                                                              tid1
                                                                              tx2)
                                                              ;; wants X-lock tid1 -> deadlock!
                                                              (table-m.commit-txn tbl
                                                                                  tx2))))
                                              (if ok
                                                  (set committed
                                                       (+ committed 1))
                                                  (do
                                                    (when (string.find (tostring err)
                                                                       :deadlock)
                                                      (set deadlocked
                                                           (+ deadlocked 1)))
                                                    (table-m.abort-txn tbl tx2)))
                                              ok)]))
    (t.assert= 1 deadlocked "exactly one transaction detected deadlock")
    (t.assert= 1 committed
               "the non-victim transaction committed after victim aborted")
    ;; Both rows deleted by the surviving transaction (victim's partial delete was rolled back).
    (t.assert= 0 (table-m.count tbl)
               "surviving txn finished both deletes cleanly")
    (table-m.close tbl)
    (cleanup)))

(t.deftest "concurrency: online abort-txn rolls back loser without touching winner"
  (fn []
    (cleanup)
    (local tbl (table-m.create dbfile row-schema {:wal walfile}))
    (local base
           (table-m.insert tbl (schema.encode row-schema {:id 1 :val :base})))
    (local t1 (table-m.begin-txn tbl))
    (local t2 (table-m.begin-txn tbl))
    (table-m.insert tbl (schema.encode row-schema {:id 2 :val :t1-abort}) t1)
    (table-m.insert tbl (schema.encode row-schema {:id 3 :val :t2-commit}) t2)
    (table-m.delete tbl base t1)
    (table-m.commit-txn tbl t2)
    (table-m.abort-txn tbl t1)
    (local ids [])
    (each [_ bytes (table-m.scan tbl)]
      (table.insert ids (. (schema.decode row-schema bytes) :id)))
    (table.sort ids)
    (t.assert= 2 (length ids)
               "base + t2 row survive; t1 insert undone & delete restored")
    (t.assert= 1 (. ids 1) "base row restored on abort")
    (t.assert= 3 (. ids 2) "t2 committed row intact")
    (table-m.close tbl)
    (cleanup)))

(t.deftest "concurrency: POSIX flock coordinates across OS processes"
  (fn []
    (cleanup)
    (local hf (heapfile.open dbfile))
    (heapfile.extend hf ((. (require :umbel.page) :page-init)))
    (t.assert= true (heapfile.lock hf :ex_nb) "parent acquires exclusive flock")
    ;; Spawn a real child Lua 5.4 process attempting non-blocking exclusive flock on same file.
    (local cmd (string.format "LUA_CPATH='src/?.so;;' lua5.4 -e 'local sys=require(\"umbel.sys\"); local f=io.open(%q,\"r+b\"); local ok,why=sys.flock(f,\"ex_nb\"); f:close(); os.exit((not ok and why==\"wouldblock\") and 0 or 1)'"
                              dbfile))
    (local (ok1) (os.execute cmd))
    (t.assert= true ok1
               "child process observed EWOULDBLOCK while parent holds flock")
    (t.assert= true (heapfile.lock hf :un) "parent releases flock")
    (local cmd2 (string.format "LUA_CPATH='src/?.so;;' lua5.4 -e 'local sys=require(\"umbel.sys\"); local f=io.open(%q,\"r+b\"); local ok=sys.flock(f,\"ex_nb\"); sys.flock(f,\"un\"); f:close(); os.exit(ok and 0 or 1)'"
                               dbfile))
    (local (ok2) (os.execute cmd2))
    (t.assert= true ok2 "child process acquired flock after parent unlocked")
    (heapfile.close hf)
    (cleanup)))
