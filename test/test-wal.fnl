;;; ARIES WAL and power-loss crash recovery tests.

(local t (require :harness))
(local wal (require :umbel.wal))
(local page (require :umbel.page))
(local schema (require :umbel.schema))
(local table-m (require :umbel.table))
(local heapfile (require :umbel.heapfile))

(local tmpdir (os.getenv :TMPDIR))
(local dir (if (and tmpdir (not= tmpdir "")) tmpdir :/tmp))
(local dbfile (.. dir :/umbel-test-wal.db))
(local walfile (.. dir :/umbel-test-wal.wal))

(local item-schema [{:name :id :type :uint64}
                    {:name :name :type :string :size 16}])

(fn cleanup []
  (os.remove dbfile)
  (os.remove walfile))

(fn scan-ids [tbl]
  (local ids [])
  (each [_ bytes (table-m.scan tbl)]
    (table.insert ids (. (schema.decode item-schema bytes) :id)))
  (table.sort ids)
  ids)

(t.deftest "wal: committed inserts survive crash without data flush (redo)"
  (fn []
    (cleanup)
    (local tbl (table-m.create dbfile item-schema {:wal walfile}))
    (for [i 1 5]
      (table-m.insert tbl
                      (schema.encode item-schema {:id i :name (.. :row- i)})))
    ;; Simulate crash: close raw handles without flushing dirty buffer pool.
    (wal.close tbl.wal)
    (heapfile.close tbl.hf)
    ;; Without recovery, the unflushed data pages on disk have 0 records.
    (local raw (table-m.open dbfile nil))
    (t.assert= 0 (table-m.count raw) "unflushed data lost before recovery")
    (table-m.close raw)
    ;; Reopening with WAL replays committed records via ARIES redo.
    (local recovered (table-m.open dbfile nil {:wal walfile}))
    (t.assert= 5 (table-m.count recovered) "all 5 committed records recovered")
    (local ids (scan-ids recovered))
    (t.assert= 1 (. ids 1) "first recovered row intact")
    (t.assert= 5 (. ids 5) "last recovered row intact")
    (table-m.close recovered)
    (cleanup)))

(t.deftest "wal: uncommitted transaction flushed before crash is undone"
  (fn []
    (cleanup)
    (local tbl (table-m.create dbfile item-schema {:wal walfile}))
    (table-m.insert tbl (schema.encode item-schema {:id 1 :name :committed-1}))
    (table-m.insert tbl (schema.encode item-schema {:id 2 :name :committed-2}))
    (table-m.checkpoint tbl)
    ;; Explicit transaction mutates + flushes dirty pages, then crashes!
    (local tx (table-m.begin-txn tbl))
    (table-m.insert tbl (schema.encode item-schema {:id 3 :name :ghost-3}) tx)
    (table-m.insert tbl (schema.encode item-schema {:id 4 :name :ghost-4}) tx)
    (table-m.flush tbl true)
    (wal.close tbl.wal)
    (heapfile.close tbl.hf)
    (local raw (table-m.open dbfile nil))
    (t.assert= 4 (table-m.count raw) "dirty uncommitted writes reached disk")
    (table-m.close raw)
    ;; ARIES recovery must undo the uncommitted transaction and write CLRs.
    (local recovered (table-m.open dbfile nil {:wal walfile}))
    (t.assert= 2 (table-m.count recovered) "uncommitted rows rolled back")
    (local ids (scan-ids recovered))
    (t.assert= 1 (. ids 1) "committed row 1 kept")
    (t.assert= 2 (. ids 2) "committed row 2 kept")
    (table-m.close recovered)
    (cleanup)))

(t.deftest "wal: interleaved txns on same page recover winner and undo loser"
  (fn []
    (cleanup)
    (local tbl (table-m.create dbfile item-schema {:wal walfile}))
    (local keep-tid
           (table-m.insert tbl (schema.encode item-schema {:id 10 :name :base})))
    (table-m.checkpoint tbl)
    ;; T1 and T2 interleave on the SAME data page (page 2):
    ;; T1 inserts id=11 and deletes id=10 (loser: never commits)
    ;; T2 inserts id=20 and id=21 (winner: commits!)
    (local t1 (table-m.begin-txn tbl))
    (local t2 (table-m.begin-txn tbl))
    (table-m.insert tbl (schema.encode item-schema {:id 11 :name :loser-ins})
                    t1)
    (table-m.insert tbl (schema.encode item-schema {:id 20 :name :winner-1}) t2)
    (table-m.delete tbl keep-tid t1)
    (table-m.insert tbl (schema.encode item-schema {:id 21 :name :winner-2}) t2)
    (table-m.commit-txn tbl t2)
    ;; Force dirty page to disk and crash with T1 still active!
    (table-m.flush tbl true)
    (wal.close tbl.wal)
    (heapfile.close tbl.hf)
    ;; ARIES must keep id=10 (T1's delete undone), id=20, id=21 (T2 committed),
    ;; and remove id=11 (T1's insert undone) — all on the same page!
    (local rec (table-m.open dbfile nil {:wal walfile}))
    (local ids (scan-ids rec))
    (t.assert= 3 (length ids) "3 surviving rows after interleaved recovery")
    (t.assert= 10 (. ids 1) "T1's uncommitted delete of id=10 restored")
    (t.assert= 20 (. ids 2) "T2's committed id=20 preserved")
    (t.assert= 21 (. ids 3) "T2's committed id=21 preserved")
    (table-m.close rec)
    (cleanup)))

(t.deftest "wal: crash during ARIES undo recovery is idempotent via CLRs"
  (fn []
    (cleanup)
    (local tbl (table-m.create dbfile item-schema {:wal walfile}))
    (table-m.insert tbl (schema.encode item-schema {:id 1 :name :keep}))
    (table-m.checkpoint tbl)
    (local tx (table-m.begin-txn tbl))
    (table-m.insert tbl (schema.encode item-schema {:id 2 :name :u1}) tx)
    (table-m.insert tbl (schema.encode item-schema {:id 3 :name :u2}) tx)
    (table-m.insert tbl (schema.encode item-schema {:id 4 :name :u3}) tx)
    (table-m.flush tbl true)
    (wal.close tbl.wal)
    (heapfile.close tbl.hf)
    ;; Pass 1: simulate crash after undoing only 1 of 3 uncommitted writes!
    (local hf1 (heapfile.open dbfile))
    (local s1 (wal.recover walfile hf1 {:crash-after-undo 1}))
    (heapfile.close hf1)
    (t.assert= 1 s1.undone "pass 1 crashed after 1 CLR")
    (t.assert= 1 s1.clrs "pass 1 wrote 1 CLR")
    ;; Pass 2: re-run recovery to completion; must follow undo_next_lsn and undo remaining 2.
    (local hf2 (heapfile.open dbfile))
    (local s2 (wal.recover walfile hf2))
    (heapfile.close hf2)
    (t.assert= 2 s2.undone "pass 2 undid only the remaining 2 writes")
    ;; Pass 3: already clean -> 0 redone, 0 undone.
    (local hf3 (heapfile.open dbfile))
    (local s3 (wal.recover walfile hf3))
    (heapfile.close hf3)
    (t.assert= 0 s3.undone "pass 3 has nothing left to undo")
    (local back (table-m.open dbfile nil {:wal walfile}))
    (local ids (scan-ids back))
    (t.assert= 1 (length ids)
               "only committed row survives double-crash recovery")
    (t.assert= 1 (. ids 1) "committed row intact")
    (table-m.close back)
    (cleanup)))

(t.deftest "wal: bufmgr eviction enforces WAL-before-data (flushed_lsn >= page_lsn)"
  (fn []
    (cleanup)
    ;; Tiny pool of 2 forces dirty eviction during an active uncommitted txn.
    (local tbl (table-m.create dbfile item-schema {:wal walfile :pool-size 2}))
    (local tx (table-m.begin-txn tbl))
    ;; Insert enough rows to spill across >3 pages (each record is 24 bytes + 8 slot = 32 bytes; ~127/page).
    (for [i 1 300]
      (table-m.insert tbl (schema.encode item-schema {:id i :name (.. :p i)})
                      tx))
    ;; Read page 2 straight from disk: it must have been evicted AND its pageLSN <= wal.flushed-lsn.
    (local disk-p2 (heapfile.read-page tbl.hf 2))
    (t.assert= true (> (page.get-lsn disk-p2) 0)
               "evicted page carries non-zero pageLSN")
    (t.assert= true (>= tbl.wal.flushed-lsn (page.get-lsn disk-p2))
               "WAL was fdatasynced before dirty page eviction")
    (table-m.abort-txn tbl tx)
    (t.assert= 0 (table-m.count tbl) "online rollback undid all 300 rows")
    (table-m.close tbl)
    (cleanup)))

(t.deftest "wal: checkpoint bounds replay and torn tail is ignored"
  (fn []
    (cleanup)
    (local tbl (table-m.create dbfile item-schema {:wal walfile}))
    (table-m.insert tbl (schema.encode item-schema {:id 1 :name :before-ckpt}))
    (table-m.close tbl)
    ;; fsyncs data then writes clean checkpoint
    ;; Append a torn/corrupt tail frame to simulate mid-write power loss.
    (local f (io.open walfile :ab))
    (f:write "UW\153\000\000\000trunc")
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
