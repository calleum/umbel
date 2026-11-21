# UMBEL

*Just because you can, doesn't mean you should.*

A page-oriented storage engine written in idiomatic Fennel (on Lua 5.4), implementing the full 5-phase Mosken roadmap plus power-loss durability, concurrent transactions, and churn-safe B+ trees. Named after the *umbel* — the umbrella-like flower cluster of a fennel plant.

## Modules

- [`csrc/umbel_sys.c`](csrc/umbel_sys.c) — thin POSIX C bridge for Lua 5.4 (`fsync`, `fdatasync`, `sync-dir`, `flock`)
- [`src/umbel/page.fnl`](src/umbel/page.fnl) — 4 KiB slotted pages with 64-bit `pageLSN` header (`add-item`, `get-item`, `delete-item`, `compact`, `restore-slot`)
- [`src/umbel/heapfile.fnl`](src/umbel/heapfile.fnl) — multi-page binary file I/O with directory/file `fsync` and POSIX `flock`
- [`src/umbel/pagedir.fnl`](src/umbel/pagedir.fnl) — single-page free-space directory (rebuilt from recovered pages after crash)
- [`src/umbel/bufmgr.fnl`](src/umbel/bufmgr.fnl) — fixed-size buffer pool with clock-sweep eviction and WAL-before-data hook (`flushed_lsn >= page_lsn`)
- [`src/umbel/lockmgr.fnl`](src/umbel/lockmgr.fnl) — Strict 2PL lock manager with S/X locks, upgrades, wait-for deadlock detection, and cooperative concurrency
- [`src/umbel/schema.fnl`](src/umbel/schema.fnl) — fixed-layout record codec (`int32`, `uint32`, `int64`, `uint64`, `string`) and page-0 schema catalog
- [`src/umbel/table.fnl`](src/umbel/table.fnl) — table layer tying catalog, page directory, buffer pool, Strict 2PL, and ARIES WAL together
- [`src/umbel/btree.fnl`](src/umbel/btree.fnl) — in-memory B+ tree with O(log N) borrow/merge/collapse delete
- [`src/umbel/btree-disk.fnl`](src/umbel/btree-disk.fnl) — disk-resident B+ tree with sibling borrow, merge, root collapse, on-disk free-page reclamation, and structural `verify`
- [`src/umbel/wal.fnl`](src/umbel/wal.fnl) — `fdatasync`-backed ARIES WAL (LSN, `prev_lsn`, physiological page ops, CLRs, fuzzy checkpoints, 3-phase crash recovery)
- [`src/umbel/repl.fnl`](src/umbel/repl.fnl) — interactive query REPL (`CREATE`, `INSERT`, `SCAN`, `FIND`, `RANGE`, `DELETE`, `SCHEMA`)

## Usage

Requires `fennel` (1.x on Lua 5.4), a C compiler + Lua 5.4 headers (for `src/umbel/sys.so`), and `fnlfmt` (for `make fmt`).

```sh
make test            # builds src/umbel/sys.so and runs full test suite
make fmt             # formats all Fennel sources with fnlfmt
make repl            # opens REPL in ./data
make repl DIR=/tmp/u # custom data directory
```

Example session:

```text
umbel> CREATE payments id:uint64 name:string:24 time:uint64 total:int32
umbel> INSERT payments 1 "Expensive Tuna" 1682331745 2000
umbel> FIND payments 1
umbel> RANGE payments 1 10
umbel> SCAN payments 0 10
umbel> SCHEMA payments
umbel> .quit
```
