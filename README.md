# umbel

A small page-oriented storage engine written in idiomatic Fennel (on Lua 5.4), implementing the full 5-phase Mosken roadmap. Named after the *umbel* — the umbrella-like flower cluster of a fennel plant.

## Modules

- [`src/umbel/page.fnl`](src/umbel/page.fnl) — 4 KiB slotted pages (`add-item`, `get-item`, `delete-item`, `compact`)
- [`src/umbel/heapfile.fnl`](src/umbel/heapfile.fnl) — multi-page binary file I/O
- [`src/umbel/pagedir.fnl`](src/umbel/pagedir.fnl) — single-page free-space directory
- [`src/umbel/bufmgr.fnl`](src/umbel/bufmgr.fnl) — fixed-size buffer pool with clock-sweep eviction
- [`src/umbel/schema.fnl`](src/umbel/schema.fnl) — fixed-layout record codec (`int32`, `uint32`, `int64`, `uint64`, `string`) and page-0 schema catalog
- [`src/umbel/table.fnl`](src/umbel/table.fnl) — table layer tying catalog, page directory, buffer pool, and WAL together
- [`src/umbel/btree.fnl`](src/umbel/btree.fnl) — in-memory B+ tree (insert, search, range scan, delete)
- [`src/umbel/btree-disk.fnl`](src/umbel/btree-disk.fnl) — disk-resident B+ tree paged through the buffer pool
- [`src/umbel/wal.fnl`](src/umbel/wal.fnl) — checksummed write-ahead log with redo/undo crash recovery and checkpoints
- [`src/umbel/repl.fnl`](src/umbel/repl.fnl) — interactive query REPL (`CREATE`, `INSERT`, `SCAN`, `FIND`, `RANGE`, `DELETE`, `SCHEMA`)

## Usage

Requires `fennel` (1.x on Lua 5.4).

```sh
make test
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
