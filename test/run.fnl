;;; Test runner — `fennel test/run.fnl` from the repo root.

(local fennel (require :fennel))
(set fennel.path "src/?.fnl;src/?/init.fnl;test/?.fnl")
(set package.cpath (.. "src/?.so;" package.cpath))
(table.insert (or package.searchers package.loaders) 2 fennel.searcher)

(local t (require :harness))

(print :page)
(require :test-page)
(print :storage)
(require :test-storage)
(print :table)
(require :test-table)
(print :btree)
(require :test-btree)
(print :btree-disk)
(require :test-btree-disk)
(print :repl)
(require :test-repl)
(print :wal)
(require :test-wal)
(print :concurrency)
(require :test-concurrency)
(print "")

(local code (t.summary))
(os.exit code)
