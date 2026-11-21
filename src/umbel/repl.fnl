;;; umbel.repl — minimal query layer and interactive REPL (roadmap task 4.2).
;;
;; Commands:
;;   CREATE <table> <col>:<type> ...   types: int32 uint32 int64 uint64 string:N
;;   INSERT <table> <val1> <val2> ...
;;   SCAN   <table> [<start> <count>]
;;   FIND   <table> <key>              B+ tree lookup on first integer column
;;   RANGE  <table> <lo> <hi>          B+ tree range scan
;;   DELETE <table> <key>              tombstone row by key
;;   SCHEMA <table>
;;   .tables | .help | .quit

(local schema-m (require :umbel.schema))
(local table-m (require :umbel.table))
(local btd (require :umbel.btree-disk))

(local HELP
  (table.concat
   ["Commands:"
    "  CREATE <table> <col>:<type> ...  (types: int32 uint32 int64 uint64 string:N)"
    "  INSERT <table> <val1> <val2> ..."
    "  SCAN   <table> [<start> <count>]"
    "  FIND   <table> <key>"
    "  RANGE  <table> <lo> <hi>"
    "  DELETE <table> <key>"
    "  SCHEMA <table>"
    "  .tables | .help | .quit"]
   "\n"))

(fn file-exists? [path]
  (let [f (io.open path "r")]
    (if f (do (f:close) true) false)))

(fn ensure-dir [dir]
  (local safe (.. "'" (dir:gsub "'" "'\\''") "'"))
  (os.execute (.. "mkdir -p " safe)))

(fn check-ident [s kind]
  (when (or (= s nil) (not (s:match "^[%w_%-]+$")))
    (error (.. "invalid " kind ": " (tostring s))))
  s)

(fn tokenize [line]
  "Split line on whitespace, respecting double-quoted strings."
  (local out [])
  (local len (length line))
  (var i 1)
  (while (<= i len)
    (let [c (line:sub i i)]
      (if (c:match "%s")
          (set i (+ i 1))
          (and (= c "-") (= (line:sub (+ i 1) (+ i 1)) "-"))
          (set i (+ len 1))
          (= c "\"")
          (do (set i (+ i 1))
              (local chars [])
              (var closed false)
              (while (and (<= i len) (not closed))
                (let [ch (line:sub i i)]
                  (if (= ch "\"")
                      (do (set closed true) (set i (+ i 1)))
                      (and (= ch "\\") (< i len))
                      (do (table.insert chars (line:sub (+ i 1) (+ i 1)))
                          (set i (+ i 2)))
                      (do (table.insert chars ch)
                          (set i (+ i 1))))))
              (when (not closed) (error "unterminated string literal"))
              (table.insert out (table.concat chars)))
          (do (local start i)
              (while (and (<= i len) (not (string.match (line:sub i i) "%s")))
                (set i (+ i 1)))
              (table.insert out (line:sub start (- i 1)))))))
  out)

(fn parse-col-spec [spec]
  "Parse col:type or col:string:N into a schema field table."
  (let [(name ty sz) (spec:match "^([^:]+):([^:]+):?(.*)$")]
    (when (or (= name nil) (= ty nil))
      (error (.. "bad column spec (want col:type): " spec)))
    (check-ident name "column name")
    (local kw (string.lower ty))
    (if (= kw "string")
        (let [n (math.tointeger sz)]
          (when (or (= n nil) (< n 1))
            (error (.. "string column needs positive size: " spec)))
          {:name name :type :string :size n})
        (do (when (not= sz "")
              (error (.. "unexpected size on fixed type: " spec)))
            (when (= (. schema-m.fixed-sizes kw) nil)
              (error (.. "unknown column type: " ty)))
            {:name name :type kw}))))

(fn key-field-of [sch]
  "First column is the B+ tree key when it is an integer type."
  (let [f (. sch 1)]
    (when (and f (not= f.type :string)) f)))

(fn open-db [dir]
  (ensure-dir dir)
  {:dir dir :tables {}})

(fn table-paths [db name]
  (values (.. db.dir "/" name ".db")
          (.. db.dir "/" name ".idx")))

(fn get-table [db name]
  (check-ident name "table name")
  (or (. db.tables name)
      (let [(db-path idx-path) (table-paths db name)]
        (when (not (file-exists? db-path))
          (error (.. "unknown table: " name)))
        (local tbl (table-m.open db-path nil))
        (local kf (key-field-of tbl.schema))
        (local idx (when kf (btd.open idx-path)))
        (local entry {:tbl tbl :idx idx :key-field kf})
        (tset db.tables name entry)
        entry)))

(fn close-db [db]
  (each [name entry (pairs db.tables)]
    (when entry.idx (btd.close entry.idx))
    (table-m.close entry.tbl)
    (tset db.tables name nil)))

(fn format-schema [sch]
  (table.concat
   (icollect [_ f (ipairs sch)]
     (if (= f.type :string)
         (.. f.name ":string:" f.size)
         (.. f.name ":" f.type)))
   " "))

(fn format-row [sch tid rec]
  (local cols
    (icollect [_ f (ipairs sch)]
      (let [v (. rec f.name)]
        (if (= f.type :string)
            (.. f.name "=" (string.format "%q" v))
            (.. f.name "=" (tostring v))))))
  (.. "[" tid.page ":" tid.slot "] " (table.concat cols " ")))

(fn parse-int [s label]
  (or (math.tointeger s)
      (error (.. label " must be an integer, got: " (tostring s)))))

;; -- command handlers -----------------------------------------------------

(fn cmd-create [db args]
  (local name (check-ident (. args 2) "table name"))
  (when (< (length args) 3)
    (error "CREATE needs at least one column spec"))
  (local (db-path idx-path) (table-paths db name))
  (when (or (. db.tables name) (file-exists? db-path))
    (error (.. "table already exists: " name)))
  (local sch [])
  (local seen {})
  (for [i 3 (length args)]
    (let [f (parse-col-spec (. args i))]
      (when (. seen f.name) (error (.. "duplicate column: " f.name)))
      (tset seen f.name true)
      (table.insert sch f)))
  (local tbl (table-m.create db-path sch))
  (local kf (key-field-of sch))
  (local idx (when kf (btd.open idx-path)))
  (tset db.tables name {:tbl tbl :idx idx :key-field kf})
  (.. "created " name " (" (format-schema sch) ")"))

(fn cmd-insert [db args]
  (local entry (get-table db (. args 2)))
  (local sch entry.tbl.schema)
  (when (not= (- (length args) 2) (length sch))
    (error (.. "INSERT expects " (length sch) " value(s), got " (- (length args) 2))))
  (local rec {})
  (each [i f (ipairs sch)]
    (let [raw (. args (+ i 2))]
      (tset rec f.name
            (if (= f.type :string) raw (parse-int raw f.name)))))
  (local bytes (schema-m.encode sch rec))
  ;; Reject duplicate key before mutating the heap table.
  (when entry.idx
    (let [k (. rec entry.key-field.name)
          existing (btd.search entry.idx k)]
      (when existing
        (error (.. "duplicate key " k)))))
  (local tid (table-m.insert entry.tbl bytes))
  (when entry.idx
    (btd.insert entry.idx (. rec entry.key-field.name) tid))
  (string.format "inserted [%d:%d]" tid.page tid.slot))

(fn cmd-scan [db args]
  (local entry (get-table db (. args 2)))
  (local start (if (. args 3) (parse-int (. args 3) "start") 0))
  (local limit (if (. args 4) (parse-int (. args 4) "count") math.maxinteger))
  (local rows [])
  (var seen 0)
  (each [tid bytes (table-m.scan entry.tbl)]
    (when (and (>= seen start) (< (length rows) limit))
      (table.insert rows (format-row entry.tbl.schema tid
                                     (schema-m.decode entry.tbl.schema bytes))))
    (set seen (+ seen 1)))
  (if (= (length rows) 0) "(empty)" (table.concat rows "\n")))

(fn cmd-find [db args]
  (local entry (get-table db (. args 2)))
  (when (= entry.idx nil) (error "table has no integer key index"))
  (local key (parse-int (. args 3) "key"))
  (local tid (btd.search entry.idx key))
  (let [bytes (when tid (table-m.read entry.tbl tid))]
    (if bytes
        (format-row entry.tbl.schema tid
                    (schema-m.decode entry.tbl.schema bytes))
        "not found")))

(fn cmd-range [db args]
  (local entry (get-table db (. args 2)))
  (when (= entry.idx nil) (error "table has no integer key index"))
  (local lo (parse-int (. args 3) "lo"))
  (local hi (parse-int (. args 4) "hi"))
  (local rows [])
  (each [_ pair (ipairs (btd.range entry.idx lo hi))]
    (let [tid (. pair 2)
          bytes (table-m.read entry.tbl tid)]
      (when bytes
        (table.insert rows (format-row entry.tbl.schema tid
                                       (schema-m.decode entry.tbl.schema bytes))))))
  (if (= (length rows) 0) "(empty)" (table.concat rows "\n")))

(fn cmd-delete [db args]
  (local entry (get-table db (. args 2)))
  (when (= entry.idx nil) (error "table has no integer key index"))
  (local key (parse-int (. args 3) "key"))
  (local tid (btd.search entry.idx key))
  (if (and tid (table-m.read entry.tbl tid))
      (do (table-m.delete entry.tbl tid)
          (string.format "deleted [%d:%d]" tid.page tid.slot))
      "not found"))

(fn cmd-schema [db args]
  (local entry (get-table db (. args 2)))
  (format-schema entry.tbl.schema))

(fn eval-line [db line]
  "Execute one REPL command line. Returns (output, quit?)."
  (local tokens (tokenize line))
  (if (= (length tokens) 0)
      (values nil false)
      (case (string.upper (. tokens 1))
        :CREATE (values (cmd-create db tokens) false)
        :INSERT (values (cmd-insert db tokens) false)
        :SCAN   (values (cmd-scan db tokens) false)
        :FIND   (values (cmd-find db tokens) false)
        :RANGE  (values (cmd-range db tokens) false)
        :DELETE (values (cmd-delete db tokens) false)
        :SCHEMA (values (cmd-schema db tokens) false)
        :.TABLES (let [names (icollect [k _ (pairs db.tables)] k)]
                   (table.sort names)
                   (values (if (= (length names) 0) "(none)"
                               (table.concat names " "))
                           false))
        :.HELP  (values HELP false)
        :.QUIT  (do (close-db db) (values "bye" true))
        :.EXIT  (do (close-db db) (values "bye" true))
        cmd     (error (.. "unknown command: " cmd)))))

(fn run [dir]
  "Run an interactive REPL backed by directory `dir`."
  (local db (open-db (or dir "data")))
  (print (.. "umbel — Fennel storage engine (dir: " db.dir ")"))
  (print "Type .help for commands, .quit to exit.")
  (var running true)
  (while running
    (io.write "umbel> ")
    (io.flush)
    (local line (io.read "*l"))
    (if (= line nil)
        (do (close-db db) (set running false))
        (let [(ok out quit) (pcall eval-line db line)]
          (if ok
              (do (when out (print out))
                  (when quit (set running false)))
              (print (.. "error: " out)))))))

{:tokenize tokenize :parse-col-spec parse-col-spec :open open-db :close close-db
 :eval eval-line :run run}
