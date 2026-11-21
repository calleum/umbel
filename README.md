# UMBEL

*Just because you can, doesn't mean you should.*

A small page-oriented storage engine 

## Usage

Requires `fennel` (1.x on Lua 5.4).

```sh
make test
make repl            # opens REPL in ./data
make repl DIR=/tmp/u 
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
