FENNEL ?= fennel
CC ?= cc
LUA_CFLAGS ?= $(shell pkg-config --cflags lua5.4 2>/dev/null || echo -I/usr/include/lua5.4)
CFLAGS ?= -O2 -Wall -Wextra -Werror -fPIC
DIR ?= data
SYS_SO = src/umbel/sys.so

.PHONY: all sys test repl clean

all: $(SYS_SO)

sys: $(SYS_SO)

$(SYS_SO): csrc/umbel_sys.c
	$(CC) $(CFLAGS) $(LUA_CFLAGS) -shared -o $@ $<

test: $(SYS_SO)
	LUA_CPATH="src/?.so;;" $(FENNEL) test/run.fnl

repl: $(SYS_SO)
	LUA_CPATH="src/?.so;;" $(FENNEL) --add-fennel-path "src/?.fnl;src/?/init.fnl" -e '((. (require :umbel.repl) :run) "$(DIR)")'

clean:
	rm -rf $(DIR) $(SYS_SO) *.db *.idx *.wal
