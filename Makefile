FENNEL ?= fennel
FNLFMT ?= fnlfmt
CC ?= cc
LUA_CFLAGS ?= $(shell pkg-config --cflags lua5.4 2>/dev/null || echo -I/usr/include/lua5.4)
CFLAGS ?= -O2 -Wall -Wextra -Werror -fPIC
DIR ?= data
SYS_SO = src/umbel/sys.so
FNL_FILES = $(wildcard src/umbel/*.fnl test/*.fnl)

.PHONY: all sys test repl fmt clean

all: $(SYS_SO)

sys: $(SYS_SO)

$(SYS_SO): csrc/umbel_sys.c
	$(CC) $(CFLAGS) $(LUA_CFLAGS) -shared -o $@ $<

test: $(SYS_SO)
	LUA_CPATH="src/?.so;;" $(FENNEL) test/run.fnl

repl: $(SYS_SO)
	LUA_CPATH="src/?.so;;" $(FENNEL) --add-fennel-path "src/?.fnl;src/?/init.fnl" -e '((. (require :umbel.repl) :run) "$(DIR)")'

fmt:
	$(FNLFMT) --fix $(FNL_FILES)

clean:
	rm -rf $(DIR) $(SYS_SO) *.db *.idx *.wal
