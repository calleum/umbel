FENNEL ?= fennel
DIR ?= data

.PHONY: test repl clean

test:
	$(FENNEL) test/run.fnl

repl:
	$(FENNEL) --add-fennel-path "src/?.fnl;src/?/init.fnl" -e '((. (require :umbel.repl) :run) "$(DIR)")'

clean:
	rm -rf $(DIR) *.db *.idx *.wal
