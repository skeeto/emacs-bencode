.POSIX:
EMACS = emacs

compile: bencode.elc bencode-test.elc

bencode.elc: bencode.el
bencode-test.elc: bencode-test.el bencode.elc

clean:
	rm -f bencode.elc bencode-test.elc

check: bencode-test.elc
	$(EMACS) -batch -Q -L . -l bencode-test.elc -f ert-run-tests-batch

bench: bencode-test.elc
	$(EMACS) -batch -Q -L . -l bencode-test.elc -f bencode-benchmark

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<
