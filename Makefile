EMACS ?= emacs

all: check

check: compile test

compile:
	${EMACS} -Q --batch -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile cc-cedict.el

test: cedict_1_0_ts_utf-8_mdbg.txt
	${EMACS} -Q --batch -L . -l cc-cedict-tests -f ert-run-tests-batch-and-exit

cedict_1_0_ts_utf-8_mdbg.txt:
	wget "https://www.mdbg.net/chinese/export/cedict/cedict_1_0_ts_utf-8_mdbg.txt.gz"
	gunzip *.gz
