all :unittest

unittest:
	emacs -batch -Q -L . -l test/run-test.el -f ert-run-tests-batch-and-exit
