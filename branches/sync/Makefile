EMACS22=d:/emacs-22.3/bin/emacs.exe
EMACS23=d:/emacs-23.1/bin/emacs.exe
BUILD=-batch -q -l jdibug-build.el -f jdibug-build .
TEST=-batch -q -l test/smoke-tests.el

all: test
	rm -rf jdibug.el
#	$(EMACS22) $(BUILD)
#	$(EMACS23) $(BUILD)
	emacs $(BUILD)

test:
	emacs $(TEST)

# EOF

