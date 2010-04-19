VERSION=0.3

BUILD_DIR=$(PWD)/build
BUILD_CONFIG=$(BUILD_DIR)/config
BUILD_DIST=$(BUILD_DIR)/jdibug-$(VERSION)

EMACS22=d:/emacs-22.3/bin/emacs.exe
EMACS23=d:/emacs-23.1/bin/emacs.exe
EMACS_ARGS=-batch -q --no-site-file  -l $(EL_INIT)

CEDET_DIR=c:/Program Files/emacs-22.3/site-lisp/cedet-1.0beta3b
TEST_DIR = $(PWD)/test

EL_INIT=$(BUILD_CONFIG)/el_init.el
EL_TEST_INIT=$(BUILD_CONFIG)/el_test_init.el

BUILD=$(EMACS_ARGS) -l jdibug-build.el -f jdibug-build
TEST=$(EMACS_ARGS) -l $(EL_TEST_INIT) -l $(TEST_DIR)/smoke-tests.el



CYGWIN=[ "${OSTYPE}" = cygwin ]

all: build test dist

.PHONY: dist
dist: build
	if $(CYGWIN); then fixpath=cygpath -u; else fixpath=echo; fi
	tar cf $(BUILD_DIR)/jdibug_$(VERSION).tar `${fixpath} $(BUILD_DIST)`
	bzip2 $(BUILD_DIR)/jdibug_$(VERSION).tar

.PHONY: build
build: init
#	$(EMACS22) $(BUILD)
#	$(EMACS23) $(BUILD)
	emacs $(EMACS_ARGS) -f semantic-grammar-batch-build-packages .
	emacs $(BUILD)

.PHONY: test
test: init
	emacs $(TEST)

.PHONY: init
init:
	rm -rf $(BUILD_DIR)
	mkdir $(BUILD_DIR)
	mkdir $(BUILD_CONFIG)
	mkdir $(BUILD_DIST)

	@echo '(defconst jdibug-build-directory  "'$(BUILD_DIST)'")' > $(EL_INIT)
	@echo "(add-to-list 'load-path "'"'$(CEDET_DIR)/semantic'")' >> $(EL_INIT)
	@echo "(require 'semantic)" >> $(EL_INIT)
	@echo "(setq wisent-verbose-flag t)" >> $(EL_INIT)
	@echo ";; EOF" >> $(EL_INIT)

	@echo "(add-to-list 'load-path " '"'$(TEST_DIR)'")' > $(EL_TEST_INIT)
	@echo "(add-to-list 'load-path " '"'$(BUILD_DIST)'")' >> $(EL_TEST_INIT)
	@echo ";; EOF" >> $(EL_TEST_INIT)
# EOF

