MAJOR_VERSION=0
MINOR_VERSION=7
VERSION=$(MAJOR_VERSION).$(MINOR_VERSION)

BUILD_DIR:=$(shell pwd)/build
BUILD_CONFIG=$(BUILD_DIR)/config
BUILD_NAME=jdibug-$(VERSION)
BUILD_DIST=$(BUILD_DIR)/$(BUILD_NAME)

EMACS_ARGS=-batch -q --no-site-file  -l $(EL_INIT)


# TODO: Better way to find jdee.
JDEE_DIR:=$(shell find $(HOME)/.emacs.d/ -type d -name "jdee-*")

TEST_DIR = ./test

EL_INIT=$(BUILD_CONFIG)/el_init.el
EL_TEST_INIT=$(BUILD_CONFIG)/el_test_init.el

BUILD=$(EMACS_ARGS) -l jdibug-build.el -f jdibug-build
SMOKE_TEST=$(EMACS_ARGS) -l $(EL_TEST_INIT) -l $(TEST_DIR)/smoke-tests.el -f ert-run-tests-batch-and-exit
JDE_TEST=$(EMACS_ARGS) -l $(EL_TEST_INIT) -l $(TEST_DIR)/jde-tests.el -f ert-run-tests-batch-and-exit

# Ubuntu uses dash, which does not seem to understand *.{a,b} as *.a and *.b
SHELL=/bin/bash

CYGWIN=[ "${OSTYPE}" = cygwin ]

all: build test dist

.PHONY: dist
dist: build doc melpa
	cd $(BUILD_DIR) && tar cvf $(BUILD_NAME).tar $(BUILD_NAME)
	bzip2 $(BUILD_DIR)/$(BUILD_NAME).tar


melpa: build doc
# MELPA wants everything in the repository, so we need to copy the files
# generated from the wy files to someplace under version control. Ugh.
	@echo Copying wy files
	cp $(BUILD_DIST)/*wy.el generated
	cp $(BUILD_DIST)/NEWS generated
	cp $(BUILD_DIST)/README.* generated
	cp $(BUILD_DIST)/*.info generated

.PHONY: doc
doc: init
	makeinfo --output=$(BUILD_DIR)/$(BUILD_NAME)/jdibug.info jdibug.texi
	makeinfo --html --no-split jdibug.texi
	mkdir $(BUILD_DIR)/$(BUILD_NAME)/images
	cp -r images/*.* $(BUILD_DIR)/$(BUILD_NAME)/images
	cp README.txt NEWS $(BUILD_DIR)/$(BUILD_NAME)

.PHONY: build
build: init
	rm -f wisent.output
	emacs $(EMACS_ARGS) -f semantic-grammar-batch-build-packages .
	cp *.wy *wy.el{,c} $(BUILD_DIST)
	emacs $(BUILD)

.PHONY: test
test: init build
	emacs $(SMOKE_TEST)

.PHONY: jde-test
jde-test: test
	emacs $(JDE_TEST)

.PHONY: init
init:
	rm -rf $(BUILD_DIR)
	mkdir $(BUILD_DIR)
	mkdir $(BUILD_CONFIG)
	mkdir $(BUILD_DIST)

	@echo '(defconst jdibug-build-directory  "'$(BUILD_DIST)'")' > $(EL_INIT)
	@echo "(require 'cl)" >> $(EL_INIT)
	@echo "(require 'semantic)" >> $(EL_INIT)
	@echo '(require (if (string-match "^1" semantic-version) '"'semantic-grammar 'semantic/grammar))" >> $(EL_INIT)
	@echo "(setq wisent-verbose-flag t)" >> $(EL_INIT)
	@echo '(defconst jdibug-release-major-version "'$(MAJOR_VERSION)'")' >> $(EL_INIT)
	@echo '(defconst jdibug-release-minor-version "'$(MINOR_VERSION)'")' >> $(EL_INIT)
	@echo '(message "load-path=%s" load-path)' >> $(EL_INIT)
	@echo ";; EOF" >> $(EL_INIT)

	@echo "(add-to-list 'load-path " '"'$(TEST_DIR)'")' > $(EL_TEST_INIT)
	@echo "(add-to-list 'load-path " '"'$(BUILD_DIST)'")' >> $(EL_TEST_INIT)
	@echo "(add-to-list 'load-path " '"'$(JDEE_DIR)'")' >> $(EL_TEST_INIT)
	@echo ";; EOF" >> $(EL_TEST_INIT)

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)
	rm -rf generated/*
# EOF

