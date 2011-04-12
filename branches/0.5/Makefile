MAJOR_VERSION=0
MINOR_VERSION=5
VERSION=$(MAJOR_VERSION).$(MINOR_VERSION)

#BUILD_DIR=$(PWD)/build
BUILD_DIR=./build
BUILD_CONFIG=$(BUILD_DIR)/config
BUILD_NAME=jdibug-$(VERSION)
BUILD_DIST=$(BUILD_DIR)/$(BUILD_NAME)

EMACS22=d:/emacs-22.3/bin/emacs.exe
EMACS23=d:/emacs-23.1/bin/emacs.exe
EMACS_ARGS=-batch -q --no-site-file  -l $(EL_INIT)

#CEDET_DIR=c:/Program Files/emacs-22.3/site-lisp/cedet-1.0beta3b
#CEDET_DIR=c:/Program Files/emacs-23.2/lisp/cedet
CEDET_DIR=~/cedet-1.0pre7
#CEDET_DIR=~/cedet-1.0beta3b


#CEDET_DIR=~/cedet-1.0pre3
TEST_DIR = ./test

EL_INIT=$(BUILD_CONFIG)/el_init.el
EL_TEST_INIT=$(BUILD_CONFIG)/el_test_init.el

BUILD=$(EMACS_ARGS) -l jdibug-build.el -f jdibug-build
SMOKE_TEST=$(EMACS_ARGS) -l $(EL_TEST_INIT) -l $(TEST_DIR)/smoke-tests.el
JDE_TEST=$(EMACS_ARGS) -l $(EL_TEST_INIT) -l $(TEST_DIR)/jde-tests.el



CYGWIN=[ "${OSTYPE}" = cygwin ]

all: build test dist

.PHONY: dist
dist: build doc
	cd $(BUILD_DIR) && tar cvf $(BUILD_NAME).tar $(BUILD_NAME)
	bzip2 $(BUILD_DIR)/$(BUILD_NAME).tar

.PHONY: doc
doc: init
	makeinfo --output=$(BUILD_DIR)/$(BUILD_NAME)/jdibug.info jdibug.texi
	makeinfo --html --no-split jdibug.texi
	mkdir $(BUILD_DIR)/$(BUILD_NAME)/images
	cp -r images/*.* $(BUILD_DIR)/$(BUILD_NAME)/images
	cp README.txt NEWS $(BUILD_DIR)/$(BUILD_NAME)

.PHONY: build
build: init
#	$(EMACS22) $(BUILD)
#	$(EMACS23) $(BUILD)
	rm -f wisent.output
	emacs $(EMACS_ARGS) -f semantic-grammar-batch-build-packages .
	cp *.wy *wy.el{,c} $(BUILD_DIST)
	emacs $(BUILD)

.PHONY: test
test: init build
	emacs $(SMOKE_TEST)

.PHONY: jde-test
jde-test: test
	emacs $(JDE-TEST)

.PHONY: init
init:
	rm -rf $(BUILD_DIR)
	mkdir $(BUILD_DIR)
	mkdir $(BUILD_CONFIG)
	mkdir $(BUILD_DIST)

	@echo '(defconst jdibug-build-directory  "'$(BUILD_DIST)'")' > $(EL_INIT)
	@echo "(require 'cl)" >> $(EL_INIT)
	@echo "(loop for dir in (file-expand-wildcards "'"'$(CEDET_DIR)'/*")' >> $(EL_INIT)
	@echo "     do (add-to-list 'load-path dir))" >> $(EL_INIT)
	@echo ';;(load-file "'$(CEDET_DIR)/cedet.el'")' >> $(EL_INIT)
	@echo "(add-to-list 'load-path "'"'$(CEDET_DIR)'")' >> $(EL_INIT)
	@echo '(load-file "'$(CEDET_DIR)/common/cedet.el'")' >> $(EL_INIT)
	@echo ";; (require 'cedet)" >> $(EL_INIT)
	@echo ';;(load-file (expand-file-name "'$(CEDET_DIR)/common/cedet.el'"))' >> $(EL_INIT)
	@echo ";; (add-to-list 'load-path "'"'$(CEDET_DIR)/semantic'")' >> $(EL_INIT)
	@echo "(require 'semantic)" >> $(EL_INIT)
	@echo "(require 'semantic-grammar)" >> $(EL_INIT)
	@echo "(setq wisent-verbose-flag t)" >> $(EL_INIT)
	@echo '(defconst jdibug-release-major-version "'$(MAJOR_VERSION)'")' >> $(EL_INIT)
	@echo '(defconst jdibug-release-minor-version "'$(MINOR_VERSION)'")' >> $(EL_INIT)
	@echo '(message "load-path=%s" load-path)' >> $(EL_INIT)
	@echo ";; EOF" >> $(EL_INIT)

	@echo "(add-to-list 'load-path " '"'$(TEST_DIR)'")' > $(EL_TEST_INIT)
	@echo "(add-to-list 'load-path " '"'$(BUILD_DIST)'")' >> $(EL_TEST_INIT)
	@echo "(setq elunit-verbose t)" >> $(EL_TEST_INIT)
	@echo ";; EOF" >> $(EL_TEST_INIT)
# EOF

