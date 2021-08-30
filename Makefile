MAIN ?= main

TEXDIR := doc
LHSSRC := $(shell find $(TEXDIR) -type f -and -name '*.lhs')
TEXSRC := $(shell find $(TEXDIR) -type f -and \( -name '*.tex' -or -name '*.bib' \))
TEXSRC := $(filter-out $(LHSSRC:.lhs=.tex),$(TEXSRC))

.PHONY: default
default: clean build view watch

.PHONY: build
build: $(TEXDIR)/$(MAIN).pdf

.PHONY: watch
watch: require-fswatch
	@fswatch -o $(TEXSRC) $(LHSSRC) | xargs -n1 -I{} make build

.PHONY: clean
clean: require-latexmk
	@cd $(TEXDIR) && latexmk -f -C $(MAIN)
	@rm -f $(LHSSRC:.lhs=.tex)

.PHONY: view
view:
	@cd $(TEXDIR) && open -a Skim $(MAIN).pdf

$(TEXDIR)/%.tex: $(TEXDIR)/%.lhs require-lhs2TeX
	@cd $(TEXDIR) && lhs2TeX $(<:$(TEXDIR)/%=%) -o $(@:$(TEXDIR)/%=%)

$(TEXDIR)/$(MAIN).pdf: $(TEXSRC) $(LHSSRC:.lhs=.tex) require-latexmk
	@cd $(TEXDIR) && latexmk -pdf $(MAIN) -halt-on-error


################################################################################
# Generate conference artifact
################################################################################

artifact.tar.gz:
	tar -czvf artifact.tar.gz	\
		LICENSE									\
		README.md								\
		README.pdf							\
		src/										\
		test/										\
		priority-sesh.cabal			\
		stack.yaml


################################################################################
# Generate ArXiv package
################################################################################

paper-src-arxiv.zip: clean build
	cd doc && zip ../paper-src-arxiv.zip	\
		00README.XXX												\
	  ACM-Reference-Format.bst						\
		*.tex																\
		preamble/*.tex											\
		*.bib																\
		*.bbl

################################################################################
# Generate paper submission package
################################################################################

paper-src.zip: clean build
	cd doc && zip ../paper-src.zip	\
	  acmart.cls										\
	  ACM-Reference-Format.bst			\
		*.tex													\
		preamble/*.tex								\
		*.bib													\
		*.bbl

################################################################################
# Dependencies with readable error messages
################################################################################

.PHONY: require-lhs2TeX
require-lhs2TeX:
ifeq (,$(wildcard $(shell which lhs2TeX)))
	@echo "The command you called requires lhs2TeX"
	@echo "See: https://www.andres-loeh.de/lhs2tex/"
	@exit 1
endif

.PHONY: require-fswatch
require-fswatch:
ifeq (,$(wildcard $(shell which fswatch)))
	@echo "The command you called requires fswatch"
	@echo "See: https://emcrisostomo.github.io/fswatch/"
	@exit 1
endif

.PHONY: require-latexmk
require-latexmk:
ifeq (,$(wildcard $(shell which latexmk)))
	@echo "The command you called requires latexmk"
	@echo "See: https://mg.readthedocs.io/latexmk.html"
	@exit 1
endif
