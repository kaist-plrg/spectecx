# Typed Imp tutorial - prose build helpers.
#
# Everything except the prose build is a plain `spectecx impty ...` command
# (see README.md); this Makefile only wraps the splice -> asciidoctor pipeline.

SPECTECX ?= spectecx
SPEC      = impty.spectec

DOC_DIR   = documentation
DOC_BUILD = $(DOC_DIR)/_build

.PHONY: splice splice-html splice-pdf clean help

help:
	@echo "Prose build (everything else is a plain 'spectecx impty ...' command):"
	@echo "  make splice-html   render the prose document to documentation/impty.html"
	@echo "  make splice-pdf    render the prose document as documentation/impty.pdf"
	@echo "  make clean         remove the build dir and rendered output"
	@echo "Override the tool with SPECTECX=... (default: $(SPECTECX))."

# splice-html needs asciidoctor; splice-pdf needs asciidoctor-pdf.

splice:
	mkdir -p $(DOC_BUILD)
	$(SPECTECX) splice -i $(DOC_DIR) -o $(DOC_BUILD) \
	  --missing $(DOC_BUILD)/splice.missing $(SPEC)

splice-html: splice
	asciidoctor -q \
	  -a docinfo=shared -a docinfodir=$(abspath $(DOC_DIR)) \
	  -o $(DOC_DIR)/impty.html $(DOC_BUILD)/impty.adoc

splice-pdf: splice
	asciidoctor-pdf -q \
	  -a docinfo=shared -a docinfodir=$(abspath $(DOC_DIR)) \
	  -o $(DOC_DIR)/impty.pdf $(DOC_BUILD)/impty.adoc

clean:
	rm -rf $(DOC_BUILD) $(DOC_DIR)/impty.html $(DOC_DIR)/impty.pdf
