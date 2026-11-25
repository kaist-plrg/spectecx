NAME = spectecx

SWITCH ?= spectecx

OPAM_EXEC = opam exec --switch=$(SWITCH) --
DUNE = cd spectec && $(OPAM_EXEC) dune

# Compile & Format

.PHONY: exe lsp check fmt fmt-check promote clean

EXELSP = _build/default/spectec/bin/lsp_main.exe

exe:
	rm -f ./$(NAME)
	$(OPAM_EXEC) dune build --promote-install-files=false @install
	@echo
	@$(OPAM_EXEC) dune exec --no-build -- sh spectec/scripts/make-launcher.sh \
	  "$(SWITCH)" "$(abspath _build/default/spectec/bin/main.exe)" > ./$(NAME)
	chmod +x ./$(NAME)

lsp:
	rm -f ./$(NAME)-lsp
	$(DUNE) build bin/lsp_main.exe
	@echo
	ln -f $(EXELSP) ./$(NAME)-lsp

check:
	$(DUNE) build @check

fmt:
	$(DUNE) fmt

fmt-check:
	$(DUNE) build @fmt

promote:
	$(DUNE) promote

clean:
	rm -f ./$(NAME) ./$(NAME)-lsp
	$(DUNE) clean

# Splice and render. `splice-html` requires `asciidoctor`; `splice-pdf`
# requires `asciidoctor-pdf`.

SPLICE_INPUT = spectec/examples/splice
SPLICE_BUILD = spectec/examples/splice/_build
IMPTY_SPEC = spectec/specs/impty/base/spec.spectec

.PHONY: splice splice-html splice-pdf splice-clean

splice: exe
	mkdir -p $(SPLICE_BUILD)
	./$(NAME) splice -i $(SPLICE_INPUT) -o $(SPLICE_BUILD) \
	  --missing $(SPLICE_BUILD)/splice.missing $(IMPTY_SPEC)

splice-html: splice
	asciidoctor -q \
	  -a docinfo=shared -a docinfodir=$(abspath $(SPLICE_INPUT)) \
	  -o $(SPLICE_BUILD)/impty.html $(SPLICE_BUILD)/impty.adoc

splice-pdf: splice
	asciidoctor-pdf -q \
	  -a docinfo=shared -a docinfodir=$(abspath $(SPLICE_INPUT)) \
	  -o $(SPLICE_BUILD)/impty.pdf $(SPLICE_BUILD)/impty.adoc

splice-clean:
	rm -rf $(SPLICE_BUILD)

# VS Code extension: package editors/vscode into a sideloadable .vsix
# (install with `code --install-extension spectecx.vsix`).

.PHONY: vsix

vsix:
	cd editors/vscode && npm ci --include=dev --ignore-scripts && npm run package
	@echo "#### extension written to editors/vscode/$(NAME).vsix"

# Tests
#
# Individual tests:
#   make test-lang       - Shared language operations
#   make test-elab       - Elaboration test (both p4 and p4-old)
#   make test-algo       - Algo test (both p4 and p4-old)
#   make test-struct     - Structuring test (both p4 and p4-old)
#   make test-annotate   - Annotate/prose render test (impty x3 + p4-old)
#   make test-roundtrip-al - EL<->AL premise roundtrip test (impty base + closure, p4)
#   make test-roundtrip-el - EL pretty-printer roundtrip test (mini-spec, p4-old, p4, impty)
#   make test-parsegen   - Grammar-driven parser differential test (impty expressions + programs)
#   make test-package    - Package ownership and plugin discovery tests
#   make test-al-pos     - AL interpreter positive tests (slow)
#   make test-al-neg     - AL interpreter negative tests
#   make test-sl-pos     - SL interpreter positive tests (slow)
#   make test-sl-neg     - SL interpreter negative tests
#   make test-pl-pos     - PL interpreter positive tests (slow)
#   make test-pl-neg     - PL interpreter negative tests
#   make test-al-eval    - AL STF evaluation tests on eBPF programs
#   make test-sl-eval    - SL STF evaluation tests on eBPF programs
#   make test-pl-eval    - PL STF evaluation tests on eBPF programs
#
# Relation and per-case interpreter tests:
#   make test-interp-relation - Relation tests across AL/SL/PL
#   make test-interp-neg      - Per-case impty AL negative tests
#
# CLI tests (snapshots and the ./spectecx launcher):
#   make test-cli        - target CLI, instrumentation and launcher snapshots
#   make test-launcher   - ./spectecx concurrent runs and config lookup
#   make test-lsp        - LSP multi-file diagnostics
#
# Grouped tests:
#   make test-quick      - Fast tests, including P4 instantiation across AL/SL/PL
#   make test-interp     - Interpreter tests for p4, impty, and Mini-ML, including P4 instantiation
#   make test-al         - AL tests for p4 (pos + neg + eval)
#   make test-sl         - SL tests for p4 (pos + neg + eval)
#   make test-pl         - PL tests for p4 (pos + neg + eval)
#
# impty interpreter tests (per-variant: base, closure):
#   make test-impty-<v>-al-pos / -al-neg / -sl-pos / -sl-neg
#   make test-impty-<v>-al / -sl                     - per-variant pos+neg
#   make test-impty-<v>                              - per-variant al+sl
#   make test-impty                                  - all impty tests
#
# Mini-ML interpreter tests:
#   make test-miniml-al-pos / -al-neg / -sl-pos / -sl-neg / -pl-pos / -pl-neg
#   make test-miniml-al / -sl / -pl                  - per-mode pos+neg
#   make test-miniml                                  - all Mini-ML tests
#
#   make test            - quick + interpreter tests

.PHONY: test test-quick test-lang test-context test-interp test-elab test-algo test-elab-neg test-interp-relation test-interp-neg test-cli test-launcher test-lsp test-struct test-annotate test-roundtrip-al test-roundtrip-el test-parsegen test-package
.PHONY: test-al test-al-pos test-al-neg test-al-eval
.PHONY: test-sl test-sl-pos test-sl-neg test-sl-eval
.PHONY: test-pl test-pl-pos test-pl-neg test-pl-eval
.PHONY: test-impty test-impty-base test-impty-closure
.PHONY: test-impty-base-al test-impty-base-sl test-impty-base-pl
.PHONY: test-impty-closure-al test-impty-closure-sl test-impty-closure-pl
.PHONY: test-impty-base-al-pos test-impty-base-al-neg
.PHONY: test-impty-base-sl-pos test-impty-base-sl-neg
.PHONY: test-impty-base-pl-pos test-impty-base-pl-neg
.PHONY: test-impty-closure-al-pos test-impty-closure-al-neg
.PHONY: test-impty-closure-sl-pos test-impty-closure-sl-neg
.PHONY: test-miniml test-miniml-al test-miniml-sl test-miniml-pl
.PHONY: test-miniml-al-pos test-miniml-al-neg
.PHONY: test-miniml-sl-pos test-miniml-sl-neg
.PHONY: test-miniml-pl-pos test-miniml-pl-neg
.PHONY: promote

test-lang:
	@echo "#### Running language unit tests"
	@$(DUNE) build @test/lang/runtest --profile=release && echo OK

test-elab:
	@echo "#### Running elaboration test"
	@$(DUNE) build @test/elab/runtest --profile=release && echo OK

test-algo:
	@echo "#### Running algo test"
	@$(DUNE) build @test/algo/runtest --profile=release && echo OK

test-roundtrip-el:
	@echo "#### Running EL pretty-printer roundtrip test"
	@$(DUNE) build @test/roundtrip/el/runtest --profile=release && echo OK

test-elab-neg:
	@echo "#### Running elaboration negative tests"
	@$(DUNE) build @test/elab/neg/runtest --profile=release && echo OK

test-interp-relation:
	@echo "#### Running interpreter relation tests"
	@$(DUNE) build @test/interp/relation/runtest --profile=release && echo OK

test-context:
	@echo "#### Running interpreter context tests"
	@$(DUNE) build @test/interp/context/runtest --profile=release && echo OK

test-interp-neg:
	@echo "#### Running interpreter negative tests (per-case impty AL corpus)"
	@$(DUNE) build @test/interp/neg/runtest --profile=release && echo OK

test-cli:
	@echo "#### Running CLI snapshot tests"
	@$(DUNE) build @test/cli/runtest --profile=release && echo OK

test-launcher: exe
	@echo "#### Running launcher test"
	@sh spectec/test/launcher/main.sh ./$(NAME)

test-lsp:
	@echo "#### Running LSP tests"
	@$(DUNE) build @test/lsp/runtest --profile=release && echo OK

test-struct:
	@echo "#### Running structuring test"
	@$(DUNE) build @test/struct/runtest --profile=release && echo OK

test-annotate:
	@echo "#### Running annotate test"
	@$(DUNE) build @test/annotate/runtest --profile=release && echo OK

test-roundtrip-al:
	@echo "#### Running EL<->AL premise roundtrip test"
	@$(DUNE) build @test/roundtrip/al/runtest --profile=release && echo OK

test-parsegen:
	@echo "#### Running grammar-driven parser differential test"
	@$(DUNE) build @test/parsegen/runtest --profile=release && echo OK

test-package:
	@echo "#### Running package ownership and plugin discovery tests"
	@$(DUNE) build --promote-install-files=false @test/package/runtest --profile=release && echo OK

# $(1): target prefix, such as p4 or impty-base
# $(2): al / sl / pl
# $(3): pos / neg / eval
define run_interp_test
	@echo "#### Running $(2) interpreter $(3) tests ($(1))"
	@$(DUNE) build @test/interp/$(1)-$(2)-$(3) --profile=release
	@cat _build/default/spectec/test/interp/$(1)-$(2)-$(3).err >&2
	@echo OK
endef

test-al-pos:
	$(call run_interp_test,p4,al,pos)

test-al-neg:
	$(call run_interp_test,p4,al,neg)

test-sl-pos:
	$(call run_interp_test,p4,sl,pos)

test-sl-neg:
	$(call run_interp_test,p4,sl,neg)

test-pl-pos:
	$(call run_interp_test,p4,pl,pos)

test-pl-neg:
	$(call run_interp_test,p4,pl,neg)

test-al-eval:
	$(call run_interp_test,p4,al,eval)

test-sl-eval:
	$(call run_interp_test,p4,sl,eval)

test-pl-eval:
	$(call run_interp_test,p4,pl,eval)

test-quick: test-context test-lang test-elab test-algo test-elab-neg test-interp-relation test-interp-neg test-cli test-launcher test-lsp test-struct test-annotate test-roundtrip-al test-roundtrip-el test-impty test-miniml test-parsegen test-package
	@$(DUNE) build @test/interp/p4/runtest --profile=release
	@echo "#### Quick tests passed"

test-interp: test-interp-relation test-interp-neg test-al test-sl test-pl test-impty test-miniml
	@$(DUNE) build @test/interp/p4/runtest --profile=release
	@echo "#### Interpreter tests passed"

test-al: test-al-pos test-al-neg test-al-eval
	@echo "#### AL tests passed"

test-sl: test-sl-pos test-sl-neg test-sl-eval
	@echo "#### SL tests passed"

test-pl: test-pl-pos test-pl-neg test-pl-eval
	@echo "#### PL tests passed"

test-impty-base-al-pos:
	$(call run_interp_test,impty-base,al,pos)

test-impty-base-al-neg:
	$(call run_interp_test,impty-base,al,neg)

test-impty-base-sl-pos:
	$(call run_interp_test,impty-base,sl,pos)

test-impty-base-sl-neg:
	$(call run_interp_test,impty-base,sl,neg)

test-impty-base-pl-pos:
	$(call run_interp_test,impty-base,pl,pos)

test-impty-base-pl-neg:
	$(call run_interp_test,impty-base,pl,neg)

test-impty-closure-al-pos:
	$(call run_interp_test,impty-closure,al,pos)

test-impty-closure-al-neg:
	$(call run_interp_test,impty-closure,al,neg)

test-impty-closure-sl-pos:
	$(call run_interp_test,impty-closure,sl,pos)

test-impty-closure-sl-neg:
	$(call run_interp_test,impty-closure,sl,neg)

test-impty-closure-pl-pos:
	$(call run_interp_test,impty-closure,pl,pos)

test-impty-closure-pl-neg:
	$(call run_interp_test,impty-closure,pl,neg)

test-impty-base-al: test-impty-base-al-pos test-impty-base-al-neg
	@echo "#### AL (impty-base) tests passed"

test-impty-base-sl: test-impty-base-sl-pos test-impty-base-sl-neg
	@echo "#### SL (impty-base) tests passed"

test-impty-base-pl: test-impty-base-pl-pos test-impty-base-pl-neg
	@echo "#### PL (impty-base) tests passed"

test-impty-closure-al: test-impty-closure-al-pos test-impty-closure-al-neg
	@echo "#### AL (impty-closure) tests passed"

test-impty-closure-sl: test-impty-closure-sl-pos test-impty-closure-sl-neg
	@echo "#### SL (impty-closure) tests passed"

test-impty-closure-pl: test-impty-closure-pl-pos test-impty-closure-pl-neg
	@echo "#### PL (impty-closure) tests passed"

test-impty-base: test-impty-base-al test-impty-base-sl test-impty-base-pl
	@echo "#### impty-base interpreter tests passed"

test-impty-closure: test-impty-closure-al test-impty-closure-sl test-impty-closure-pl
	@echo "#### impty-closure interpreter tests passed"

test-impty: test-impty-base test-impty-closure
	@echo "#### impty interpreter tests passed"

test-miniml-al-pos:
	$(call run_interp_test,miniml,al,pos)

test-miniml-al-neg:
	$(call run_interp_test,miniml,al,neg)

test-miniml-sl-pos:
	$(call run_interp_test,miniml,sl,pos)

test-miniml-sl-neg:
	$(call run_interp_test,miniml,sl,neg)

test-miniml-pl-pos:
	$(call run_interp_test,miniml,pl,pos)

test-miniml-pl-neg:
	$(call run_interp_test,miniml,pl,neg)

test-miniml-al: test-miniml-al-pos test-miniml-al-neg
	@echo "#### AL (Mini-ML) tests passed"

test-miniml-sl: test-miniml-sl-pos test-miniml-sl-neg
	@echo "#### SL (Mini-ML) tests passed"

test-miniml-pl: test-miniml-pl-pos test-miniml-pl-neg
	@echo "#### PL (Mini-ML) tests passed"

test-miniml: test-miniml-al test-miniml-sl test-miniml-pl
	@echo "#### Mini-ML interpreter tests passed"

test: test-quick test-interp
	@echo "#### All quick tests + p4 + impty + Mini-ML interpreter tests passed"
