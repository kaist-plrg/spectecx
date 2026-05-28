NAME = spectecx

SWITCH ?= spectecx

OPAM_EXEC = opam exec --switch=$(SWITCH) --
DUNE = cd spectec && $(OPAM_EXEC) dune

# Compile & Format

.PHONY: exe check fmt fmt-check promote clean

EXESPEC = spectec/_build/default/bin/main.exe

exe:
	rm -f ./$(NAME)
	$(DUNE) build bin/main.exe
	@echo
	ln -f $(EXESPEC) ./$(NAME)

check:
	$(DUNE) build @check

fmt:
	$(DUNE) fmt

fmt-check:
	$(DUNE) build @fmt

promote:
	$(DUNE) promote

clean:
	rm -f ./$(NAME)
	$(DUNE) clean

# Tests
#
# Individual tests (run against the new p4 spec by default):
#   make test-elab       - Elaboration test (both p4 and p4-old)
#   make test-struct     - Structuring test (both p4 and p4-old)
#   make test-il-pos     - IL interpreter positive tests (slow)
#   make test-il-neg     - IL interpreter negative tests
#   make test-sl-pos     - SL interpreter positive tests (slow)
#   make test-sl-neg     - SL interpreter negative tests
#
# p4-old interpreter tests:
#   make test-il-pos-old / test-il-neg-old / test-sl-pos-old / test-sl-neg-old
#
# Grouped tests:
#   make test-quick      - Fast tests (elab + elab-neg + struct + impty)
#   make test-il         - IL tests for new p4 (pos + neg)
#   make test-sl         - SL tests for new p4 (pos + neg)
#   make test-il-old     - IL tests for p4-old (pos + neg)
#   make test-sl-old     - SL tests for p4-old (pos + neg)
#   make test-old        - All p4-old interpreter tests
#
# impty interpreter tests (per-variant: base, closure):
#   make test-impty-<v>-il-pos / -il-neg / -sl-pos / -sl-neg
#   make test-impty-<v>-il / -sl                     - per-variant pos+neg
#   make test-impty-<v>                              - per-variant il+sl
#   make test-impty                                  - all impty tests
#
#   make test            - quick + new p4 il/sl

.PHONY: test test-quick test-elab test-elab-neg test-struct
.PHONY: test-il test-il-pos test-il-neg
.PHONY: test-sl test-sl-pos test-sl-neg
.PHONY: test-old test-il-old test-il-pos-old test-il-neg-old
.PHONY: test-sl-old test-sl-pos-old test-sl-neg-old
.PHONY: test-impty test-impty-base test-impty-closure
.PHONY: test-impty-base-il test-impty-base-sl
.PHONY: test-impty-closure-il test-impty-closure-sl
.PHONY: test-impty-base-il-pos test-impty-base-il-neg
.PHONY: test-impty-base-sl-pos test-impty-base-sl-neg
.PHONY: test-impty-closure-il-pos test-impty-closure-il-neg
.PHONY: test-impty-closure-sl-pos test-impty-closure-sl-neg
.PHONY: promote

test-elab:
	@echo "#### Running elaboration test"
	@$(DUNE) build @test/elab/runtest --profile=release && echo OK

test-elab-neg:
	@echo "#### Running elaboration negative tests"
	@$(DUNE) build @test/elab/neg/runtest --profile=release && echo OK

test-struct:
	@echo "#### Running structuring test"
	@$(DUNE) build @test/struct/runtest --profile=release && echo OK

# $(1): target prefix (p4 / p4-old)
# $(2): il / sl
# $(3): pos / neg
define run_interp_test
	@echo "#### Running $(2) interpreter $(3) tests ($(1))"
	@$(DUNE) build @test/interp/$(1)-$(2)-$(3) --profile=release
	@cat spectec/_build/default/test/interp/$(1)-$(2)-$(3).err >&2
	@echo OK
endef

test-il-pos:
	$(call run_interp_test,p4,il,pos)

test-il-neg:
	$(call run_interp_test,p4,il,neg)

test-sl-pos:
	$(call run_interp_test,p4,sl,pos)

test-sl-neg:
	$(call run_interp_test,p4,sl,neg)

test-il-pos-old:
	$(call run_interp_test,p4-old,il,pos)

test-il-neg-old:
	$(call run_interp_test,p4-old,il,neg)

test-sl-pos-old:
	$(call run_interp_test,p4-old,sl,pos)

test-sl-neg-old:
	$(call run_interp_test,p4-old,sl,neg)

test-quick: test-elab test-elab-neg test-struct test-impty
	@echo "#### Quick tests passed"

test-il: test-il-pos test-il-neg
	@echo "#### IL tests passed"

test-sl: test-sl-pos test-sl-neg
	@echo "#### SL tests passed"

test-il-old: test-il-pos-old test-il-neg-old
	@echo "#### IL (p4-old) tests passed"

test-sl-old: test-sl-pos-old test-sl-neg-old
	@echo "#### SL (p4-old) tests passed"

test-old: test-il-old test-sl-old
	@echo "#### p4-old interpreter tests passed"

test-impty-base-il-pos:
	$(call run_interp_test,impty-base,il,pos)

test-impty-base-il-neg:
	$(call run_interp_test,impty-base,il,neg)

test-impty-base-sl-pos:
	$(call run_interp_test,impty-base,sl,pos)

test-impty-base-sl-neg:
	$(call run_interp_test,impty-base,sl,neg)

test-impty-closure-il-pos:
	$(call run_interp_test,impty-closure,il,pos)

test-impty-closure-il-neg:
	$(call run_interp_test,impty-closure,il,neg)

test-impty-closure-sl-pos:
	$(call run_interp_test,impty-closure,sl,pos)

test-impty-closure-sl-neg:
	$(call run_interp_test,impty-closure,sl,neg)

test-impty-base-il: test-impty-base-il-pos test-impty-base-il-neg
	@echo "#### IL (impty-base) tests passed"

test-impty-base-sl: test-impty-base-sl-pos test-impty-base-sl-neg
	@echo "#### SL (impty-base) tests passed"

test-impty-closure-il: test-impty-closure-il-pos test-impty-closure-il-neg
	@echo "#### IL (impty-closure) tests passed"

test-impty-closure-sl: test-impty-closure-sl-pos test-impty-closure-sl-neg
	@echo "#### SL (impty-closure) tests passed"

test-impty-base: test-impty-base-il test-impty-base-sl
	@echo "#### impty-base interpreter tests passed"

test-impty-closure: test-impty-closure-il test-impty-closure-sl
	@echo "#### impty-closure interpreter tests passed"

test-impty: test-impty-base test-impty-closure
	@echo "#### impty interpreter tests passed"

test: test-quick test-il test-sl
	@echo "#### All quick tests + p4 + impty interpreter tests passed"
