NAME = spectec-core

SWITCH ?= $(NAME)

OPAM_EXEC = opam exec --switch=$(SWITCH) --
DUNE = cd spectec && $(OPAM_EXEC) dune

# Compile & Format

.PHONY: exe fmt promote clean

EXESPEC = spectec/_build/default/bin/main.exe

exe:
	rm -f ./$(NAME)
	$(DUNE) build bin/main.exe
	@echo
	ln -f $(EXESPEC) ./$(NAME)

fmt:
	$(DUNE) fmt

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
#   make test-quick      - Fast tests only (elab + struct)
#   make test-il         - IL tests for new p4 (pos + neg)
#   make test-sl         - SL tests for new p4 (pos + neg)
#   make test-il-old     - IL tests for p4-old (pos + neg)
#   make test-sl-old     - SL tests for p4-old (pos + neg)
#   make test-old        - All p4-old interpreter tests
#   make test            - All tests excluding p4-old (quick + new p4 il/sl)

.PHONY: test test-quick test-elab test-struct
.PHONY: test-il test-il-pos test-il-neg
.PHONY: test-sl test-sl-pos test-sl-neg
.PHONY: test-old test-il-old test-il-pos-old test-il-neg-old
.PHONY: test-sl-old test-sl-pos-old test-sl-neg-old
.PHONY: promote

test-elab:
	@echo "#### Running elaboration test"
	@$(DUNE) build @test/elab/runtest --profile=release && echo OK

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

test-quick: test-elab test-struct
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

test: test-quick test-il test-sl
	@echo "#### All quick tests + p4 interpreter tests passed"
