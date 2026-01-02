NAME = spectec-core

# Compile

.PHONY: exe

EXESPEC = spectec/_build/default/bin/main.exe

exe:
	rm -f ./$(NAME)
	opam switch 5.1.0
	cd spectec && opam exec -- dune build bin/main.exe && echo
	ln -f $(EXESPEC) ./$(NAME)

# Format

.PHONY: fmt

fmt:
	opam switch 5.1.0
	cd spectec && opam exec dune fmt

# Tests
#
# Individual tests:
#   make test-elab       - Elaboration test
#   make test-struct     - Structuring test
#   make test-il-pos     - IL interpreter positive tests (slow)
#   make test-il-neg     - IL interpreter negative tests
#   make test-sl-pos     - SL interpreter positive tests (slow)
#   make test-sl-neg     - SL interpreter negative tests
#
# Grouped tests:
#   make test-quick      - Fast tests only (elab + struct)
#   make test-il         - All IL tests (pos + neg)
#   make test-sl         - All SL tests (pos + neg)
#   make test            - All tests

.PHONY: test test-quick test-elab test-struct
.PHONY: test-il test-il-pos test-il-neg
.PHONY: test-sl test-sl-pos test-sl-neg
.PHONY: promote

test-elab:
	@echo "#### Running elaboration test"
	@opam switch 5.1.0
	@cd spectec && opam exec -- dune build @test/elab/runtest --profile=release && echo OK

test-struct:
	@echo "#### Running structuring test"
	@opam switch 5.1.0
	@cd spectec && opam exec -- dune build @test/struct/runtest --profile=release && echo OK

test-il-pos:
	@echo "#### Running IL interpreter positive tests"
	@opam switch 5.1.0
	@cd spectec && opam exec -- dune build @test/interp/il-pos --profile=release && echo OK

test-il-neg:
	@echo "#### Running IL interpreter negative tests"
	@opam switch 5.1.0
	@cd spectec && opam exec -- dune build @test/interp/il-neg --profile=release && echo OK

test-sl-pos:
	@echo "#### Running SL interpreter positive tests"
	@opam switch 5.1.0
	@cd spectec && opam exec -- dune build @test/interp/sl-pos --profile=release && echo OK

test-sl-neg:
	@echo "#### Running SL interpreter negative tests"
	@opam switch 5.1.0
	@cd spectec && opam exec -- dune build @test/interp/sl-neg --profile=release && echo OK

test-quick: test-elab test-struct
	@echo "#### Quick tests passed"

test-il: test-il-pos test-il-neg
	@echo "#### IL tests passed"

test-sl: test-sl-pos test-sl-neg
	@echo "#### SL tests passed"

test: test-quick test-il test-sl
	@echo "#### All tests passed"

promote:
	@opam switch 5.1.0
	@cd spectec && opam exec -- dune promote

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(NAME)
	cd spectec && dune clean
