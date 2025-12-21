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
#   make test-elab    - Elaboration test (fast)
#   make test-struct  - Structuring test (fast)
#   make test-il      - IL interpreter test (slow)
#   make test-sl      - SL interpreter test (slow)
#
# Grouped tests:
#   make test-quick   - Fast tests only (elab + struct)
#   make test         - All tests

.PHONY: test test-quick test-elab test-struct test-il test-sl promote

test-elab:
	@echo "#### Running elaboration test"
	@opam switch 5.1.0
	@cd spectec && opam exec -- dune build @test/elab/runtest --profile=release && echo OK

test-struct:
	@echo "#### Running structuring test"
	@opam switch 5.1.0
	@cd spectec && opam exec -- dune build @test/struct/runtest --profile=release && echo OK

test-il:
	@echo "#### Running IL interpreter test"
	@opam switch 5.1.0
	@cd spectec && opam exec -- dune build @test/il/runtest --profile=release && echo OK

test-sl:
	@echo "#### Running SL interpreter test"
	@opam switch 5.1.0
	@cd spectec && opam exec -- dune build @test/sl/runtest --profile=release && echo OK

test-quick: test-elab test-struct
	@echo "#### Quick tests passed"

test: test-elab test-struct test-il test-sl
	@echo "#### All tests passed"

promote:
	@opam switch 5.1.0
	@cd spectec && opam exec -- dune promote

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(NAME)
	cd spectec && dune clean
