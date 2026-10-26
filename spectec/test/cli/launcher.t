Launcher: the script that `make exe` writes passes the switch, the Dune paths,
and every argument through unchanged.

Stubs stand in for opam and the program. The stub opam prints its switch
argument and runs the command after `--`. The stub program prints the Dune
paths it receives and its arguments. Their directory name holds a space, a
quote, and an equals sign:

  $ mkdir "stubs ' a=b"
  $ cat > "stubs ' a=b/opam" <<'EOF'
  > #!/bin/sh
  > printf '%s\n' "$2"
  > shift 3
  > exec "$@"
  > EOF
  $ cat > "stubs ' a=b/program" <<'EOF'
  > #!/bin/sh
  > printf '%s\n' "$DUNE_DIR_LOCATIONS" "$DUNE_OCAML_STDLIB" \
  >   "$DUNE_OCAML_HARDCODED" "$DUNE_SOURCEROOT" "$OCAMLPATH" \
  >   "$CAML_LD_LIBRARY_PATH" "$@"
  > EOF
  $ chmod +x "stubs ' a=b/opam" "stubs ' a=b/program"

  $ DUNE_DIR_LOCATIONS="sites with a ' quote" DUNE_OCAML_STDLIB=stdlib \
  >   DUNE_OCAML_HARDCODED=runtime DUNE_SOURCEROOT=source \
  >   OCAMLPATH="libraries with spaces" CAML_LD_LIBRARY_PATH=stubs \
  >   sh ../../scripts/make-launcher.sh "switch with a ' quote" \
  >   "$PWD/stubs ' a=b/program" > launcher

  $ PATH="$PWD/stubs ' a=b:$PATH" sh launcher "a b" "c'd" '$HOME'
  --switch=switch with a ' quote
  sites with a ' quote
  stdlib
  runtime
  source
  libraries with spaces
  stubs
  a b
  c'd
  $HOME
