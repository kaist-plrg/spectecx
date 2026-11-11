Splice warnings underline only the unknown name, including after indentation
and before arguments.

  $ mkdir input
  $ cat > input/anchors.adoc <<'EOF'
  > ${unknown:}
  >   ${unknown: key}
  > EOF
  $ spectec splice -i input -o output ../../specs/impty/base/spec.spectec --color never
  warning: unknown splice anchor: unknown
    --> input/anchors.adoc:1:3
    |
  1 | ${unknown:}
    |   ^^^^^^^
    |
    | source: splice
  
  warning: unknown splice anchor: unknown
    --> input/anchors.adoc:2:5
    |
  2 |   ${unknown: key}
    |     ^^^^^^^
    |
    | source: splice

The driver places each known anchor's text inside its frame. For anchors with the same name, it writes the `[[key]]` header only at the first anchor that resolves the key. The unused-keys report includes every registered anchor name.

  $ mkdir known
  $ cat > known/known.adoc <<'EOF'
  > ${syntax: type}
  > ${syntax: type}
  > ${rel-title-prose: Check_prog}
  > EOF
  $ spectec splice -i known -o known-out --missing known.missing ../../specs/impty/base/spec.spectec --color never
  $ cat known-out/known.adoc
  [[type]]
  [source,bison]
  ----
  syntax type = 
     | INT
     | BOOL
  ----
  [source,bison]
  ----
  syntax type = 
     | INT
     | BOOL
  ----
  [[Check_prog]]
  [.sidebar-title]
  ****
  xref:Check_prog[Check_prog]:
  
  * Program ``command`` is well-typed
  ****
  $ grep '^#' known.missing
  # func-prose: 1/1 unused (100.00%)
  # func-source: 1/1 unused (100.00%)
  # func-title-prose: 1/1 unused (100.00%)
  # func-title-source: 1/1 unused (100.00%)
  # rel-prose: 7/7 unused (100.00%)
  # rel-source: 7/7 unused (100.00%)
  # rel-title-prose: 6/7 unused (85.71%)
  # rel-title-source: 7/7 unused (100.00%)
  # syntax: 10/11 unused (90.91%)
