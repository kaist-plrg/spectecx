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
