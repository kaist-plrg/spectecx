# Block comments nest, as in spectec/lib/pass/parse/lexer.mll.
# Comment markers in text literals and quoted operators start comments here.
# The TFAE and TRFAE specs have no markers in those positions.
{
  out = ""
  for (i = 1; i <= length($0); i++) {
    pair = substr($0, i, 2)
    if (depth > 0) {
      if (pair == "(;") { depth++; i++ }
      else if (pair == ";)") { depth--; i++ }
    } else if (pair == "(;") { depth = 1; i++ }
    else if (pair == ";;") break
    else out = out substr($0, i, 1)
  }
  sub(/[ \t]+$/, "", out)
  if (out != "") print out
}
