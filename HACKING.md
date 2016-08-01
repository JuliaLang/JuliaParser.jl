# Making changes to JuliaParser

While working on JuliaParser, it is convenient to use the `testshell` located in
`bin/testshell.jl`. This REPL allows introspection of the parsed AST or location
nodes. If you make a change to JuliaParser, you can reload all parser code using
`^R` (dependencies are not reloaded) and switch between JuliaParser and the base
parser using `^S`. Thus, the recommended workflow is:
1. Investigate the problem (going back and forth using `S` as necessary)
2. Make a change in JuliaParser or add debugging statements
3. Reload the code (using `^R`).
4. Check if fixed, if not repeat.

If you want to inspect the SourceLocation tracking in JuliaParser, pass the `--sl`
command line flag to the test shell.  This option only works for single line inputs.

# Symbology

Currently, there are a number of unicode operators used to denote various AST
operations. While their final name is being decided upon, here's a handy
reference.

- `√` (\\sqrt) Get the location node for an AST node
- '¬' (\\neg) Extract the actual AST node
- '⤄' (\\nvLeftrightarrow) Expands the location associated with the node on the
  LHS to also cover the range associated to the RHS. This is a noop if the parser
  does not maintain location information.
- '⪥' Appends children of the expression (or tuple or array) on the RHS to the 
  arguments of the expression on the left hand side. If the parser does not maintain
  location information, this is equivalent to, `append!(LHS.args,collect(children(RHS)))`.
  
