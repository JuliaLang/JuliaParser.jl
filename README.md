# JuliaParser

[![Build Status](https://travis-ci.org/JuliaLang/JuliaParser.jl.svg?branch=master)](https://travis-ci.org/JuliaLang/JuliaParser.jl?branch=master)
[![Coverage Status](https://img.shields.io/coveralls/jakebolewski/JuliaParser.jl.svg)](https://coveralls.io/r/jakebolewski/JuliaParser.jl)

A pure Julia port of [Julia](https://github.com/JuliaLang/julia)'s [parser](https://github.com/JuliaLang/julia/blob/master/src/julia-parser.scm).  It should be fully compatible with Julia's built in parser and it correctly parses all ~3000+ Julia source files currently available in Julia's [300+ user packages](http://pkg.julialang.org/) and all Julia source code in [Base](https://github.com/JuliaLang/julia/tree/master/base).

Differences with Julia's Flisp Parser
-------------------------------------
* `BigInt` and `Int128` numbers are treated as literal values instead of expressions.
* Literal negation is done as negated literals rather than using `Expr(:-)`
* `QuoteNode`s are replaced with `Expr(:quote)`.

Using JuliaParser as your primary parser
-------------------------------------
JuliaParser provides a script that will replace the builtin [flisp](https://github.com/JeffBezanson/femtolisp) parser by itself.
You may load it as follows:

```
julia -L ~/.julia/v0.5/JuliaParser/bin/repl.jl
```

TODO Items
----------
* performance improvements
* refactor number tokenization
* refactor to make it more useful to use as a library (right now it is pretty monolithic)

Trying it Out
-------------
```julia
julia> Pkg.clone("JuliaParser")

julia> import JuliaParser.Parser
julia> import JuliaParser.Lexer

julia> src = """
              function test(x::Int)
                  return x ^ 2
              end
              """
julia> ts = Lexer.TokenStream(src);

julia> Lexer.next_token(ts)
:function

julia> Lexer.next_token(ts)
:test

julia> Lexer.next_token(ts)
'('

julia> Lexer.next_token(ts)
:x

julia> Lexer.next_token(ts)
:(::)

julia> Lexer.next_token(ts)
:Int

julia> ast = Parser.parse(src);

julia> Meta.show_sexpr(ast)
(:function, (:call, :test, (:(::), :x, :Int)), (:block,
    (:line, 2, :none),
    (:return, (:call, :^, :x, 2))
  ))

julia> dump(ast)
Expr 
  head: Symbol function
  args: Array(Any,(2,))
    1: Expr 
      head: Symbol call
      args: Array(Any,(2,))
        1: Symbol test
        2: Expr 
          head: Symbol ::
          args: Array(Any,(2,))
          typ: Any
      typ: Any
    2: Expr 
      head: Symbol block
      args: Array(Any,(2,))
        1: Expr 
          head: Symbol line
          args: Array(Any,(2,))
          typ: Any
        2: Expr 
          head: Symbol return
          args: Array(Any,(1,))
          typ: Any
      typ: Any
  typ: Any
```
