using JuliaParser
using JuliaParser.Diagnostics: AbstractDiagnostic, Incomplete
using Base.Test

function do_diag_test(text)
    ts = Lexer.TokenStream{Lexer.SourceLocToken}(text)
    ts.filename = "test"
    try
        Parser.parse(ts)
        error("Should have failed")
    catch err
        !isa(err, AbstractDiagnostic) && rethrow(err)
        return err
    end
end

let diags = do_diag_test("""
      foo(a,b
      bar(
  """)
#  none:1:6 error: Expected ')' or ','
#  abc(d
#       ^
#  none:1:4 note: to match '(' here
#  abc(d
#     ^
    @test length(diags.elements) == 2
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("a && && b")
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("print x")
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("a ? b c")
#  none:1:7 error: colon expected in "?" expression
#  a ? b c
#        ^
#  none:1:3 note: "?" was here
#  a ? b c
#    ^
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("1:)")
#  none:1:3 error: missing last argument in range expression
#  1:)
#    ^
#  none:1:1 note: start of range was here
#  1:)
#  ^~
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("1:\n")
#   none:1:3 error: line break in ":" expression
#   1:
#     ^
#   none:1:1 note: start of range was here
#   1:
#   ^~
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("1:<Int")
#   none:1:2 error: Invalid "<" in range expression. Did you mean "<:"?
#   1:<Int
#    ^~
#    <:
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :fixit
end

let diags = do_diag_test("function f() 1")
#    none:1:15 error: incomplete: "function" requires end
#    function f() 1
#                  ^
#    none:1:1 note: "function" began here
#    function f() 1
#    ^~~~~~~~
    @test isa(diags, Incomplete)
    diags = diags.d
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("if\nend")
#    none:1:1 error: missing condition in "if"
#    if
#    ^~
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("if true\nelseif\nend")
#   none:2:1 error: missing condition in "elseif"
#   elseif
#   ^~~~~~
#   none:1:1 note: "if" was here
#   if true
#   ^~")
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("if true\nelseif\nend")
#   none:2:1 error: missing condition in "elseif"
#   elseif
#   ^~~~~~
#   none:1:1 note: "if" was here
#   if true
#   ^~")
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("if true\nelse if false\nend")
#   none:2:1 error: use elseif instead of else if
#   else if false
#   ^~~~~~~
#   elseif
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :fixit
end

let diags = do_diag_test("if true 1\nfinally\nend")
#   none:2:1 error: Unexpected "finally" in if expression
#   finally
#   ^~~~~~~
#   none:1:1 note: previous "if" was here
#   if true 1
#   ^~
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("let a b")
#   none:1:7 error: let variables should end in ";" or newline
#   let a b
#         ^
#   none:1:1 note: "let" was here
#   let a b
#   ^~~
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("function foo{T} end")
# none:1:16 error: expected "(" in function definition
# function foo{T} end
#                ^
# none:1:1 note: "function" was here
# function foo{T} end
# ^~~~~~~~
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("type type\nend")
# none:1:6 error: invalid type name "type"
# type type
#      ^~~~
# none:1:1 note: type was here
# type type
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("try 1\nelse\nend")
# none:2:1 error: Unexpected "else" in try expression
# else
# ^~~~
# none:1:1 note: "try" was here
# try 1
# ^~~
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("const sin(2)")
# none:1:7 error: expected assignment after "const"
# const sin(2)
#       ^~~~~~
# none:1:1 note: "const" was here
# const sin(2)
# ^~~~~
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("export true")
# none:1:8 error: invalid literal "true" in "export" statement
# export true
#        ^~~~
# none:1:1 note: "export" was here
# export true
# ^~~~~~
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("ccall|")
# none:1:6 error: Expected '('
# ccall|
#      ^
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("do")
# none:1:1 error: invalid "do" syntax
# do
# ^~
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("[a,a}")
# none:1:5 error: Expected "]", got "}"
# [a,a}
#     ^
# none:1:1 note: Expression began here
# [a,a}
# ^
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("[a,a b")
# none:1:6 error: missing separator in array expression
# [a,a b
#      ^
# none:1:1 note: Expression began here
# [a,a b
# ^
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("export ]")
# none:1:8 error: invalid syntax: "]"
# export ]
#        ^
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("/= = 1")
# none:1:1 error: invalid identifier name "/="
# /= = 1
# ^~
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("@[1]")
# none:1:3 error: invalid macro use "@([1])"
# @[1]
#   ^~
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("[x for (x ? true : false)]")
# none:1:9 error: invalid iteration spec
# [x for (x ? true : false)]
#         ^~~~~~~~~~~~~~~~
# none:1:4 note: for this `for`
# [x for (x ? true : false)]
#    ^~~
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("[ _ for i = 1:10}")
# none:1:17 error: expected ']' not "}"
# [ _ for i = 1:10}
#                 ^
# none:1:1 note: in comprehension that began here
# [ _ for i = 1:10}
# ^
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
    @test diags.elements[2].location.offset == 0
end

let diags = do_diag_test("[a b;\nc d}")
# none:2:5 error: unexpected "}"
#  c d}
#     ^
# none:1:1 note: In matrix expression that began here
# [a b;
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("[a b;\nc d for a = 1:10]")
# none:2:6 error: invalid comprehension syntax
#  c d for a = 1:10]
#      ^~~
# none:1:1 note: In matrix expression that began here
# [a b;
# ^
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("(a,b c)")
# none:1:5 error: missing separator in tuple
# (a,b c)
#     ^
# none:1:3 note: tuple began here
# (a,b c)
# ^
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

for expr in ["'","''","'\\","'abc'","`\\","\"","\"\"\"","\"\"\"\"","\"\"\"\"\""]
    let diags = do_diag_test(expr)
        isa(diags, Incomplete) && (diags = diags.d)
        @test diags.elements[1].severity == :error
    end
end

let diags = do_diag_test("=")
# none:1:1 error: unexpected "="
# =
# ^
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test(":(= a)")
# none:1:4 error: Expected ')'
# :(= a)
#    ^
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("(x]")
# none:1:3 error: expected ')' not ']'
# (x]
#   ^
# none:1:1 note: to match '(' here
# (x]
# ^
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("(a b)")
# none:1:3 error: missing spearator (did you mean ',' or ';')
# (a b)
#   ^
# none:1:1 note: in expression that began here
# (a b)
# ^
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("\"\$(a,)\"")
# none:1:5 error: Expected ')'
# "$(a,)"
#     ^
# none:1:2 note: In interpolation syntax starting here
# "$(a,)"
#  ^
# none:1:1 note: In string expression beginning here
# "$(a,)"
# ^
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
    @test diags.elements[3].severity == :note
end

let diags = do_diag_test("\"\$[1]\"")
# none:1:2 error: invalid interpolation syntax: "["
# "$[1]"
#  ^
# none:1:2 note: In interpolation syntax starting here
# "$[1]"
#  ^
# none:1:1 note: In string expression beginning here
# "$[1]"
# ^
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
    @test diags.elements[3].severity == :note
end

let diags = do_diag_test("\\xff")
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("0f1.2")
# none:1:1 error: invalid numeric constant "0f1."
# 0f1.2
# ^~~
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("1._a")
# none:1:1 error: invalid use of '_' in numeric constant ""
# 1._a
# ^~
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("0b12")
# none:1:1 error: invalid numeric constant "0b12"
# 0b12
# ^~~
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("#=")
# none:1:3 error: incomplete: unterminated multi-line comment #= ... =#
# #=
#   ^
# none:1:1 note: starting here
# #=
# ^
    @test isa(diags, Incomplete)
    diags = diags.d
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("for i = 1:")
# none:1:11 error: missing last argument in range expression
# for i = 1:
#           ^
# none:1:9 note: start of range was here
# for i = 1:
#         ^~
    @test diags.elements[1].severity == :error
    @test diags.elements[1].location.offset == endof("for i = 1:")
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("ccall((:SetVarDeclInit, libcxxffi), Void, (Ptr{Void},) VD)")
# none:1:56 error: Expected ')' or ','
# ccall((:SetVarDeclInit, libcxxffi), Void, (Ptr{Void},) VD)
#                                                        ^
# none:1:6 note: to match '(' here
# ccall((:SetVarDeclInit, libcxxffi), Void, (Ptr{Void},) VD)
#      ^
    @test diags.elements[1].severity == :error
    @test diags.elements[1].location.offset == 55
    @test diags.elements[2].severity == :note
end

# fix issue #47

# test string with charwidth < 1
let code = "(foo̧ + b̂ar))",
    diag = do_diag_test(code)

    io = IOBuffer()
    JuliaParser.Diagnostics.display_diagnostic(io, code, diag, filename="test")
    res = String(take!(io))
    @test startswith(res, "test:1:12")
end

# test string with charwidth > 1
let code = "(foo̧ + 🔨))",
    diag = do_diag_test(code)
    io = IOBuffer()
    JuliaParser.Diagnostics.display_diagnostic(io, code, diag, filename="test")
    res = String(take!(io))
    @test startswith(res, "test:1:11")
end
