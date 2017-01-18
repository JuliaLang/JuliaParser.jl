using JuliaParser
using FactCheck

const Parser = JuliaParser.Parser
const Lexer  = JuliaParser.Lexer

const TokenStream = JuliaParser.Parser.TokenStream

include("ast.jl")

facts("test parse IOBuffer") do
    io  = IOBuffer("# test comment")
    res = Parser.parse(io)
    @fact res --> nothing
end

facts("test parse IOStream") do
    io = open(joinpath(dirname(@__FILE__), "testfile"))
    res = Parser.parse(io)
    @fact res --> nothing
end

facts("test parse string") do
    str = "# test comment"
    res = Parser.parse(str)
    @fact res --> nothing
end

facts("test special case all whitespace") do
    io  = IOBuffer("")
    res = Parser.parse(io)
    @fact eof(io) --> true
    @fact res --> nothing

    io = IOBuffer(" \n")
    res = Parser.parse(io)
    @fact eof(io) --> true
    @fact res --> nothing

    io = IOBuffer("# test comment\n")
    res = Parser.parse(io)
    @fact eof(io) --> true
    @fact res --> nothing

    io = IOBuffer("#= test comment \n
                  another comment =#\n")
    res = Parser.parse(io)
    @fact eof(io) --> true
    @fact res --> nothing
end

facts("test simple expressions/operators") do
    exprs = ["1 + 1",
             "1 + 1 + 1",
             "1 * 1 * 1",
             "1 / 2",
             "1 // 2",
             "1 < 2",
             "1 <= 2",
             "1 > 2",
             "1 >= 2",
             "1 == 2",
             "1 === 2",
             "1 != 2",
             "1 !== 2",
             "1 | 2",
             "1 << 2",
             "1 >> 2",
             "1 >>> 2",
             "1 % 2",
             "1 & 2",
             "x → y",
             "a'",
             "a.'"
             ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end

    for i = 1:length(exprs)
        for j = i:length(exprs)
            ex = "$(exprs[i]) + $(exprs[j])"
            @fact Parser.parse(ex) --> Base.parse(ex)
        end
    end
end

facts("test assignment expressions") do
    exprs = [
        "a = 1",
        "a = b,c",
        "a = (b,c)",
        "a = 1; b = 2"
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test parse single operator") do
    for op in Lexer.OPERATORS
        if op === Symbol("'")
            continue
        end
        code = string(op)
        try
            ex = Base.parse(code)
            @fact Parser.parse(code) --> Symbol("$ex")
        catch
            # do nothing if base cannot parse operator
        end
    end
    @fact_throws Parser.parse("'")
end

facts("test tuple expressions") do
    exprs = [
        "1,2",
        "1,2,3",
        "()",
        "(==)",
        "(1)",
        "(1,)",
        "(1,2)",
        "(a,b,c)",
        "(a...)",
        "((a,b)...)",
        "((a,b);)",
        "((a,b);(c,d);)"]

    # Disallowed in lowering, but allowed in parser
    (VERSION >= v"0.5.0-dev+2375") && append!(exprs,[
        "(1,2;3)",
        "((a,b);(c,d),(e,f))"
    ])
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
    # unexpected closing token
    code = "(1,]"
    @fact_throws Parser.parse(code)
    code = "(1,}"
    @fact_throws Parer.parse(code)

    # missing separator in tuple constructor
    code = "(1,2 3)"
    @fact_throws Parser.parse(code)
end

facts("test parse block") do
    exprs = [
        "begin
            x + 1
        end",
        "(x + 1; nothing)",
        "(x(); nothing)",
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test range expressions") do
    exprs = [
        "1:2",
        "1:2:3",
        ":2",
        ":1:2",
        "10:-1:1",
        "-10:1:10",
        "10:-1:-10",
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
    @fact_throws Parser.parse("1:end")
    @fact_throws Parser.parse("1:2:end")
end

facts("parse symbol / expression quote") do
    exprs = [
        ":a",
        "a :b",
        ":(a + 1)"
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("parse misplaced (=)") do
    @fact_throws Parser.parse("(=)")
end

facts("test char literal expression") do
    exprs = [
        "'a'",
        "'1'",
        "'\n'",
        "\\",
        "'$(convert(Char,256))'",
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test string literal expression") do
    exprs = [
        "\"test\""
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test prefixed string literals") do
    exprs = [
        """x\"test\"""",
        """x\"\"\"test\"\"\"""",
        """\"test\"x""",
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test cell expressions") do
    exprs = [
        "[]",
        "Any[1,2]",
        "Any[1 2 3]",
        "Any[1;2;3]",
        "Any[Any[1 2 3], Any[1 2 3]]",
        "Any[Any[1,2,3] Any[1,2,3,]]",
        """Any[Any[1,2,3]
            Any[1,2,3]]""",
        "Any[:a => 1,'b' => 2]",
        "Any[i for i=1:10]",
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test cat expressions") do
    exprs = [
        "[]",
        "[1,2]",
        "[1,2,3,]",
        "[1 2 3]",
        "[[1 2 3], [1 2 3]]",
        "[[1,2,3] [1,2,3,]]",
        """[[1,2,3]
            [1,2,3]]""",
        VERSION < v"0.4.0-dev+980" ? "[:a => 1, :b => 2]" : "Dict(:a => 1, :b => 2)",
        "[i for i=1:10]"
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test generator expressions") do
    exprs = [
        "(i/2 for i=1:2)",
        "collect(2i for i=2:5)"
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test macrocall expression") do
    exprs = [
        "@test",
        "@test 1 2",
        "@test(a,b)"
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test backquote (cmd) expression") do
    exprs = [
        "``",
        "`pwd()`",
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end

    # premature end of file
    @fact_throws Parser.parse("`pwd()")
end

facts("test quote/begin expression") do
    exprs = [
        """begin; x + 1; end""",
        """quote; x + 1; end""",
        """
        begin
            x + 1
        end
        """,
        """
        quote
            x + 1
        end
        """
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test while expression") do
    exprs = [
        """while true; x + 1; end""",
        """
        while true
            x + 1
        end
        """
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test for loop expression") do
    exprs = [
        """for i in coll; x + i; end""",
        """for i ∈ coll; x + i; end""",
        """for i = 1:10
            x + 1
         end""",
         """for i=1:10,j=1:10,k=1:10
             i + j + k
         end"""
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test if condtion expression") do
    exprs = [
        """if x == 2
           end""",
        """if x == 1
           else
           end""",
        """if x == 1
           elseif x == 2
           else
           end""",
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
    # else if should throw and error => use elseif
    @fact_throws Parser.parse("""if x == 1
                                 else if
                                 end""")
end

facts("test let expression") do
    exprs = [
        """let
        end""",
        """let; end""",
        """let x=1,y=2
            x + y
        end"""
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test global/local reserved words") do
    exprs = ["global x",
             "global x, y",
             "global x = 1, y = 2",
             "global const x = 1, y = 2",
             "global const x = 1",
             "local x",
             "local x = 1"]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test function expressions") do
    exprs = ["function x() end",
             """function x()
                x + 1
              end""",
              """function x()
                  return x + 1
              end""",
              """foo(a::bar) = foo(a.a)""",
              """function foo end""",
              """function foo
                 end"""]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test function return tuple") do
    exprs = ["""function test(x)
                    return (x, next(x))
                end
            """,
            """
            # from Iterators.jl parse failure
            function next(it::Iterators.Take, state)
                n, xs_state = state
                v, xs_state = next(it.xs, xs_state)
                return v, (n - 1, xs_state)
            end
            """
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test array ref") do
    exprs = [
        """
        # from Iterators.jl parse failure
        for i in 1:overlap
            p[i] = ans[iter.step + i]
        end
        """
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test macro expressions") do
    exprs = ["""macro x(body) end""",
            """macro x(body)
                \$body
            end""",
             """macro x()
                quote
                    x + 1
                end
              end""",
              """macro x()
                  :(x + 1)
              end"""]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test abstract type expression") do
    exprs = [
        "abstract Test",
        "abstract Test{T}",
        "abstract Test1{T<:Test}",
        "abstract Test1 <: Test2",
        "abstract Test1{T} <: Test2{T}"
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test unary operator expressions") do
    exprs = [
        "-10",
        "x -y",
        "-2x",
        "-x",
        "-2^x",
        "-2^-3",
        "2^(-x)",
        "-x.^-y",
        "2^-x",
        "-2^-x",
        "¬ = 1"
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test multiply expressions") do
    exprs = [
        "2 * x",
        "2*x",
        "2(10)",
        "2x"
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
    #TODO: errors when is_juxtaposed is false (reserved words, etc)
end

facts("test type assertions") do
    exprs = [
        "x::Int",
        "x::Array{Float32, 2}",
        "const x::Int = 1",
        "(x + 1)::Int",
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test type / immutable expression") do
    exprs = [
        """type Test;end""",
        """type Test
        end""",
        """type Test{T}; end""",
        """type Test{T}
        end""",
        """type Test{T<:Int}
        end""",
        """type Test{T,X}
        end""",
        """type Test <: Int
        end""",
        """type Test
            x
            y
            z
        end""",
        """type Test{T} end""",
        """type Test
            x
            y
        end""",
        """type Test{T<:Int}
            x::T
            y::T
        end""",
        """type Test1{Test2{T} <: Test3}; end""",
        """immutable Test;end""",
        """immutable Test
        end"""
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
    immutabletypeexprs = [
        """immutable type Test; end""",
        """immutable type Test
                x
                y
                z
        end"""
    ]

    for ex in immutabletypeexprs
        if VERSION < v"0.4"
            @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
        else
            @fact_throws Parser.parse(ex)
        end
    end
end

facts("test try, catch, finally expression") do
    exprs = [
        """try;end""",
        """try
        end""",
        """try
            x + 1
        catch
        end""",
        """try
            x + 1
        catch ex
            rethrow(ex)
        end""",
        """try
            x + 1
        catch ex
            rethrow(ex)
        finally
            dothis()
        end""",
    ]
    for (i, ex) in enumerate(exprs)
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test ternary operator") do
    exprs = [
        "x::Bool = z == 10 ? true : false",
        """x = z == 10 ? true :
                         false
        """
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test return expression") do
    exprs = [
        "return",
        "return x + 1"
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test break / continue expression") do
    exprs = [
        "break",
        "continue",
        """while true
            break
        end""",
        """while true
            continue
        end"""
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test const expression") do
    exprs = [
        "const x = 1",
        "const global x = 1",
        "const local  x = 1",
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
    # expected assignment after const
    @fact_throws Parser.parse("const x")
end

facts("test module expressions") do
    exprs = [
        """module Test
        end""",
        """module Test; end""",
        """baremodule Test
        end""",
        """module Test
            const x = 1
        end""",
        """baremodule Test
            const x = 1
        end"""
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test export expression") do
    exprs = [
        """export a""",
        """export @a,b,c,
                d""",

    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
    # you need to export at least one symbol
    @fact_throws Parser.parse("export")
end

facts("test import / using / importall expressions") do
    exprs = [
        """import Test""",
        """import .Test""",
        """import ..Test""",
        """import ...Test""",
        """import ....Test""",
        """import .....Test""",
        """import ......Test""",
        """import .......Test""",
        """import ........Test""",
        """import Test1, Test2""",
        """import Test: a, b, c""",
        """import Test: a, b,\nc,d""",
        """import Test: a, b; c, d""",
        """import Test.Base: @a, b, c, d""",
        """using Test""",
        """importall Test""",
        """:(using A)""",
        """:(using A.b, B)""",
        """:(using A: b, c.d)""",
        """:(importall A)""",
        """:(import A)""",
        """:(import A.b, B)""",
        """:(import A: b, c.d)""",
        """begin using A end"""
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test bitstype expression") do
    exprs = [
        "bitstype 16 Float16",
        "bitstype 16 Float16 <: FloatingPoint"
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end

end

facts("test typealias expression") do
    exprs = [
        "typealias Test Test",
        "typealias Test Union{Test, Test}",
        "typealias Test Tuple{Int, Int}",
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test ccall expression") do
     exprs = [
     "ccall((:testUcharX, \"./libccalltest\"), Int32, (UInt8,), x)"
     "ccall((:testUcharX, \"./libccalltest\"), :stdcall, Int32, (UInt8,), x)"
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end

end

facts("test string interpolation") do
    exprs = [
        "\"\$test\"",
        "\"\$(\"string\")\"",
        "\"\$(1 + 1)\"",
        "\"\"\"\$(1+1)\"\"\"",
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("test parse do") do
    exprs = [
        """
        test(x) do
            x + 1
        end
        """,
        """
        map(inputs...; init=nothing, typ=Any) do args...
        end
        """,
        """
        test(open(\"test.txt\")) do x, y, z
            ret = x + y + z
            return ret
        end
        """
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("test parse formula") do
    exprs = [
        """y ~ x""",
        """y ~ x + log(y)""",
        """y ~ x += 2""",
    ]
    for ex in exprs
        @fact Parser.parse(ex) --> Base.parse(ex)
    end
end

facts("parse argument list") do
    exprs = [
        """
        function test(x, y, z)
        end""",
        """function test(x,y=10,z)
        end""",
        """function test(x, y=10; z=10)
        end""",
        """+{T}(x::T, y::T) = x + y""",
        """function test{T<:Int}(::Type{T}, x::T, y::Int=7)
        end""",
        """test{T<:Int}(::Type{T}, x::T, y::Int=7) = begin
        end""",
        """function test(;x=10,y=10,z=10)
        end""",
        """function test(x,
                         y;
                         z=10)
        end""",
        """test(x,y,z) = begin
        end""",
        """test(x,y=10;z=10) = begin
        end""",
        """test(;x=10,y=10,z=10) = begin
        end"""
    ]

    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("parse test functions") do
    exprs = ["""
        test(x) = println(\"hello world! \$x\")
    """,
    """
       function test{T<:Int}(x::T, y::T=zero(T); z::T=one(T))
           return (true, x + y + z)
       end""",
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

facts("parse test module") do
    exprs = [
"""
module Test
    abstract AbstractTest

    type Test <: AbstractTest
        a
        b
        c
    end

    immutable Test{T<:Int32}
        x::T
        y::T
    end

    const X = 10
    const Y = [[1,2,3]
               [1,2,3]]

    let x = 10, y = 10
        test(z::Int) = x + y + z
    end

    test(x) = begin
        println(\"hello world! \$x\")
    end

    function test{T<:Int}(x::T, y::T=zero(T); z::T=one(T))
        (x + y + z;)
    end

    function test(x)
        x + 2
    end

end
""",
"""
baremodule Test
    function test1(x)
        (x;)
    end
    function test2(y) y end
end
""",
    ]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end


facts("parser failures") do
    exprs = [
"""
# don't consume newline char in skipping comments
immutable RGB <: ColorValue
    r::Float64 #cmt
    g::Float64 #cmt
    b::Float64
end
""",
"""
# method declarsions with builtin operators with more than one argument
+(x::Bool, y::Bool) = int(x) + int(y)
""",
"""
# calling convention is Expr(:stdcall) not QuoteNode(:stdcall)
ret=ccall(:GetEnvironmentVariableA,stdcall,UInt32,(Ptr{UInt8},Ptr{UInt8},UInt32),s,val,len)
""",
"""
# type assert a string macro expression
writemime(io, ::MIME"text/plain", x) = showlimited(io, x)
""",
"""
# Module.macro syntax
l = (Base.@_mod64 (length(a)-1)) + 1
""",
"""
# invalid char literal after windows? (parsed as ctranspose_op)
windows? ';' : ':'
""",
"""
# try / catch / finally didn't parse correctly
try
    error(" ")
catch
    test()
finally
    after = 1
end
""",
"""
# comments were not obeying whitespace_newline when parsing leading paren
if (isWeekend(c,w)
    || (d == 26 && m == January)
    # Republic Day
    || (d == 25 && m == December))
    return false
end
""",
"""
# lexer return .* as an operator (symbol)
import Base.*
""",
# large number corner cases
"""
-9223372036854775808^2 == -(9223372036854775808^2)
""",
"""
isa(170141183460469231731687303715884105728,BigInt)
""",
"""
ordtype(o::by, vs::abstractarray) = try typeof(o.by(vs[1])) catch; any end
""",
"""
try something
catch test()
x
end
""",
]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
    @fact typeof(Parser.parse("0o724")) --> typeof(parse("0o724"))
    @fact typeof(Parser.parse("0o1111111111111111111111")) --> UInt64
end

facts("misc errors") do
    context("disallow dot error") do
        ex = nothing
        try
            Parser.parse("2.2.")
        catch ex
            ex = ex
        end
        @fact isa(ex, JuliaParser.Diagnostics.Diagnostic) --> true
        @fact ex.elements[1].text --> "invalid numeric constant \"2.2.\""
    end
end

if VERSION >= v"0.4.0-dev+2606"
    # Julia issue 9617
    facts("incorrect parsing of hex floats") do
        src = """
        let p = 15
            @test 2p+1 == 31  # not a hex float literal
        end
        """
        @fact Parser.parse(src) |> norm_ast --> (Base.parse(src) |> norm_ast)
    end
end

if VERSION >= v"0.4.0-dev+3083"
    facts("interpolate var in cache block") do
        src = """
        try
        catch \$x
        end
        """
        # See Julia issue #17704
        #@fact Parser.parse(src) |> norm_ast --> (Base.parse(src) |> norm_ast)
    end
end

facts("parse two statements") do
    s = "0\na(b)"
    ts = Lexer.TokenStream(s)
    @fact Parser.parse(ts) --> 0
    @fact Parser.parse(ts) --> :(a(b))
end

facts("misc syntax changes") do
    exprs = ["t <: Tuple","a >: b","@__LINE__"]
    for ex in exprs
        @fact (Parser.parse(ex) |> norm_ast) --> (Base.parse(ex) |> norm_ast)
    end
end

#issue 72
facts("parse an incorrectly specified vector") do
    str = "[1,2;3]"
    @fact Parser.parse(str) --> Base.parse(str)
end
