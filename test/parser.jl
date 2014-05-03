using JuliaParser
using FactCheck

const Parser = JuliaParser.Parser
const Lexer  = JuliaParser.Lexer

const TokenStream = JuliaParser.Parser.TokenStream 

tokens(ts::TokenStream) = begin
    toks = {}
    while !eof(ts.io)
        push!(toks, Lexer.next_token(ts))
    end
    return toks
end

tokens(str::String) = tokens(TokenStream(str))

without_linenums(ex::Expr) = begin
    args = {}
    for a in ex.args
        if isa(a, Expr) && !is(a.head, :line)
            push!(args, without_linenums(a))
        else
            is(a, LineNumberNode) && continue
            push!(args, a)
        end
    end
    return Expr(ex.head, args...)
end

without_linenums(ex::QuoteNode) = QuoteNode(without_linenums(ex.value))

#=
facts("test TokenStream constructor") do
    io = IOBuffer("testfunc(i) = i * i") 
    try
        ts = TokenStream(io)
        @fact true => true
    catch
        @fact false => false
    end

end

facts("test set_token! / last_token") do
    code = "1 + 1"
    tks = tokens(code) 
    @fact tks => {1, :+, 1} 
    
    ts = TokenStream(code)
    @fact Parser.last_token(ts) => nothing
    
    Parser.set_token!(ts, tks[1])
    @fact Parser.last_token(ts) => tks[1]

    Parser.set_token!(ts, tks[2])
    @fact Parser.last_token(ts) => tks[2]
end

facts("test put_back!") do
    code = "1 + 1"
    tks  = tokens(code)
    ts   = TokenStream(code)
    Parser.put_back!(ts, tks[1])
    @fact ts.putback => tks[1]
    @fact_throws ts.put_back(tks[2])
end

facts("test peek_token") do
    code = "1 + 1"
    tks  = tokens(code)
    ts   = TokenStream(code)
    @fact Parser.peek_token(ts) => tks[1]
    Parser.put_back!(ts, :test)
    @fact Parser.peek_token(ts) => :test
    @fact_throws Parser.put_back!(ts, :test2)
    Parser.set_token!(ts, :test2)
    @fact Parser.peek_token(ts) => :test
end 

# you must peek before you can take
facts("test take_token") do
    code = "1 + 1"
    ts   = TokenStream(code)
    @fact Parser.take_token(ts) => nothing
    for t in tokens(code)
        tk = Parser.peek_token(ts)
        @fact tk => t
        Parser.take_token(ts) 
    end
end

facts("test require_token") do
    code = "1 +\n1"
    ts   = TokenStream(code)
    for t in (1, :+, 1)
        tk = Parser.require_token(ts)
        @fact tk => t
        Parser.take_token(ts)
    end
end

facts("test parse IOBuffer") do
    io  = IOBuffer("# test comment")
    res = Parser.parse(io)
    @fact res => nothing
end

facts("test parse String") do
    str = "# test comment"
    res = Parser.parse(str)
    @fact res => nothing
end

facts("test special case all whitespace") do
    io  = IOBuffer("")
    res = Parser.parse(io)
    @fact eof(io) => true
    @fact res => nothing

    io = IOBuffer(" \n")
    res = Parser.parse(io)
    @fact eof(io) => true
    @fact res => nothing

    io = IOBuffer("# test comment\n")
    res = Parser.parse(io)
    @fact eof(io) => true
    @fact res => nothing

    io = IOBuffer("#= test comment \n
                  another comment =#\n")
    res = Parser.parse(io)
    @fact eof(io) => true
    @fact res => nothing
end

facts("test is_juxtaposed") do 
    ex = Expr(:call, :+, 1)
    @fact Parser.is_juxtaposed(ex, :+) => false
    @fact Parser.is_juxtaposed(:+, 1)  => false
    @fact Parser.is_juxtaposed(ex, :if) => false
    @fact Parser.is_juxtaposed(ex, '\n') => false
    @fact Parser.is_juxtaposed(Expr(:..., 1), 1) => false
    @fact Parser.is_juxtaposed(ex, '(') => false
    @fact Parser.is_juxtaposed(ex, 1) => true
    @fact Parser.is_juxtaposed(1, 1) => true
    @fact Parser.is_juxtaposed(1, '(') => true
end

facts("test simple numeric expressions") do
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
             ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end

    for i = 1:length(exprs)
        for j = i:length(exprs)
            ex = "$(exprs[i]) + $(exprs[j])"
            @fact Parser.parse(ex) => Base.parse(ex)
        end
    end
end

facts("test assignment expressions") do
    code = "a = 1"
    @fact Parser.parse(code) => Base.parse(code) 

    code = "a = 1;b = 2"
    @fact Parser.parse(code) => Base.parse(code)
end

facts("test parse single operator") do
    for op in Lexer.operators
        if op === symbol("'")  || 
           op === symbol("::") ||
           op === symbol(":>")
            continue
        end
        code = string(op) 
        try
            ex   = Base.parse(code)
            @fact Parser.parse(code) => ex
        catch
            # do nothing if base cannot parse operator
        end
    end
    @fact_throws Parser.parse("'")
    @fact_throws Parser.parse("::")
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
        "((a,b);(c,d))"
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end
    # unexpected closing token
    code = "(1,]"
    @fact_throws Parser.parse(code) 
    code = "(1,}"
    @fact_throws Parer.parse(code)
    
    # unexpected ; in tuple constructor
    code = "(1,2;3)"
    @fact_throws Parser.parse(code)
    
    # unexpected , in statement block 
    code = "((a,b);(c,d),(e,f))"
    @fact_throws Parser.parse(code)

    # missing separator in statement block
    code = "((a,b) (c,d))"
    @fact_throws Parser.parse(code)

    # missing separator in tuple constructor
    code = "(1,2 3)"
    @fact_throws Parser.parse(code)
end

facts("test range expressions") do
    exprs = [
        "1:2", 
        "1:2:3", 
        "10:-1:1"
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end
    @fact_throws Parser.parse("1:end")
    @fact_throws Parser.parse("1:2:end")
end

facts("parse symbol / expression quote") do 
    exprs = [
        ":a",
        ":(a + 1)"
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
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
        "'$(char(256))'",
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end
end

facts("test string literal expression") do
    exprs = [
        "\"test\""
        #TODO: string interpolation
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end
end

facts("test cell expressions") do
    exprs = [
        "{}",
        "{1,2}",
        #"{1 2 3}",
        "{:a => 1,:b => 2}",
        "{i for i=1:10}",
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end
end

facts("test cat expressions") do
    exprs = [
        "[]",
        "[1,2]",
        "[1,2,3,]",
        "[:a => 1, :b => 2]",
        "[i for i=1:10]"
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end
end

facts("test macrocall expression") do
    exprs = [
        "@test",
        "@test 1 2",
        "@test(a,b)"
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end
end

facts("test backquote (cmd) expression") do 
    exprs = [
        "``",
        "`pwd()`",
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end

    # premature end of file
    @fact_throws Parser.parse("`pwd()")
end

facts("test quote/begin expression") do
    exprs = [
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
        pex = Parser.parse(ex)
        bex = Base.parse(ex) 
        if isa(pex, QuoteNode) && isa(bex, QuoteNode)
            pex, bex = pex.value, bex.value
        end
        @fact pex.head => bex.head
        @fact Base.without_linenums(pex.args) => Base.without_linenums(bex.args)
    end
end

facts("test while expression") do
    exprs = [
        """
        while true
            x + 1
        end
        """
    ]
    for ex in exprs
        pex = Parser.parse(ex)
        bex = Parser.parse(ex)
        @fact pex.head => bex.head
        @fact Base.without_linenums(pex.args) => Base.without_linenums(bex.args)
    end
end

facts("test for loop expression") do
    exprs = [
        """for 1 = 1:10
            x + 1
           end""",
        #"for i in coll; x + 1; end"
    ]
    for ex in exprs
        pex = Parser.parse(ex)
        bex = Parser.parse(ex)
        @fact pex.head => bex.head
        @fact Base.without_linenums(pex.args) => Base.without_linenums(bex.args)
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
           end"""
    ]
    for ex in exprs
        pex = Parser.parse(ex)
        bex = Parser.parse(ex)
        @fact pex.head => bex.head
        @fact Base.without_linenums(pex.args) => Base.without_linenums(bex.args)
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
        pex = Parser.parse(ex)
        bex = Parser.parse(ex)
        @fact pex.head => bex.head
        @fact Base.without_linenums(pex.args) => Base.without_linenums(bex.args)
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
        @fact Parser.parse(ex) => Base.parse(ex)
    end
end

facts("test function expressions") do
    exprs = ["function x() end",
             """function x()
                x + 1
              end""", 
              """function x()
                  return x + 1
              end"""]
    for ex in exprs
        pex = Parser.parse(ex)
        bex = Parser.parse(ex)
        @fact pex.head => bex.head
        @fact Base.without_linenums(pex.args) => Base.without_linenums(bex.args)
    end
end

facts("test macro expressions") do
    exprs = ["macro x(body) end",
             """macro x()
                quote
                    x + 1
                end
              end""", 
              """macro x()
                  :(x + 1)
              end"""]
    for ex in exprs
        pex = Parser.parse(ex)
        bex = Parser.parse(ex)
        @fact pex.head => bex.head
        @fact Base.without_linenums(pex.args) => Base.without_linenums(bex.args)
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
        @fact Parser.parse(ex) => Base.parse(ex)
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
        end""",
        #"""type Test{T}
        #    x::T
        #end""",
        """immutable Test;end""",
        """immutable Test
        end""",
        """immutable type Test; end""",
        """immutable type Test
        end""",
    ]
    for ex in exprs
        pex = Parser.parse(ex)
        bex = Parser.parse(ex)
        @fact pex.head => bex.head
        @fact Base.without_linenums(pex.args) => Base.without_linenums(bex.args)
    end
end


facts("test type / immutable expression") do
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
        end""",
        """try
            x + 1
        catch ex
        finally
        end""",
    ]
    for ex in exprs
        pex = Parser.parse(ex)
        bex = Parser.parse(ex)
        @fact pex.head => bex.head
        @fact Base.without_linenums(pex.args) => Base.without_linenums(bex.args)
    end
end

facts("test return expression") do
    exprs = [
        "return",
        "return x + 1"
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end
end

facts("test break / continue expression") do
    #TODO: single line statments still fail 
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
        pex = Parser.parse(ex)
        bex = Parser.parse(ex)
        @fact pex.head => bex.head
        @fact Base.without_linenums(pex.args) => Base.without_linenums(bex.args)
    end
end

facts("test const expression") do
    exprs = [
        "const x = 1",
        "const global x = 1",
        "const local  x = 1",
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end
    # expected assignment after const
    @fact_throws Parser.parse("const x")
end

facts("test module expressions") do
    exprs = [
        """module Test
        end""",
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
        pex = Parser.parse(ex)
        bex = Parser.parse(ex)
        @fact pex.head => bex.head
        @fact Base.without_linenums(pex.args) => Base.without_linenums(bex.args)
    end
end

facts("test export expression") do
    exprs = [
        """export a""",
        """export a,b,c,
                d""",
        
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end
    # you need to export at least one symbol
    @fact_throws Parser.parse("export")
end
=#

facts("test import / using / importall expressions") do
    exprs = [
        """import Test""",
        """import .Test""",
        """import ..Test""",
        """import ...Test""",
        """import ....Test""",
        """import .....Test""",
        """import Test1, Test2""",
        """import Test: a, b, c""",
        """using Test""",
        """importall Test""",
        
    ]
    for ex in exprs
        @fact Parser.parse(ex) => Base.parse(ex)
    end
end
