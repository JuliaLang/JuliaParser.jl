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
