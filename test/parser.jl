using JuliaParser
using FactCheck

const Parser = JuliaParser.Parser
const Lexer  = JuliaParser.Lexer

const TokenStream = JuliaParser.Parser.TokenStream 

tokens(io::IO) = begin
    toks = {}
    while !eof(io)
        push!(toks, Lexer.next_token(io, nothing))
    end
    return toks
end

tokens(str::String) = tokens(IOBuffer(str))

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

facts("test simple expressions") do
    code = "1 + 1"
    @fact Parser.parse(code) => Base.parse(code)
end

