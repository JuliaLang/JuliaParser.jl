using JuliaParser
using FactCheck

const Parser = JuliaParser.Parser
const TokenStream = JuliaParser.Parser.TokenStream 

facts("test token stream constructor") do
    io = IOBuffer("testfunc(i) = i * i") 
    try
        ts = TokenStream(io)
        @fact true => true
    catch
        @fact false => false
    end
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
