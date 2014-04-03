using JuliaParser
using FactCheck

const Parser = JuliaParser.Parser

facts("test special case all whitespace") do
    io = IOBuffer("")
    Parser.parse(io)
    @fact eof(io) => true

    io = IOBuffer(" \n")
    Parser.parse(io)
    @fact eof(io) => true

    io = IOBuffer("# test comment\n")
    Parser.parse(io)
    @fact eof(io) => true

    io = IOBuffer("#= test comment \n
                  another comment =#\n")
    Parser.parse(io)
    @fact eof(io) => true
end
