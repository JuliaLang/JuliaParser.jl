using JuliaParser
using Base.Test  

const Lexer = JuliaParser.Lexer

#= test skip to end of line =#

io = IOBuffer("abcd\nabcd\n")
Lexer.skip_to_eol!(io)
@test position(io) == 5
skip(io, 2)
@test position(io) == 7
Lexer.skip_to_eol!(io)
@test position(io) == 10 
@test eof(io) == true

# no line break in buffer 
io = IOBuffer("abcde")
Lexer.skip_to_eol!(io)
@test position(io) == 5
@test eof(io) == true

# empty buffer 
io = IOBuffer()
Lexer.skip_to_eol!(io)
@test eof(io) == true

