using JuliaParser
using Base.Test  

const Lexer = JuliaParser.Lexer

#= test skip to end of line =#
io = IOBuffer("abcd\nabcd\n")
Lexer.skip_to_eol(io)
@test position(io) == 5
skip(io, 2)
@test position(io) == 7
Lexer.skip_to_eol(io)
@test position(io) == 10 
@test eof(io) == true

# no line break in buffer 
io = IOBuffer("abcde")
Lexer.skip_to_eol(io)
@test position(io) == 5
@test eof(io) == true

# empty buffer 
io = IOBuffer()
Lexer.skip_to_eol(io)
@test eof(io) == true


#= test read_operator =#

#= test is char hex =#
for i = 1:9
    @test Lexer.is_char_hex(first("$i"))
end
for c in ['a', 'b', 'c', 'd', 'e', 'f']
    @test Lexer.is_char_hex(c) 
end
@test Lexer.is_char_hex('z') == false
for c in ['A', 'B', 'C', 'D', 'E', 'F']
    @test Lexer.is_char_hex(c)
end
@test Lexer.is_char_hex('Z') == false


#= test is char oct =#
for i = 1:9
    if i < 8
        @test Lexer.is_char_oct(first("$i"))
    else
	@test Lexer.is_char_oct(first("$i")) == false
    end
end


#= test is char bin =#
@test Lexer.is_char_bin('0')
@test Lexer.is_char_bin('1')
@test Lexer.is_char_bin('2') == false


#= test skipwhitespace =# 
io = IOBuffer("   abc")
Lexer.skipwhitespace(io)
@test position(io) == 4

io = IOBuffer("abc")
Lexer.skipwhitespace(io)
@test position(io) == 1

io = IOBuffer(" \n abc")
Lexer.skipwhitespace(io)
@test position(io) == 4

io = IOBuffer("")
Lexer.skipwhitespace(io)
@test position(io) == 0

#= test skipcomment =# 
io = IOBuffer("#test\n")
Lexer.skipcomment(io)
@test position(io) == 6

io = IOBuffer("# \ntest")
Lexer.skipcomment(io)
@test position(io) == 3

io = IOBuffer("test")
@test_throws Lexer.skipcomment(io)

