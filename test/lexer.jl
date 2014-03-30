const Lexer = JuliaParser.Lexer

facts("test skip to end of line") do
    io = IOBuffer("abcd\nabcd\n")
    Lexer.skip_to_eol(io)
    @fact position(io) => 5
    skip(io, 2)
    @fact position(io) => 7
    Lexer.skip_to_eol(io)
    @fact position(io) => 10 
    @fact eof(io) => true

    context("no line break in buffer") do 
        io = IOBuffer("abcde")
        Lexer.skip_to_eol(io)
        @fact position(io) => 5
        @fact eof(io) => true
    end

    context("empty buffer") do
        io = IOBuffer()
        Lexer.skip_to_eol(io)
        @fact eof(io) => true
    end
end

#= test read_operator =#

facts("test is char hex") do
    for i = 1:9
        @fact Lexer.is_char_hex(first("$i")) => true
    end
    for c in ['a', 'b', 'c', 'd', 'e', 'f']
        @fact Lexer.is_char_hex(c) => true
    end
    @fact Lexer.is_char_hex('z') => false
    for c in ['A', 'B', 'C', 'D', 'E', 'F']
        @fact Lexer.is_char_hex(c) => true
    end
    @fact Lexer.is_char_hex('Z') => false
end

facts("test is char oct") do
    for i = 1:9
        if i < 8
            @fact Lexer.is_char_oct(first("$i")) => true
        else
	    @fact Lexer.is_char_oct(first("$i")) => false
        end
    end
end

facts("test is char bin") do
    @fact Lexer.is_char_bin('0') => true
    @fact Lexer.is_char_bin('1') => true
    @fact Lexer.is_char_bin('2') => false
end

facts("test skipwhitespace") do
    io = IOBuffer("   abc")
    Lexer.skipwhitespace(io)
    @fact position(io) => 4

    io = IOBuffer("abc")
    Lexer.skipwhitespace(io)
    @fact position(io) => 1

    io = IOBuffer(" \n abc")
    Lexer.skipwhitespace(io)
    @fact position(io) => 4

    io = IOBuffer("")
    Lexer.skipwhitespace(io)
    @fact position(io) => 0
end

facts("test skipcomment") do
    io = IOBuffer("#test\n")
    Lexer.skipcomment(io)
    @fact position(io) => 6

    io = IOBuffer("# \ntest")
    Lexer.skipcomment(io)
    @fact position(io) => 3

    io = IOBuffer("#")
    Lexer.skipcomment(io)
    @fact position(io) => 1 

    io = IOBuffer("# ")
    Lexer.skipcomment(io)
    @fact position(io) => 2 

    context("must start with a comment symbol") do
        io = IOBuffer("test")
        @fact_throws Lexer.skipcomment(io)
    end
end

facts("test skipcomment") do
    io = IOBuffer("#=test=#a")
    Lexer.skip_multiline_comment(io, 0)
    @fact position(io) => 8

    io = IOBuffer("#======#a")
    Lexer.skip_multiline_comment(io, 0)
    @fact position(io) => 8

    io = IOBuffer("#==#a")
    Lexer.skip_multiline_comment(io, 0)
    @fact position(io) => 4

    io = IOBuffer("#=test\ntest\n=#a")
    Lexer.skip_multiline_comment(io, 0)
    @fact position(io) => 14

    io = IOBuffer("#= # =#")
    Lexer.skip_multiline_comment(io, 0)
    @fact position(io) => 7

    io = IOBuffer("#=\n#= =# =#")
    Lexer.skip_multiline_comment(io, 0)
    @fact position(io) => 11

    io = IOBuffer("#= test")
    @fact_throws Lexer.skip_multiline_comment(io, 0)

    io = IOBuffer("#=#=#")
    @fact_throws Lexer.skip_multiline_comment(io, 0)
end
