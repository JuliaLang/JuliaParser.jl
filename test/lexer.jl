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


facts("test string_to_number") do
   
    @fact_throws Lexer.string_to_number("")
    @fact_throws Lexer.string_to_number("1x10")
    @fact_throws Lexer.string_to_number("x10")
    @fact_throws Lexer.string_to_number("1.f.10")
    @fact_throws Lexer.string_to_number("b10")
    @fact_throws Lexer.string_to_number("0xzz")
    @fact_throws Lexer.string_to_number("0b22")
    
    context("NaN") do
        for s in ("NaN", "+NaN", "-NaN")
            n = Lexer.string_to_number(s)
            @fact isnan(n) => true
        end
    end

    context("Inf") do
        for s in ("Inf", "+Inf", "-Inf")
            n = Lexer.string_to_number(s)
            @fact isinf(n) => true
        end
    end

    context("float64") do
        s = "1.0"
        n = Lexer.string_to_number(s)
        @fact n => 1.0
        @fact typeof(n) => Float64

        s = "-1.0"
        n = Lexer.string_to_number(s)
        @fact n => -1.0
        @fact typeof(n) => Float64

        s = "1."
        n = Lexer.string_to_number(s)
        @fact n => 1.0
        @fact typeof(n) => Float64

        s = "1e10"
        n = Lexer.string_to_number(s)
        @fact n => 1.0e10
        @fact typeof(n) => Float64
        
        s = "-1E10"
        n = Lexer.string_to_number(s)
        @fact n => -1.0e10
        @fact typeof(n) => Float64

        s = "0x1p0"
        n = Lexer.string_to_number(s)
        @fact n => 1.0
        @fact typeof(n) => Float64

        s = "0x1.8p3"
        n = Lexer.string_to_number(s)
        @fact n => 12.0
        @fact typeof(n) => Float64

        s = "0x0.4p-1"
        n = Lexer.string_to_number(s)
        @fact n => 0.125
        @fact typeof(n) => Float64

        for _ = 1:10
            tn = rand()
            s  = string(tn)
            n  = Lexer.string_to_number(s)
            @fact n => tn
            @fact typeof(n) => Float64
        end
    end

    context("float32") do 
        s = "1.0f0"
        n = Lexer.string_to_number(s)
        @fact n => 1.0
        @fact typeof(n) => Float32

        s = "-1.f0"
        n = Lexer.string_to_number(s)
        @fact n => -1.0
        @fact typeof(n) => Float32

        s = "1f0"
        n = Lexer.string_to_number(s)
        @fact n => 1.0
        @fact typeof(n) => Float32

        s = "1f"
        @fact_throws Lexer.string_to_number(n)

        s = "0x1p0f0"
        n = Lexer.string_to_number(s)
        @fact n => 1.0
        @fact typeof(n) => Float32 

        s = "0x1.8p3f0"
        n = Lexer.string_to_number(s)
        @fact n => 12.0
        @fact typeof(n) => Float32

        s = "0x0.4p-1f0"
        n = Lexer.string_to_number(s)
        @fact n => 0.125
        @fact typeof(n) => Float32

        for _ = 1:10
            tn = rand(Float32)
            s  = repr(tn)
            n  = Lexer.string_to_number(s)
            @fact n => tn
            @fact typeof(n) => Float32
        end
    end

    context("integers") do
        s = "1"
        n = Lexer.string_to_number(s)
        @fact n => 1
        @fact typeof(n) => Uint64

        s = "-1"
        n = Lexer.string_to_number(s)
        @fact n => -1
        @fact typeof(n) => Int64

        s = string(typemin(Int64))
        n = Lexer.string_to_number(s)
        @fact n => typemin(Int64)
        @fact typeof(n) => Int64 

        s = string(typemax(Int64))
        n = Lexer.string_to_number(s)
        @fact n => typemax(Int64)
        @fact typeof(n) => Uint64

        s = string(typemax(Uint64))
        n = Lexer.string_to_number(s)
        @fact n => typemax(Uint64)
        @fact typeof(n) => Uint64

        s = "0b010101"
        n = Lexer.string_to_number(s)
        @fact n => 21
        @fact typeof(n) => Uint64
        
        s = "-0b010101"
        n = Lexer.string_to_number(s)
        @fact n => -21
        @fact typeof(n) => Int64
    
        s = "0x15"
        n = Lexer.string_to_number(s)
        @fact n => 21
        @fact typeof(n) => Uint64

        s = "-0x15"
        n = Lexer.string_to_number(s)
        @fact n => -21
        @fact typeof(n) => Int64
    end
end








