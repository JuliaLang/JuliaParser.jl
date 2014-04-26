using JuliaParser
using FactCheck

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


facts("test read operator") do
    for op in Lexer.operators
        str = " $(string(op)) "
        io = IOBuffer(str)
        _ = Lexer.readchar(io)
        c = Lexer.readchar(io)
        res = Lexer.read_operator(io, c)
        @fact res => op
    end
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
            @fact isnan(n) => isnan(eval(parse(s)))
        end
    end

    context("Inf") do
        for s in ("Inf", "+Inf", "-Inf")
            n = Lexer.string_to_number(s)
            @fact isinf(n) => true
            @fact isinf(n) => isinf(eval(parse(s)))
        end
    end

    context("float64") do
        s = "1.0"
        n = Lexer.string_to_number(s)
        @fact n => 1.0
        @fact n => parse(s)
        @fact typeof(n) => Float64
        @fact typeof(n) => typeof(parse(s))

        s = "-1.0"
        n = Lexer.string_to_number(s)
        @fact n => -1.0
        @fact n => parse(s)
        @fact typeof(n) => Float64
        @fact typeof(n) => typeof(parse(s))

        s = "1."
        n = Lexer.string_to_number(s)
        @fact n => 1.0
        @fact n => parse(s)
        @fact typeof(n) => Float64
        @fact typeof(n) => typeof(parse(s))

        s = "1e10"
        n = Lexer.string_to_number(s)
        @fact n => 1.0e10
        @fact n => parse(s)
        @fact typeof(n) => Float64
        @fact typeof(n) => typeof(parse(s))
        
        s = "-1E10"
        n = Lexer.string_to_number(s)
        @fact n => -1.0e10
        @fact n => parse(s)
        @fact typeof(n) => Float64
        @fact typeof(n) => typeof(parse(s))

        s = "0x1p0"
        n = Lexer.string_to_number(s)
        @fact n => 1.0
        @fact n => parse(s)
        @fact typeof(n) => Float64
        @fact typeof(n) => typeof(parse(s))

        s = "0x1.8p3"
        n = Lexer.string_to_number(s)
        @fact n => 12.0
        @fact n => parse(s)
        @fact typeof(n) => Float64
        @fact typeof(n) => typeof(parse(s))

        s = "0x0.4p-1"
        n = Lexer.string_to_number(s)
        @fact n => 0.125
        @fact n => parse(s)
        @fact typeof(n) => Float64
        @fact typeof(n) => typeof(parse(s))

        for _ = 1:10
            tn = rand()
            s  = string(tn)
            n  = Lexer.string_to_number(s)
            @fact n => tn
            @fact typeof(n) => Float64
            @fact n => parse(s)
            @fact typeof(n) => typeof(parse(s))
        end
    end

    context("float32") do 
        s = "1.0f0"
        n = Lexer.string_to_number(s)
        @fact n => 1.0
        @fact typeof(n) => Float32
        @fact n => parse(s)
        @fact typeof(n) => typeof(parse(s))

        s = "-1.f0"
        n = Lexer.string_to_number(s)
        @fact n => -1.0
        @fact typeof(n) => Float32
        @fact n => parse(s)
        @fact typeof(n) => typeof(parse(s))

        s = "1f0"
        n = Lexer.string_to_number(s)
        @fact n => 1.0
        @fact typeof(n) => Float32
        @fact n => parse(s)
        @fact typeof(n) => typeof(parse(s))

        s = "1f"
        @fact_throws Lexer.string_to_number(n)
        
        for _ = 1:10
            tn = rand(Float32)
            s  = repr(tn)
            n  = Lexer.string_to_number(s)
            @fact n => tn
            @fact typeof(n) => Float32
            @fact n => parse(s)
            @fact typeof(n) => typeof(parse(s))
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

        s = repr(typemin(Int64))
        n = Lexer.string_to_number(s)
        @fact n => typemin(Int64)
        @fact typeof(n) => Int64 

        s = repr(typemax(Int64))
        n = Lexer.string_to_number(s)
        @fact n => typemax(Int64)
        @fact typeof(n) => Uint64
        
        #=
        s = repr(typemax(Uint64))
        n = Lexer.string_to_number(s)
        @fact n => typemax(Uint64)
        @fact typeof(n) => Uint64
        =# 

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


facts("test uint neg") do
    n = eval(Lexer.fix_uint_neg(true,  1))
    p = eval(Lexer.fix_uint_neg(false, 1))
    @fact n => -1 
    @fact p =>  1 
end


facts("test sized uint literal") do
    
    context("hexadecimal") do
        s  = "0x0"
        sn = int(s)
        n  = Lexer.sized_uint_literal(sn, s, 4)
        @fact sn => n
        @fact typeof(n) => Uint8
        
        for ty in (Uint8, Uint16, Uint32, Uint64, Uint128)
            @eval begin
                s = repr(typemax($ty))
                sn = uint128(s)
                n  = Lexer.sized_uint_literal(sn, s, 4)
                @fact sn => n
                @fact typeof(n) => $ty
                # parse / eval output (128 bit integers and BigInts
                # are returned as expressions
                pn = eval(parse(s))
                @fact pn => n
                @fact typeof(pn) => $ty
            end
        end
        
        s  = string(repr(typemax(Uint128)), "f")
        sn = BigInt(s) 
        n  = Lexer.sized_uint_literal(sn, s, 4)
        @fact sn == n => true
        @fact typeof(n) => BigInt

        pn = eval(parse(s))
        @fact pn == n => true
        @fact typeof(pn) => BigInt
    end
    
    context("octal") do
        s  = "0o0"
        sn = int(s)
        n  = Lexer.sized_uint_oct_literal(sn, s)
        @fact sn => n
        @fact typeof(n) => Uint8
        pn = parse(s)
        @fact pn == n => true
        @fact typeof(n) => typeof(pn)

        for ty in (Uint8, Uint16, Uint32, Uint64, Uint128)
            @eval begin
                s = string("0o", oct(typemax($ty)))
                sn = uint128(s)
                n  = Lexer.sized_uint_oct_literal(sn, s)
                @fact sn => n
                @fact typeof(n) => $ty
                        
                pn = eval(parse(s))
                @fact pn => n
                @fact typeof(pn) => $ty
            end
        end
        
        s  = string(repr(typemax(Uint128)), "7")
        sn = BigInt(s) 
        n  = Lexer.sized_uint_oct_literal(sn, s)
        @fact sn => n
        @fact typeof(n) => BigInt
        pn = eval(parse(s))
        @fact n => pn
        @fact typeof(n) => typeof(pn)
    end

    context("binary") do
        s  = "0b0"
        sn = int(s)
        n  = Lexer.sized_uint_literal(sn, s, 1)
        @fact sn => n
        @fact typeof(n) => Uint8
        
        for ty in (Uint8, Uint16, Uint32, Uint64, Uint128)
            @eval begin
                s = string("0b", bin(typemax($ty)))
                sn = uint128(s)
                n  = Lexer.sized_uint_literal(sn, s, 1)
                @fact sn => n
                @fact typeof(n) => $ty
            end
        end
        
        s  = string("0b", bin(typemax(Uint128)), "1")
        sn = BigInt(s) 
        n  = Lexer.sized_uint_literal(sn, s, 1)
        @fact sn => n
        @fact typeof(n) => BigInt
    end
end

facts("test accum_digits") do
    io = IOBuffer("1_000_000")
    c = Lexer.readchar(io)
    pred = Lexer.is_char_numeric
    digits, success = Lexer.accum_digits(io, pred, c, false)
    @fact digits => "1000000"
    @fact success => true
   
    io = IOBuffer("01_000_000")
    c = Lexer.peekchar(io)
    pred = Lexer.is_char_numeric
    digits, success = Lexer.accum_digits(io, pred, c, false)
    @fact digits => "01000000"
    @fact success => true


    io = IOBuffer("_000_000")
    c = Lexer.peekchar(io)
    pred = Lexer.is_char_numeric
    _, success = Lexer.accum_digits(io, pred, c, false)
    @fact success => false

    io = IOBuffer("_000_000")
    c = Lexer.peekchar(io)
    pred = Lexer.is_char_numeric
    _, success = Lexer.accum_digits(io, pred, c, true)
    @fact success => true
end


facts("test compare num strings") do
    a = "123"
    b = "1453"
    isless = Lexer.compare_num_strings(a, b) 
    @fact isless => true

    a = "123"
    b = "321"
    isless  = Lexer.compare_num_strings(a, b)
    @fact isless => true
    isless  = Lexer.compare_num_strings(b, a)
    @fact isless => false
end

facts("test is oct within uint 128") do
    m = typemax(Uint128)
    @fact Lexer.is_oct_within_uint128(string("0o", oct(m))) => true

    m = typemax(Uint128) - 1 
    @fact Lexer.is_oct_within_uint128(string("0o", oct(m))) => true

    m = BigInt(typemax(Uint128)) + 1
    @fact Lexer.is_oct_within_uint128(string("0o", oct(m))) => false
end

facts("test is within int128") do
    m = typemax(Int128)
    @fact Lexer.is_within_int128(repr(m)) => true 

    m = BigInt(typemax(Int128)) + 1
    @fact Lexer.is_within_int128(repr(m)) => false

    m = typemin(Int128)
    @fact Lexer.is_within_int128(repr(m)) => true 

    m = BigInt(typemin(Int128)) - 1
    @fact Lexer.is_within_int128(repr(m)) => false
end


facts("test readnumber") do
    
    context("signed integer") do
        io = IOBuffer("100") 
        n = Lexer.read_number(io, false, false)
        @fact n => 100
        @fact typeof(n) => Uint

        io = IOBuffer("100_000_000")
        n = Lexer.read_number(io, false, false)
        @fact n => 100_000_000
        @fact typeof(n) => Uint

        io = IOBuffer("-100")
        skip(io, 1)
        n = Lexer.read_number(io, false, true)
        @fact n => -100
        @fact typeof(n) => Int

        io = IOBuffer("00100")
        n = Lexer.read_number(io, false,  false)
        @fact n => 100
        @fact typeof(n) => Uint
    end

    context("decimal") do
        io = IOBuffer("100.0")
        n = Lexer.read_number(io, false, false)
        @fact n => 100.0
        @fact typeof(n) => Float64
     
        io = IOBuffer("100.0f0")
        n = Lexer.read_number(io, false, false)
        @fact n => 100.0
        @fact typeof(n) => Float32

        io = IOBuffer("10.0.0")
        @fact_throws Lexer.read_number(io, false, false)
    end

    context("floating point exponent") do
        io = IOBuffer("1e10")
        n = Lexer.read_number(io, false, false)
        @fact n => 1e10
        @fact typeof(n) => Float64

        io = IOBuffer("-10E10")
        skip(io, 1)
        n = Lexer.read_number(io, false, true)
        @fact n => -10e10
        @fact typeof(n) => Float64

        io = IOBuffer("1e-1")
        n = Lexer.read_number(io, false, false)
        @fact n => 1e-1
        @fact typeof(n) => Float64
    end

    context("leading dot") do 
        io = IOBuffer(".01")
        skip(io, 1)
        n = Lexer.read_number(io, true, false)
        @fact n => 0.01
        @fact typeof(n) => Float64

        io = IOBuffer(".000_1")
        skip(io, 1)
        n = Lexer.read_number(io, true, false)
        @fact n => 0.0001
        @fact typeof(n) => Float64

        io = IOBuffer("-.01")
        skip(io, 2)
        n = Lexer.read_number(io, true, true)
        @fact n => -0.01
        @fact typeof(n) => Float64

        io = IOBuffer("-.000_1")
        skip(io, 2)
        n = Lexer.read_number(io, true, true)
        @fact n => -0.0001
        @fact typeof(n) => Float64

        io = IOBuffer(".01f0")
        skip(io, 1)
        n = Lexer.read_number(io, true, false)
        @fact n => 0.01f0
        @fact typeof(n) => Float32

        io = IOBuffer(".000_1f0")
        skip(io, 1)
        n = Lexer.read_number(io, true, false)
        @fact n => 0.0001f0
        @fact typeof(n) => Float32 

        io = IOBuffer("-.01f0")
        skip(io, 2)
        n = Lexer.read_number(io, true, true)
        @fact n => -0.01f0
        @fact typeof(n) => Float32

        io = IOBuffer("-.000_1f0")
        skip(io, 2)
        n = Lexer.read_number(io, true, true)
        @fact n => -0.0001f0
        @fact typeof(n) => Float32 
    end

    context("floating point hex") do
        io = IOBuffer("0x1.8p3")
        n = Lexer.read_number(io, false, false)
        @fact n => 12.0
        @fact typeof(n) => Float64

        io = IOBuffer("0x0.4p-1")
        n = Lexer.read_number(io, false, false)
        @fact n => 0.125
        @fact typeof(n) => Float64
    end

    context("binary") do
        io = IOBuffer(string("0b", bin(10), " "))
        n = Lexer.read_number(io, false, false)
        @fact n => 10
        #@show typeof(n)  
    end

    context("hex") do
        io = IOBuffer(string("0x", hex(10), " "))
        n = Lexer.read_number(io, false, false)
        @fact n => 10 
        #@show typeof(n)
    end
end


facts("test skipwhitespace") do
    io = IOBuffer("   abc")
    Lexer.skipwhitespace(io)
    @fact position(io) => 3

    io = IOBuffer("abc")
    Lexer.skipwhitespace(io)
    @fact position(io) => 0

    io = IOBuffer("  \n abc")
    Lexer.skipwhitespace(io)
    @fact position(io) => 2
    @fact Lexer.readchar(io) => '\n'

    io = IOBuffer("")
    Lexer.skipwhitespace(io)
    @fact position(io) => 0
end


facts("test skip comment") do
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

facts("test skip multiline comment") do
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

    io = IOBuffer("#=#==#=#")
    Lexer.skip_multiline_comment(io, 0)
    @fact position(io) => 8

    io = IOBuffer("#=#=#")
    @fact_throws Lexer.skip_multiline_comment(io, 0)

    io = IOBuffer("#= test")
    @fact_throws Lexer.skip_multiline_comment(io, 0)

    io = IOBuffer("#=#")
    @fact_throws Lexer.skip_multiline_comment(io, 0)
end

facts("test skip ws and comment") do
    io = IOBuffer("")
    Lexer.skip_ws_and_comments(io) 
    @fact eof(io) => true

    io = IOBuffer(" \n")
    Lexer.skip_ws_and_comments(io) 
    @fact Lexer.readchar(io) => '\n'

    io = IOBuffer("  # test comment\n")
    Lexer.skip_ws_and_comments(io)
    @fact eof(io) => true

    io = IOBuffer("    #= test comment \n
                  another comment =#a")
    Lexer.skip_ws_and_comments(io)
    @fact Lexer.readchar(io) => 'a'

    io = IOBuffer(" # a comment\ntest")
    Lexer.skip_ws_and_comments(io)
    @fact Lexer.readchar(io) => 't'
end

function tokens(io::IO)
    @task while !eof(io)
        produce(Lexer.next_token(io, nothing))
    end
end

facts("test next_token") do
    # throw EOF error
    io  = IOBuffer("")
    @fact Lexer.next_token(io, nothing) => nothing

    io  = IOBuffer("\n")
    tok = Lexer.next_token(io, nothing)
    @fact tok => '\n'

    toks = collect(tokens(IOBuffer("(test,)")))
    @fact toks => {'(', :test, ',', ')'}

    toks = collect(tokens(IOBuffer("[10.0,2.0]")))
    @fact toks => {'[', 10.0, ',', 2.0, ']'}

    toks = collect(tokens(IOBuffer("#test\n{10,};")))
    @fact toks => {'{', 10, ',', '}', ';'}

    toks = collect(tokens(IOBuffer("#=test1\ntest2\n=#@test\n")))
    @fact toks => {'@', :test, '\n'}

    toks = collect(tokens(IOBuffer("1<=2")))
    @fact toks => {1, :(<=), 2}

    toks = collect(tokens(IOBuffer("1.0 .+ 2")))
    @fact toks => {1.0, :(.+), 2}

    toks = collect(tokens(IOBuffer("abc .+ .1")))
    @fact toks => {:abc, :(.+), 0.1}

    toks = collect(tokens(IOBuffer("`ls`")))
    @fact toks => {'`', :ls, '`'}

    toks = collect(tokens(IOBuffer("@testmacro")))
    @fact toks => {'@', :testmacro}

    toks = collect(tokens(IOBuffer("func(2) |> send!")))
    @fact toks => {:func, '(', 2, ')', :(|>), :(send!)}

    sym_end = symbol("end")
    
    toks = collect(tokens(IOBuffer("type Test{T<:Int32}\n\ta::T\n\tb::T\nend")))
    @fact toks => {:type, :Test, '{',  :T, :(<:), :Int32, '}', '\n',
                   :a, :(::), :T, '\n', 
                   :b, :(::), :T, '\n', 
                   sym_end} 

    toks = collect(tokens(IOBuffer("function(x::Int)\n\treturn x^2\nend")))
    @fact toks => {:function, '(', :x, :(::), :Int, ')', '\n',
                   :return , :x, :(^), uint(2), '\n',
                   sym_end}

end
