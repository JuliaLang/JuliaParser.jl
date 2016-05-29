using JuliaParser
using FactCheck
using Compat

const Lexer = JuliaParser.Lexer
const TokenStream = JuliaParser.Lexer.TokenStream
const utf8sizeof = Lexer.utf8sizeof

using Lexer: ¬

facts("test skip to end of line") do
    io = IOBuffer("abcd\nabcd\n")
    Lexer.skip_to_eol(io)
    @fact position(io) --> 4
    skip(io, 3)
    @fact position(io) --> 7
    Lexer.skip_to_eol(io)
    @fact position(io) --> 9
    Lexer.readchar(io)
    @fact eof(io) --> true

    context("no line break in buffer") do
        io = IOBuffer("abcde")
        Lexer.skip_to_eol(io)
        @fact position(io) --> 5
        @fact eof(io) --> true
    end

    context("empty buffer") do
        io = IOBuffer()
        Lexer.skip_to_eol(io)
        @fact eof(io) --> true
    end
end

facts("test read operator") do
    for op in Lexer.OPERATORS
        str = "$(string(op))"
        io = TokenStream(str)
        c = Lexer.readchar(io)
        res = Lexer.read_operator(io, c, nothing)
        @fact res --> op
        @fact Lexer.eof(io) --> true
    end
end

facts("test string_to_number") do

    @fact_throws Lexer.string_to_number("",nothing)
    @fact_throws Lexer.string_to_number("1x10",nothing)
    @fact_throws Lexer.string_to_number("x10",nothing)
    @fact_throws Lexer.string_to_number("1.f.10",nothing)
    @fact_throws Lexer.string_to_number("b10",nothing)
    @fact_throws Lexer.string_to_number("0xzz",nothing)
    @fact_throws Lexer.string_to_number("0b22",nothing)

    context("NaN") do
        for s in ("NaN", "+NaN", "-NaN")
            n = Lexer.string_to_number(s,nothing)
            @fact isnan(n) --> true
            @fact isnan(n) --> isnan(eval(parse(s)))
        end
    end

    context("Inf") do
        for s in ("Inf", "+Inf", "-Inf")
            n = Lexer.string_to_number(s,nothing)
            @fact isinf(n) --> true
            @fact isinf(n) --> isinf(eval(parse(s)))
        end
    end

    context("float64") do
        s = "1.0"
        n = Lexer.string_to_number(s,nothing)
        @fact n --> 1.0
        @fact n --> parse(s)
        @fact typeof(n) --> Float64
        @fact typeof(n) --> typeof(parse(s))

        s = "-1.0"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> -1.0
        @fact n --> parse(s)
        @fact typeof(n) --> Float64
        @fact typeof(n) --> typeof(parse(s))

        s = "1."
        n = Lexer.string_to_number(s, nothing)
        @fact n --> 1.0
        @fact n --> parse(s)
        @fact typeof(n) --> Float64
        @fact typeof(n) --> typeof(parse(s))

        s = "1e10"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> 1.0e10
        @fact n --> parse(s)
        @fact typeof(n) --> Float64
        @fact typeof(n) --> typeof(parse(s))

        s = "-1E10"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> -1.0e10
        @fact n --> parse(s)
        @fact typeof(n) --> Float64
        @fact typeof(n) --> typeof(parse(s))

        s = "0x1p0"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> 1.0
        @fact n --> parse(s)
        @fact typeof(n) --> Float64
        @fact typeof(n) --> typeof(parse(s))

        s = "0x1.8p3"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> 12.0
        @fact n --> parse(s)
        @fact typeof(n) --> Float64
        @fact typeof(n) --> typeof(parse(s))

        s = "0x0.4p-1"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> 0.125
        @fact n --> parse(s)
        @fact typeof(n) --> Float64
        @fact typeof(n) --> typeof(parse(s))

        for _ = 1:10
            tn = rand()
            s  = string(tn)
            n  = Lexer.string_to_number(s, nothing)
            @fact n --> tn
            @fact typeof(n) --> Float64
            @fact n --> parse(s)
            @fact typeof(n) --> typeof(parse(s))
        end
    end

    context("float32") do
        s = "1.0f0"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> 1.0
        @fact typeof(n) --> Float32
        @fact n --> parse(s)
        @fact typeof(n) --> typeof(parse(s))

        s = "-1.f0"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> -1.0
        @fact typeof(n) --> Float32
        @fact n --> parse(s)
        @fact typeof(n) --> typeof(parse(s))

        s = "1f0"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> 1.0
        @fact typeof(n) --> Float32
        @fact n --> parse(s)
        @fact typeof(n) --> typeof(parse(s))

        s = "1f"
        @fact_throws Lexer.string_to_number(n, nothing)

        for _ = 1:10
            tn = rand(Float32)
            s  = repr(tn)
            n  = Lexer.string_to_number(s, nothing)
            @fact n --> tn
            @fact typeof(n) --> Float32
            @fact n --> parse(s)
            @fact typeof(n) --> typeof(parse(s))
        end
    end

    context("integers") do
        s = "1"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> 1
        @fact typeof(n) --> Int64

        s = "-1"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> -1
        @fact typeof(n) --> Int64

        s = repr(typemin(Int64))
        n = Lexer.string_to_number(s, nothing)
        @fact n --> typemin(Int64)
        @fact typeof(n) --> Int64

        s = repr(typemax(Int64))
        n = Lexer.string_to_number(s, nothing)
        @fact n --> typemax(Int64)
        @fact typeof(n) --> Int64

        #=
        s = repr(typemax(UInt64))
        n = Lexer.string_to_number(s)
        @fact n --> typemax(UInt64)
        @fact typeof(n) --> UInt64
        =#

        s = "0b010101"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> 21
        @fact typeof(n) --> Int64

        s = "-0b010101"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> -21
        @fact typeof(n) --> Int64

        s = "0x15"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> 21
        @fact typeof(n) --> Int64

        s = "-0x15"
        n = Lexer.string_to_number(s, nothing)
        @fact n --> -21
        @fact typeof(n) --> Int64
    end
end


facts("test is char hex") do
    for i = 1:9
        @fact Lexer.is_char_hex(first("$i")) --> true
    end
    for c in ['a', 'b', 'c', 'd', 'e', 'f']
        @fact Lexer.is_char_hex(c) --> true
    end
    @fact Lexer.is_char_hex('z') --> false
    for c in ['A', 'B', 'C', 'D', 'E', 'F']
        @fact Lexer.is_char_hex(c) --> true
    end
    @fact Lexer.is_char_hex('Z') --> false
end


facts("test is char oct") do
    for i = 1:9
        if i < 8
            @fact Lexer.is_char_oct(first("$i")) --> true
        else
	        @fact Lexer.is_char_oct(first("$i")) --> false
        end
    end
end


facts("test is char bin") do
    @fact Lexer.is_char_bin('0') --> true
    @fact Lexer.is_char_bin('1') --> true
    @fact Lexer.is_char_bin('2') --> false
end


facts("test uint neg") do
    @fact eval(Lexer.fix_uint_neg(true,  1)) --> -1
    @fact eval(Lexer.fix_uint_neg(false, 1)) --> 1
end


facts("test sized uint literal") do

    context("hexadecimal") do
        s  = "0x0"
        sn = @compat parse(Int,s)
        n  = Lexer.sized_uint_literal(s, 4)
        @fact sn --> n
        @fact typeof(n) --> UInt8

        for ty in (UInt8, UInt16, UInt32, UInt64, UInt128)
            @eval begin
                s = repr(typemax($ty))
                sn = @compat parse(UInt128,s)
                n  = Lexer.sized_uint_literal(s, 4)
                @fact sn --> n
                @fact typeof(n) --> $ty
                # parse / eval output (128 bit integers and BigInts
                # are returned as expressions
                pn = eval(parse(s))
                @fact pn --> n
                @fact typeof(pn) --> $ty
            end
        end

        if VERSION < v"0.4-"
            s  = string(repr(typemax(UInt128)), "f")
            sn = BigInt(s)
            n  = Lexer.sized_uint_literal(s, 4)
            @fact sn == n --> true
            @fact typeof(n) --> BigInt

            pn = eval(parse(s))
            @fact pn == n --> true
            @fact typeof(pn) --> BigInt
        end
    end

    context("octal") do
        s  = "0o0"
        sn = @compat parse(Int,s)
        n  = Lexer.sized_uint_oct_literal(s)
        @fact sn --> n
        @fact typeof(n) --> UInt8

        pn = parse(s)
        @fact pn == n --> true
        @fact typeof(n) --> typeof(pn)

        for ty in (UInt8, UInt16, UInt32, UInt64, UInt128)
            @eval begin
                s = string("0o", oct(typemax($ty)))
                sn = @compat parse(UInt128,s)
                n  = Lexer.sized_uint_oct_literal(s)
                @fact sn --> n
                @fact typeof(n) --> $ty

                pn = eval(parse(s))
                @fact pn --> n
                @fact typeof(pn) --> $ty
            end
        end

        if VERSION < v"0.4-"
            s = "0o" * oct(typemax(UInt128)) * "7"
            sn = BigInt(s)
            n  = Lexer.sized_uint_oct_literal(s)
            @fact sn --> n
            @fact typeof(n) --> BigInt
            pn = eval(parse(s))
            @fact n --> pn
            @fact typeof(n) --> typeof(pn)
        end
    end

    context("binary") do
        s  = "0b0"
        sn = @compat parse(Int,s)
        n  = Lexer.sized_uint_literal(s, 1)
        @fact sn --> n
        @fact typeof(n) --> UInt8

        for ty in (UInt8, UInt16, UInt32, UInt64, UInt128)
            @eval begin
                s = string("0b", bin(typemax($ty)))
                sn = @compat parse(UInt128,s)
                n  = Lexer.sized_uint_literal(s, 1)
                @fact sn --> n
                @fact typeof(n) --> $ty
            end
        end

        s  = string("0b", bin(typemax(UInt128)), "1")
        sn = @compat parse(BigInt,s)
        n  = Lexer.sized_uint_literal(s, 1)
        @fact sn --> n
        @fact typeof(n) --> BigInt
    end
end

facts("test accum_digits") do
    pred = isdigit

    io = TokenStream("1_000_000")
    c = Lexer.readchar(io)
    digits, success = Lexer.accum_digits(io, pred, c, false)
    @fact digits --> [c for c in "1000000"]
    @fact success --> true

    io = TokenStream("01_000_000")
    c = Lexer.peekchar(io)
    digits, success = Lexer.accum_digits(io, pred, c, false)
    @fact digits --> [c for c in "01000000"]
    @fact success --> true


    io = TokenStream("_000_000")
    c = Lexer.peekchar(io)
    _, success = Lexer.accum_digits(io, pred, c, false)
    @fact success --> false

    io = TokenStream("_000_000")
    c = Lexer.peekchar(io)
    _, success = Lexer.accum_digits(io, pred, c, true)
    @fact success --> true
end


facts("test compare num strings") do
    a = "123"
    b = "1453"
    isless = Lexer.compare_num_strings(a, b)
    @fact isless --> true

    a = "123"
    b = "321"
    isless  = Lexer.compare_num_strings(a, b)
    @fact isless --> true
    isless  = Lexer.compare_num_strings(b, a)
    @fact isless --> false
end

facts("test is oct within uint 128") do
    m = typemax(UInt128)
    @fact Lexer.is_oct_within_uint128(string("0o", oct(m))) --> true

    m = typemax(UInt128) - 1
    @fact Lexer.is_oct_within_uint128(string("0o", oct(m))) --> true

    m = BigInt(typemax(UInt128)) + 1
    @fact Lexer.is_oct_within_uint128(string("0o", oct(m))) --> false
end

facts("test is within int128") do
    m = typemax(Int128)
    @fact Lexer.is_within_int128(repr(m)) --> true

    m = BigInt(typemax(Int128)) + 1
    @fact Lexer.is_within_int128(repr(m)) --> false

    m = typemin(Int128)
    @fact Lexer.is_within_int128(repr(m)) --> true

    m = BigInt(typemin(Int128)) - 1
    @fact Lexer.is_within_int128(repr(m)) --> false
end


facts("test readnumber") do

    context("signed integer") do
        io = TokenStream("100")
        n = ¬Lexer.read_number(io, false, false)
        @fact n --> 100
        @fact typeof(n) --> Int64

        io = TokenStream("100_000_000")
        n = ¬Lexer.read_number(io, false, false)
        @fact n --> 100_000_000
        @fact typeof(n) --> Int64

        io = TokenStream("-100")
        Lexer.skip(io, 1)
        n = ¬Lexer.read_number(io, false, true)
        @fact n --> -100
        @fact typeof(n) --> Int64

        io = TokenStream("00100")
        n = ¬Lexer.read_number(io, false,  false)
        @fact n --> 100
        @fact typeof(n) --> Int64
    end

    context("decimal") do
        io = TokenStream("100.0")
        n = ¬Lexer.read_number(io, false, false)
        @fact n --> 100.0
        @fact typeof(n) --> Float64

        io = TokenStream("100.0f0")
        n = ¬Lexer.read_number(io, false, false)
        @fact n --> 100.0
        @fact typeof(n) --> Float32

        io = TokenStream("10.0.0")
        @fact_throws Lexer.read_number(io, false, false)
    end

    context("floating point exponent") do
        io = TokenStream("1e10")
        n = ¬Lexer.read_number(io, false, false)
        @fact n --> 1e10
        @fact typeof(n) --> Float64

        io = TokenStream("-10E10")
        Lexer.skip(io, 1)
        n = ¬Lexer.read_number(io, false, true)
        @fact n --> -10e10
        @fact typeof(n) --> Float64

        io = TokenStream("1e-1")
        n = ¬Lexer.read_number(io, false, false)
        @fact n --> 1e-1
        @fact typeof(n) --> Float64
    end

    context("leading dot") do
        io = TokenStream(".01")
        Lexer.skip(io, 1)
        n = ¬Lexer.read_number(io, true, false)
        @fact n --> 0.01
        @fact typeof(n) --> Float64

        io = TokenStream(".000_1")
        Lexer.skip(io, 1)
        n = ¬Lexer.read_number(io, true, false)
        @fact n --> 0.0001
        @fact typeof(n) --> Float64

        io = TokenStream("-.01")
        Lexer.skip(io, 2)
        n = ¬Lexer.read_number(io, true, true)
        @fact n --> -0.01
        @fact typeof(n) --> Float64

        io = TokenStream("-.000_1")
        Lexer.skip(io, 2)
        n = ¬Lexer.read_number(io, true, true)
        @fact n --> -0.0001
        @fact typeof(n) --> Float64

        io = TokenStream(".01f0")
        Lexer.skip(io, 1)
        n = ¬Lexer.read_number(io, true, false)
        @fact n --> 0.01f0
        @fact typeof(n) --> Float32

        io = TokenStream(".000_1f0")
        Lexer.skip(io, 1)
        n = ¬Lexer.read_number(io, true, false)
        @fact n --> 0.0001f0
        @fact typeof(n) --> Float32

        io = TokenStream("-.01f0")
        Lexer.skip(io, 2)
        n = ¬Lexer.read_number(io, true, true)
        @fact n --> -0.01f0
        @fact typeof(n) --> Float32

        io = TokenStream("-.000_1f0")
        Lexer.skip(io, 2)
        n = ¬Lexer.read_number(io, true, true)
        @fact n --> -0.0001f0
        @fact typeof(n) --> Float32
    end

    context("floating point hex") do
        io = TokenStream("0x1.8p3")
        n = ¬Lexer.read_number(io, false, false)
        @fact n --> 12.0
        @fact typeof(n) --> Float64

        io = TokenStream("0x0.4p-1")
        n = ¬Lexer.read_number(io, false, false)
        @fact n --> 0.125
        @fact typeof(n) --> Float64
    end

    context("binary") do
        io = TokenStream(string("0b", bin(10)))
        n = ¬Lexer.read_number(io, false, false)
        @fact n --> 10
    end

    context("hex") do
        io = TokenStream(string("0x", hex(10), " "))
        n = ¬Lexer.read_number(io, false, false)
        @fact n --> 10

        io = TokenStream("0xffff7f000001")
        n = ¬Lexer.read_number(io, false, false)
        @fact n --> 0xffff7f000001
    end

    context("bigint") do
        io = TokenStream("15732444386908125794514089057706229429197107928209")
        n  = ¬Lexer.read_number(io, false, false)
        @fact n --> @compat parse(BigInt, "15732444386908125794514089057706229429197107928209")
        @fact typeof(n) --> BigInt
    end
end

facts("test skipwhitespace") do
    io = TokenStream("   abc")
    Lexer.skipws(io)
    @fact Lexer.position(io) --> 3

    io = TokenStream("abc")
    Lexer.skipws(io)
    @fact Lexer.position(io) --> 0

    io = TokenStream("  \n abc")
    Lexer.skipws(io)
    @fact Lexer.position(io) --> 2
    @fact Lexer.readchar(io) --> '\n'

    io = TokenStream("")
    Lexer.skipws(io)
    @fact Lexer.position(io) --> 0
end


facts("test skip comment") do
    io = TokenStream("#test\n")
    Lexer.skipcomment(io)
    @fact Lexer.position(io) --> 5

    io = TokenStream("# \ntest")
    Lexer.skipcomment(io)
    @fact Lexer.position(io) --> 2

    io = TokenStream("#")
    Lexer.skipcomment(io)
    @fact Lexer.position(io) --> 1

    io = TokenStream("# ")
    Lexer.skipcomment(io)
    @fact Lexer.position(io) --> 2

    context("must start with a comment symbol") do
        io = TokenStream("test")
        @fact_throws Lexer.skipcomment(io)
    end
end

facts("test skip multiline comment") do
    io = TokenStream("#=test=#a")
    Lexer.skip_multiline_comment(io, 0)
    @fact Lexer.position(io) --> 8

    io = TokenStream("#======#a")
    Lexer.skip_multiline_comment(io, 0)
    @fact Lexer.position(io) --> 8

    io = TokenStream("#==#a")
    Lexer.skip_multiline_comment(io, 0)
    @fact Lexer.position(io) --> 4

    io = TokenStream("#=test\ntest\n=#a")
    Lexer.skip_multiline_comment(io, 0)
    @fact Lexer.position(io) --> 14

    io = TokenStream("#= # =#")
    Lexer.skip_multiline_comment(io, 0)
    @fact Lexer.position(io) --> 7

    io = TokenStream("#=\n#= =# =#")
    Lexer.skip_multiline_comment(io, 0)
    @fact Lexer.position(io) --> 11

    io = TokenStream("#=#==#=#")
    Lexer.skip_multiline_comment(io, 0)
    @fact Lexer.position(io) --> 8

    io = TokenStream("#=#=#")
    @fact_throws Lexer.skip_multiline_comment(io, 0)

    io = TokenStream("#= test")
    @fact_throws Lexer.skip_multiline_comment(io, 0)

    io = TokenStream("#=#")
    @fact_throws Lexer.skip_multiline_comment(io, 0)
end

facts("test skip ws and comment") do
    io = TokenStream("")
    Lexer.skipws_and_comments(io)
    @fact Lexer.eof(io) --> true

    io = TokenStream(" \n")
    Lexer.skipws_and_comments(io)
    @fact Lexer.eof(io) --> true

    io = TokenStream("  # test comment\n")
    Lexer.skipws_and_comments(io)
    @fact Lexer.eof(io) --> true

    io = TokenStream("    #= test comment \n
                  another comment =#a")
    Lexer.skipws_and_comments(io)
    @fact Lexer.readchar(io) --> 'a'

    io = TokenStream(" # a comment\ntest")
    Lexer.skipws_and_comments(io)
    @fact Lexer.readchar(io) --> 't'
end

tokens(ts::TokenStream) = begin
    toks = Any[]
    while !eof(ts.io)
        push!(toks, Lexer.next_token(ts))
    end
    return toks
end
tokens(str::AbstractString) = tokens(TokenStream(str))
raw_tokens(s) = map(¬,tokens(s))

facts("test TokenStream constructor") do
    io = TokenStream("testfunc(i) = i * i")
    try
        ts = TokenStream(io)
        @fact true --> true
    catch
        @fact false --> false
    end
end

facts("test set_token! / last_token") do
    code = "1 + 1"

    tks = tokens(code)
    @fact map(¬,tks) --> Any[1, :+, 1]

    ts = TokenStream(code)
    @fact ¬Lexer.last_token(ts) --> nothing

    Lexer.set_token!(ts, tks[1])
    @fact Lexer.last_token(ts) --> tks[1]

    Lexer.set_token!(ts, tks[2])
    @fact Lexer.last_token(ts) --> tks[2]
end

facts("test put_back!") do
    code = "1 + 1"
    tks  = tokens(code)
    ts   = TokenStream(code)
    Lexer.put_back!(ts, tks[1])
    @fact ts.putback --> tks[1]
    @fact_throws ts.put_back(tks[2])
end

facts("test peek_token") do
    code = "1 + 1"
    tks  = tokens(code)
    ts   = TokenStream(code)
    @fact Lexer.peek_token(ts) --> tks[1]
    Lexer.put_back!(ts, Lexer.Token(:test))
    @fact ¬Lexer.peek_token(ts) --> :test
    @fact_throws Lexer.put_back!(ts, Lexer.Token(:test2))
    Lexer.set_token!(ts, Lexer.Token(:test2))
    @fact ¬Lexer.peek_token(ts) --> :test
end

# you must peek before you can take
facts("test take_token") do
    code = "1 + 1"
    ts   = TokenStream(code)
    @fact Lexer.take_token(ts) --> nothing
    for t in tokens(code)
        tk = Lexer.peek_token(ts)
        @fact tk --> t
        Lexer.take_token(ts)
    end
end

facts("test require_token") do
    code = "1 +\n1"
    ts   = TokenStream(code)
    for t in (1, :+, 1)
        tk = Lexer.require_token(ts)
        @fact ¬tk --> t
        Lexer.take_token(ts)
    end
end

facts("test next_token") do
    # throw EOF error
    ts  = Lexer.TokenStream("")
    @fact Lexer.next_token(ts) --> Lexer.eof_token(Lexer.Token)

    ts  = Lexer.TokenStream("\n")
    tok = Lexer.next_token(ts)
    @fact ¬tok --> '\n'

    toks = raw_tokens("true false")
    @fact toks --> Any[true, false]

    toks = raw_tokens("(test,)")
    @fact toks --> Any['(', :test, ',', ')']

    toks = raw_tokens("[10.0,2.0]")
    @fact toks --> Any['[', 10.0, ',', 2.0, ']']

    toks = raw_tokens("#test\n{10,};")
    @fact toks --> Any['\n', '{', 10, ',', '}', ';']

    toks = raw_tokens("#=test1\ntest2\n=#@test\n")
    @fact toks --> Any['@', :test, '\n']

    toks = raw_tokens("1<=2")
    @fact toks --> Any[1, :(<=), 2]

    toks = raw_tokens("1.0 .+ 2")
    @fact toks --> Any[1.0, :(.+), 2]

    toks = raw_tokens("abc .+ .1")
    @fact toks --> Any[:abc, :(.+), 0.1]

    toks = raw_tokens("`ls`")
    @fact toks --> Any['`', :ls, '`']

    toks = raw_tokens("@testmacro")
    @fact toks --> Any['@', :testmacro]

    toks = raw_tokens("x::Int32 + 1")
    @fact toks --> Any[:x, :(::), :Int32, :(+), 1]

    toks = raw_tokens("func(2) |> send!")
    @fact toks --> Any[:func, '(', 2, ')', :(|>), :(send!)]

    sym_end = Symbol("end")

    toks = raw_tokens("type Test{T<:Int32}\n\ta::T\n\tb::T\nend")
    @fact toks --> Any[:type, :Test, '{',  :T, :(<:), :Int32, '}', '\n',
                   :a, :(::), :T, '\n',
                   :b, :(::), :T, '\n',
                   sym_end]

    toks = raw_tokens("function(x::Int)\n\treturn x^2\nend")
    @fact toks --> Any[:function, '(', :x, :(::), :Int, ')', '\n',
                   :return , :x, :(^), 2, '\n',
                   sym_end]

    toks = raw_tokens("+(x::Bool, y::Bool) = int(x) + int(y)")
    @fact toks --> Any[:+, '(', :x, :(::), :Bool, ',', :y, :(::), :Bool, ')',
                   :(=), :int, '(', :x, ')', :+, :int, '(', :y, ')']
end

facts("test utf8sizeof") do
    @fact utf8sizeof('a') --> 1
    @fact utf8sizeof('\uff') --> 2
    @fact utf8sizeof('\uffff') --> 3
    @fact utf8sizeof('\U1f596') --> 4
    @fact utf8sizeof(@compat Char(0x110000)) --> 3
end

facts("test windows style newline") do
    toks = raw_tokens("1+#= =#\t\r\n2")
    @fact toks --> Any[1, :+, '\n', 2]
end
