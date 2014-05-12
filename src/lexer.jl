# Lexer for Julia

module Lexer

# we need to special case this becasue otherwise Julia's 
# parser thinks this is a quote node
const rsubtype = symbol(":>")
const lsubtype = symbol("<:")

const ops_by_precedent =  {
               [:(=),   :(:=),   :(+=),   :(-=),  :(*=),
                :(/=),  :(//=),  :(.//=), :(.*=), :(./=),
                :(\=),  :(.\=),  :(^=),   :(.^=), :(%=),
                :(.%=), :(|=),   :(&=),   :($=),  :(=>),
                :(<<=), :(>>=),  :(>>>=), :(~),   :(.+=),
                :(.-=)],
               [:(?)],
               [:(||)],
               [:(&&)],
               [:(--), :(-->)],
               [:(>),   :(<),   :(>=),  :(<=),  :(==), 
                :(===), :(!=),  :(!==), :(.>),  :(.<),
                :(.>=), :(.<=), :(.==), :(.!=), :(.=),
                :(.!),  lsubtype,  rsubtype],
               [:(|>),  :(<|)],
               [:(:), :(..)],
               [:(+),  :(-),  :(.+),  :(.-),  :(|),   :($)],
               [:(<<), :(>>), :(>>>), :(.<<), :(.>>), :(.>>>)],
               [:(*),  :(/),  :(./),  :(%),   :(.%),  :(&), :(.*), :(\), :(.\)],
               [:(//), :(.//)],
               [:(^),  :(.^)],
               [:(::)],
               [:(.)]}

precedent_ops(n::Integer) = ops_by_precedent[n]::Vector{Symbol}

const assignment_ops = ops_by_precedent[1]::Vector{Symbol}

const unary_ops = Set{Symbol}([:(+), :(-), :(!), :(~), :(<:), :(>:)])

const unary_and_binary_ops = Set{Symbol}([:(+), :(-), :($), :(&), :(~)])

# Operators are special forms, not function names
const syntactic_ops = Set{Symbol}([:(=),   :(:=),  :(+=),   :(-=),  :(*=), 
                                   :(/=),  :(//=), :(./=),  :(.*=), :(./=),  
                                   :(\=),  :(.\=), :(^=),   :(.^=), :(%=),
                                   :(.%=), :(|=),  :(&=),   :($=),  :(=>),
                                   :(<<=), :(>>=), :(>>>=), :(->),  :(-->),
                                   :(||),   :(&&),  :(::),   :(.),   :(...),
                                   :(.+=), :(.-=)])

const syntactic_unary_ops = Set{Symbol}([:($), :(&)])

const transpose_op  = symbol(".'")
const ctranspose_op = symbol("'")
const vararg_op     = :(...)

const operators = union(Set([:(~), :(!), :(->), ctranspose_op, transpose_op, vararg_op]), 
			                [Set(ops) for ops in ops_by_precedent]...)

const reserved_words = Set{Symbol}([:begin, :while, :if, :for, :try, :return,
                                    :break, :continue, :function, :macro, :quote,
                                    :let, :local, :global, :const, :abstract,
                                    :typealias, :type, :bitstype, :immutable, :ccall,
                                    :do, :module, :baremodule, :using, :import,
                                    :export, :importall])


#= Helper functions =#
is_assignment(ex::Expr) = ex.head === :(=) && length(ex.args) == 1
is_assignment(ex) = false

is_assignment_like(ex) = length(expr) == 2 && in(first(expr), assignment_ops)

is_kwarg(l) = length(ex) == 2 && first(l) == :(kw)

is_syntactic_op(op::Symbol) = in(op, syntactic_ops)
is_syntactic_op(op) = false

is_syntactic_unary_op(op::Symbol) = in(op, syntactic_unary_ops)
is_syntactic_unary_op(op) = false

is_dict_literal(l) = length(l) == 3 && first(l) === :(=>) 

const is_special_char = let 
    chars = Set{Char}("()[]{},;\"`@")
    is_special_char(c::Char)  = in(c, chars)
end

isnewline(c::Char) = c === '\n''
isnewline(c) = false

function is_identifier_char(c::Char)
   return (('A' <= c <= 'Z') ||
           ('a' <= c <= 'z') ||
           ('0' <= c <= '0') ||
           ('\uA1' <= c) ||
           ('\_' === c))
end 

#= Characters that can be in an operator =#
const operator_chars = union([Set(string(op)) for op in operators]...)

is_opchar(c::Char) = in(c, operator_chars)

#= Characters that can follow a . in an operator =#
const is_dot_opchar = let chars = Set{Char}(".*^/\\+-'<>!=%")
    is_dot_opchar(c::Char) = in(c, chars)
end

is_operator(op::Symbol) = in(op, operators)
is_operator(op) = false

#= Implement peekchar for IOBuffer and IOStream =#

const EOF = char(-1)

# modified version from Base to give the same
# semantics as the IOStream implementation

function peekchar(io::IOBuffer)
    if !io.readable || io.ptr > io.size
        return EOF
    end
    ch = uint8(io.data[io.ptr])
    if ch < 0x80
        return char(ch)
    end
    # mimic utf8.next function
    trailing = Base.utf8_trailing[ch+1]
    c::Uint32 = 0
    for j = 1:trailing
        c += ch
        c <<= 6
        ch = uint8(io.data[io.ptr+j])
    end
    c += ch
    c -= Base.utf8_offset[trailing+1]
    return char(c)
end


# this implementation is copied from Base
const peekchar = let chtmp = Array(Char, 1)
    peekchar(s::IOStream) = begin
        if ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Char}), s, chtmp) < 0
            return EOF
        end
        return chtmp[1]
    end
end

eof(io::IO)  = Base.eof(io)
eof(c::Char) = is(c, EOF)
eof(c) = false

readchar(io::IO) = eof(io) ? EOF : read(io, Char)
takechar(io::IO) = begin
    readchar(io)
    return io
end

#= Lexer =#

function skip_to_eol(io::IO)
   while !eof(io)
       readchar(io) == '\n' && break
    end
    return io
end

function read_operator(io::IO, c::Char)
    pc = peekchar(io)
    if (c == '*') && (pc == '*')
        error("use \"^\" instead of \"**\"")
    end
    # 1 char operator
    if eof(pc) || !is_opchar(pc)
        return symbol(c)
    end
    str = Char[c]
    c   = pc 
    while !eof(c) && is_opchar(c)
        push!(str, c)
        newop = utf32(str)
        opsym = symbol(newop)
        if is_operator(opsym) #|| opsym === :(//)
            skip(io, 1)
            c = peekchar(io)
            continue
        #TODO: this is wrong
        elseif c == '.'
            skip(io, 1)
        end
        break
    end
    return symbol(utf32(str))
end


#=============#
# Read Number
#=============#

# Notes:
# expressions starting with 0x are always hexadecimal literals
# expressiosn starting with a numeric literal followed by e or E
# are always floating point literals

function string_to_number(tok::String)
    len = length(tok)
    len > 0 || error("invalid number token \"$tok\"")
    # NaN and Infinity
    (tok == "NaN" || tok == "+NaN" || tok == "-NaN") && return NaN
    (tok == "Inf" || tok == "+Inf" || tok == "-Inf") && return Inf
    # XXX: Overflow checking?
    # floating point literals
    is_float64 = false
    is_float32 = false
    didx, fidx = 0, 0
    for i=1:len 
        c = tok[i]
        if c == '.'
            didx = i
            is_float64 = true
        end
        is_float64 = is_float64 || c == 'e' || c == 'E' || c == 'p' || c == 'P'
        if c == 'f'
            is_float32 = i > didx && i != len ? 
                true : error("invalid float32 token \"$tok\"")
            fidx = i
        end
    end
    if is_float32
        #TODO: there must be a better way to do this
        base = float32(tok[1:fidx-1])
        expn = int(tok[fidx+1:end])
        return base * 10.f0 ^ expn
    elseif is_float64
        return float64(tok)
    end
    # parse signed / unsigned integers
    if tok[1] == '-'
        return int64(tok)
    end
    #TODO: return uint64(tok)
    return int64(tok)
end

function accum_digits(io::IO, pred::Function, c::Char, leading_zero::Bool)
    if !leading_zero && c == '_'
        return (utf32(""), false)
    end
    str = Char[]
    while true 
        if c == '_'
            skip(io, 1)
            c = peekchar(io)
            if !eof(c) && pred(c)
                continue
            else
                skip(io, -1)
                break 
            end
        elseif !eof(c) && pred(c)
            skip(io, 1)
            push!(str, c)
            c = peekchar(io)
            continue
        else
            break 
        end
    end
    #@assert length(str) > 0
    return (utf32(str), true)
end

is_char_hex(c::Char) = isdigit(c) || ('a' <= c <= 'f')  || ('A' <= c <= 'F')
is_char_oct(c::Char) = '0' <= c <= '7'
is_char_bin(c::Char) = c == '0' || c == '1'

fix_uint_neg(neg::Bool, n::Real) = neg ? Expr(:call, :- , n) : n

function sized_uint_literal(n::Real, s::String, b::Integer)
    i = s[1] == '-' ? 3 : 2
    l = (length(s) - i) * b
    l <= 8   && return uint8(n)
    l <= 16  && return uint16(n)
    l <= 32  && return uint32(n)
    l <= 64  && return uint64(n)
    l <= 128 && return uint128(n)
    return BigInt(n)
end

function sized_uint_oct_literal(n::Real, s::String)
    contains(s, "o0") && return sized_uint_literal(n, s, 3)
    n <= typemax(Uint8)   && return uint8(n)
    n <= typemax(Uint16)  && return uint16(n)
    n <= typemax(Uint32)  && return uint32(n)
    n <= typemax(Uint64)  && return uint64(n)
    n <= typemax(Uint128) && return uint128(n)
    return BigInt(n)
end

function compare_num_strings(s1::String, s2::String)
    s1 = lstrip(s1, '0')
    s2 = lstrip(s2, '0') 
    l1 = length(s1)
    l2 = length(s2)
    return l1 == l2 ? s1 <= s2 : l1 <= l2
end
   
function is_oct_within_uint128(s::String)
    s = s[1] === '-' ? s[2:end] : s[1:end]
    return s <= "0o3777777777777777777777777777777777777777777"
end

function is_within_int128(s::String)
    return s[1] === '-' ? s <= "-170141183460469231731687303715884105728" :
                          s <= "170141183460469231731687303715884105727"
end

function read_number(io::IO, leading_dot::Bool, neg::Bool)
    str = Char[] 
    pred::Function = isdigit 
    is_float32_literal  = false
    is_hexfloat_literal = false
    leading_zero = false

    function allow(ch::Char)
        c = peekchar(io)
        if c == ch
            push!(str, readchar(io))
            return true
        end
        return false
    end

    function disallow_dot()
        if peekchar(io) == '.'
            skip(io, 1)
            if is_dot_opchar(peekchar(io))
                skip(io, -1)
            else
                error("invalid numeric constant \"$(utf32(str))."\"")
            end
        end
    end

    function read_digits(leading_zero::Bool)
        digits, ok = accum_digits(io, pred, peekchar(io), 
                                  leading_zero)
        if !ok
            error("invalid numeric constant \"$digits\"")
        end
        if isempty(digits)
            return false
        end
        for c in digits
            push!(str, c)
        end
        return true
    end

    neg && push!(str, '-')
    if leading_dot
        push!(str, '.')
        if peekchar(io) == '0'
            push!(str, readchar(io))
            leading_zero = true
            if allow('x')
                leading_zero = false
                pred = is_char_hex
            elseif allow('o')
                leading_zero = false
                pred = is_char_oct
            elseif allow('b')
                leading_zero = false
                pred = is_char_bin
            end
        else
            allow('.')
        end
    end
    read_digits(leading_zero)
    if peekchar(io) == '.'
        skip(io, 1)
        if is_dot_opchar(peekchar(io))
            skip(io, -1)
        else
            push!(str, '.')
            read_digits(false)
            disallow_dot()
        end
    end
    c = peekchar(io)
    if c == 'e' || c == 'E' || c == 'f' || c == 'p' || c == 'P'
        skip(io, 1)
        nc = peekchar(io)
        if !eof(nc) && (isdigit(nc) || nc == '+' || nc == '-')
            is_float32_literal  = (c == 'f')
            is_hexfloat_literal = (c == 'p' || c == 'P')
            push!(str, c)
            push!(str, readchar(io))
            read_digits(false)
            disallow_dot()
        else
            skip(io, -1)
        end
    # disallow digits after binary or octal literals, e.g. 0b12
    elseif (pred == is_char_bin || pred == is_char_oct) && !eof(c) && isdigit(c)
        push!(str, c)
        error("invalid numeric constant \"$(utf32(str))\"")
    end
    s = utf32(str)
    r = pred == is_char_hex ? 16 : 
        pred == is_char_oct ? 8  :
        pred == is_char_bin ? 2  : 10
    # for an unsigned literal starting with -, 
    # remove the - and parse instead as a call to unary -
    s = (neg && !(r == 10) && !is_hexfloat_literal) ? s[2:end] : s
    n = string_to_number(s)
    # n is false for integers > typemax(Uint64)
    if is_hexfloat_literal
        return float64(n)
    elseif pred == is_char_hex
        return fix_uint_neg(neg, sized_uint_literal(n, s, 4))
    elseif pred == is_char_oct
        return fix_uint_neg(neg, sized_uint_oct_literal(n, s))
    elseif pred == is_char_bin
        return fix_uint_neg(neg, sized_uint_literal(n, s, 1))
    elseif is_float32_literal
        return float32(n)
    elseif bool(n)
        if isinteger(n) && (n > 9223372036854775807)
            return int128(n)
        else
            return n
        end
    elseif is_within_int128(s)
        return int128(s)
    else
        return BigInt(s)
    end
end

           
#============================#
# Skip whitespace / comments
#============================#

# skip multiline comments
# maintain a count of the number of enclosed #= =# pairs
# to allow nesting of multi-line comments.
# The loop is exited when this count goes below zero.

# Count is the number of read (#=) tokens.
# (#= test =#)  (#= test =#)  (#= #= test =# =#)
#  ^              ^               ^        ^
# cnt 0           cnt 1         cnt 2    cnt 1
function skip_multiline_comment(io::IO, count::Int)
    unterminated = true
    start = -1
    while !eof(io)
        c = readchar(io)
        # if "=#" token, decement the count.
        # If count is zero, break out of the loop
        if c == '='
           if start < 0 
                start = position(io)
            end
            if peekchar(io) == '#' && position(io) != start
                skip(io, 1)
                if count > 1
                    count -= 1
                    continue
                end
                unterminated = false
                break
            end
            continue
        # if "#=" token increase count
        elseif c == '#'
            count = peekchar(io) == '=' ? count + 1 : count
        end
    end
    if unterminated
        error("incomplete: unterminated multi-line comment #= ... =#")
    end
    return io
end
       
# if this is a mulitiline comment skip to the end
# otherwise skip to end of line
function skipcomment(io::IO)
    @assert readchar(io) == '#'
    if peekchar(io) == '='
        skip_multiline_comment(io, 1)
    else
        skip_to_eol(io)
    end
    return io
end

# skip all whitespace characters
# as defined by isspace()
function skipwhitespace(io::IO)
    while !eof(io)
        if readchar(io) != ' '
            skip(io, -1)
            break
        end
    end
    return io
end

# skip all whitespace before a comment,
# upon reaching the comment, if it is a
# single line comment skip to the end of the line
# otherwise skip to the end of the multiline comment block
function skip_ws_and_comments(io::IO)
    while !eof(io)
        skipwhitespace(io)
        peekchar(io) != '#' && break
        skipcomment(io)
    end
    return io
end

function is_julia_id_char(c::Char)
    return ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
            (c >= '0' && c <= '9') || (c >= 0xA1) ||
            c == '!' || c == '_')
end

function accum_julia_symbol(io::IO, c::Char)
    # preallocate to a typical size?
    str = Char[]
    nc  = c 
    while is_julia_id_char(nc)
        # make sure that != is always an operator
        c  = readchar(io)
        nc = peekchar(io)
        if c == '!' && nc == '='
            skip(io, -1)
            break
        end
        push!(str, c)
        eof(nc) && break
    end
    str = normalize_string(utf32(str), :NFC)
    return symbol(str)
end

function isuws(wc::Char)
    return (wc==9 || wc==10 || wc==11 || wc==12 || wc==13 || wc==32 ||
            wc==133 || wc==160 || wc==5760 || wc==6158 || wc==8192 ||
            wc==8193 || wc==8194 || wc==8195 || wc==8196 || wc==8197 ||
            wc==8198 || wc==8199 || wc==8200 || wc==8201 || wc==8202 ||
            wc==8232 || wc==8233 || wc==8239 || wc==8287 || wc==12288)
end

function isbom(wc::Char)
    return wc == 0xFEFF
end

function _skipws(io::IO, newlines::Bool)
    nc = peekchar(io)
    nc === EOF && return EOF
    skipped = false
    while !eof(io) && (isuws(nc) || isbom(nc)) && (newlines || nc !== '\n')
        skipped = true
        takechar(io)
        nc = peekchar(io)
    end
    return skipped
end

#= Token Stream =#
export Token, TokenStream, next_token, set_token!, last_token, 
       put_back!, peek_token, take_token, require_token

typealias Token Union(Symbol, Char, Number, Nothing)

type TokenStream
    io::IO
    #tokens::Vector{Token}
    lasttoken::Token
    putback::Token
    isspace::Bool
    ateof::Bool

    #TODO: this should ideally not be apart of TokenStream 
    range_colon_enabled::Bool
    space_sensitive::Bool
    inside_vector::Bool
    end_symbol::Bool
    whitespace_newline::Bool
end

TokenStream(io::IO) = TokenStream(io, nothing, nothing, false, eof(io),
                                  true, false, false, false, false) 
TokenStream(str::String) = TokenStream(IOBuffer(str))

function next_token(ts::TokenStream)
    ts.ateof && return EOF
    ts.isspace = _skipws(ts.io, ts.whitespace_newline)
    while !eof(ts.io)
        c = peekchar(ts.io)
        if c == ' ' || c == '\t'
            skip(ts.io, 1)
            continue
        elseif c == '#'
            skipcomment(ts.io)
            continue
        elseif eof(c) || isnewline(c)
            return readchar(ts.io)
        elseif is_special_char(c)
            return readchar(ts.io)
        elseif isdigit(c)
            return read_number(ts.io, false, false)
        elseif c == '.'
            c  = readchar(ts.io)
            nc = peekchar(ts.io)
            if eof(nc)
                return EOF
            elseif isdigit(nc)
                return read_number(ts.io, true, false)
            elseif is_opchar(nc)
                op = read_operator(ts.io, c)
                if op === :(..) && is_opchar(peekchar(ts.io))
                    error(string("invalid operator \"", op, peekchar(ts.io), "\""))
                end
                return op
            else
                return :(.)
            end
        elseif is_opchar(c)
            return read_operator(ts.io, readchar(ts.io))
        elseif is_identifier_char(c)
            return accum_julia_symbol(ts.io, c)
        else
            error(string("invalid character \"", readchar(ts.io), "\""))
        end
    end
    ts.ateof = true
    return EOF
end

set_token!(ts::TokenStream, t::Token) = begin
    ts.lasttoken = t
    return ts
end

last_token(ts::TokenStream) = ts.lasttoken

function put_back!(ts::TokenStream, t::Token)
    if ts.putback !== nothing
        error("too many pushed back tokens (internal error)")
    end
    ts.putback = t
    return ts
end

function peek_token(ts::TokenStream)
    ts.ateof && return EOF
    local t::Token
    if ts.putback !== nothing
        return ts.putback
    end
    lt = last_token(ts)
    if lt !== nothing
        return lt
    end
    set_token!(ts, next_token(ts))
    return last_token(ts)
end
        
function take_token(ts::TokenStream)
    ts.ateof && return EOF
    local t::Token 
    if ts.putback !== nothing
        t = ts.putback
        ts.putback = nothing
    else
        t = last_token(ts)
        set_token!(ts, nothing)
    end
    return t
end

function require_token(ts::TokenStream)
    local t::Token
    if ts.putback !== nothing
        t = ts.putback
    elseif ts.lasttoken !== nothing
        t = ts.lasttoken
    else
        t = next_token(ts)
    end
    eof(t) && error("incomplete: premature end of input")
    if isnewline(t)
        take_token(ts)
        return require_token(ts)
    end 
    if ts.putback === nothing
        set_token!(ts, t)
    end
    return t
end

end
