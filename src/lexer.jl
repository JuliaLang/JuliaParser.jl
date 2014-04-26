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
               [:(/), :(.//)],
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
                                   :(\),   :(&&),  :(::),   :(.),   :(...),
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
is_assignment(expr) = length(expr) == 2 && first(expr) === :(=)

is_assignment_like(expr) = length(expr) == 2 && in(first(expr), assignment_ops)

is_kwarg(l) = length(expr) == 2 && first(l) == :(kw)

is_syntactic_op(op) = in(op, syntactic_ops)

is_syntactic_unary_op(op) = in(op, syntactic_unary_ops)

is_dict_literal(l) = length(l) == 3 && first(l) === :(=>) 

const is_special_char = let 
    chars = Set{Char}("()[]{},;\"`@")
    is_special_char(c::Char)  = in(c, chars)
end

is_newline(c::Char) = return c === '\n'

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
const is_dot_opchar = let
    chars = Set{Char}(".*^/\\+-'<>!=%")
    is_dot_opchar(c::Char) = in(c, chars)
end

is_operator(o::Symbol) = in(o, operators)


#= Implement peekchar for IOBuffer and IOStream =#

# modified version from Base to give the same
# semantics as the IOStream implementation

function peekchar(from::IOBuffer)
    if !from.readable || from.ptr > from.size
        return char(-1)
    end
    ch = uint8(from.data[from.ptr])
    if ch < 0x80
        return char(ch)
    end

    # mimic utf8.next function
    trailing = Base.utf8_trailing[ch+1]
    c::Uint32 = 0
    for j = 1:trailing
        c += ch
        c <<= 6
        ch = read(s, Uint8)
    end
    c += ch
    c -= Base.utf8_offset[trailing+1]
    return char(c)
end

# this implementation is copied from Base
const peekchar = let 
    chtmp = Array(Char, 1)
    peekchar(s::IOStream) = begin
        if ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Char}), s, chtmp) < 0
            return char(-1)
        end
        return chtmp[1]
    end
end

eof(c::Char) = c === char(-1)
eof(io::IO)  = Base.eof(io)
eof(c) = false

readchar(io::IO) = read(io, Char)
takechar(io::IO) = begin
    readchar(io)
    return io
end

is_char_numeric(c::Char) = '0' <= c <= '9'

#= Lexer =#

function skip_to_eol(io::IO)
   while !eof(io)
       c = readchar(io) 
       if c == '\n'
           break
       end
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
        if is_operator(opsym) || opsym === :(//)
            skip(io, 1)
            c = peekchar(io)
            continue
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
    return uint64(tok)
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


is_char_hex(c::Char) = is_char_numeric(c) || ('a' <= c <= 'f')  || ('A' <= c <= 'F')
is_char_oct(c::Char) = '0' <= c <= '7'
is_char_bin(c::Char) = c == '0' || c == '1'

function fix_uint_neg(neg::Bool, n::Real)
    if neg
        return Expr(:call, :- , n)
    else
        return n
    end
end 

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
    if contains(s, "o0")
        return sized_uint_literal(n, s, 3)
    end
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
    if l1 == l2 
        return s1 <= s2
    else
        return l1 <= l2
    end
end
   
function is_oct_within_uint128(s::String)
    s = s[1] == '-' ? s[2:end] : s
    return s <= "0o3777777777777777777777777777777777777777777"
end

function is_within_int128(s::String)
    if s[1] == '-'
        return s <= "-170141183460469231731687303715884105728"
    else
        return s <= "170141183460469231731687303715884105727"
    end 
end

function read_number(io::IO, leading_dot::Bool, neg::Bool)
    str = Char[] 
    pred::Function = is_char_numeric
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
        if !eof(nc) && (is_char_numeric(nc) || nc == '+' || nc == '-')
            is_float32_literal = (c == 'f')
            is_hexfloat_literal = (c == 'p' || c == 'P')
            push!(str, c)
            push!(str, readchar(io))
            read_digits(false)
            disallow_dot()
        else
            skip(io, -1)
        end
    # disallow digits after binary or octal literals, e.g. 0b12
    elseif ((pred == is_char_bin || pred == is_char_oct) && 
            !eof(c) &&
            is_char_numeric(c))
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
    c = readchar(io)
    @assert c == '#'
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
        c = readchar(io)
        if c != ' '
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
        if peekchar(io) != '#'
            break
        end
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
        if c == '!'
            if nc == '='
                skip(io, -1)
                break
            end
        end
        push!(str, c)
        eof(nc) && break
    end
    str = normalize_string(utf32(str), :NFC)
    return symbol(str)
end

function next_token(io::IO, s)
    #asert s 2 ( skip-ws port whitepace-newline
    #TODO: customize whitespace management
    while !eof(io)
        c = peekchar(io)
        if c == ' ' || c == '\t'
            skip(io, 1)
            continue
        elseif c == '#'
            skipcomment(io)
            continue
        elseif eof(c) || is_newline(c)
            return readchar(io)
        elseif is_special_char(c)
            return readchar(io)
        elseif is_char_numeric(c)
            return read_number(io, false, false)
        elseif c == '.'
            c  = readchar(io)
            nc = peekchar(io)
            if eof(nc)
                return :(.)
            elseif is_char_numeric(nc)
                return read_number(io, true, false)
            elseif is_opchar(nc)
                op = read_operator(io, c)
                if op === :(..) && is_opchar(peekchar(io))
                    error(string("invalid operator \"", op, peekchar(io), "\""))
                end
                return op
            else
                return :(.)
            end
        elseif is_opchar(c)
            return read_operator(io, readchar(io))
        elseif is_identifier_char(c)
            return accum_julia_symbol(io, c)
        else
            error(string("invalid character \"", readchar(io), "\""))
        end
    end
    return nothing
end

end
