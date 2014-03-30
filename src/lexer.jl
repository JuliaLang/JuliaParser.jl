# Lexer for Julia

module Lexer

const ops_by_precedent =  {[:(=),   :(:=),   :(+=),   :(-=),  :(*=),
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
			    :(.!),  :(<:),  :(>:)],
			   [:(|>), :(<|)],
			   [:(:),  :(..)],
			   [:(+),  :(-),  :(.+),  :(.-),  :(|),   :($)],
			   [:(<<), :(>>), :(>>>), :(.<<), :(.>>), :(.>>>)],
			   [:(*),  :(/),  :(./),  :(%),   :(.%),  :(&), :(.*), :(\), :(.\)],
			   [:(/), :(./)],
			   [:(^),  :(.^)],
			   [:(::)],
			   [:(.)]}

precedent_ops(n::Integer) = ops_by_precedent[n]::Vector{Symbol}

const assignment_ops = ops_by_precedent[1]::Vector{Symbol}

const unary_ops = Set{Symbol}([:(+), :(-), :(!), :(~), :(<:), :(>:)])

const unary_and_binary = Set{Symbol}([:(+), :(-), :($), :(&), :(~)])

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

is_syntactic_op(op::Symbol) = in(op, syntactic_ops)

is_syntactic_unary_op(op::Symbol) = in(op, syntactic_unary_ops)

is_dict_literal(l) = length(l) == 3 && first(l) === :(=>) 

const is_special_char = 
    let chars = Set{Char}("()[]{},;\"`@")
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
const is_dot_opchar =
    let chars = Set{Char}(".*^/\\+-'<>!=%")
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
const _chtmp = Array(Char, 1)
function peekchar(s::IOStream)
    if ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Char}), s, _chtmp) < 0
        return char(-1)
    end
    return _chtmp[1]
end

eof(c::Char) = c === char(-1)
eof(io::IO ) = Base.eof(io)

readchar(io::IO) = read(io, Char)

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
    if eof(pc) || !(is_opchar(pc))
        return symbol(c)
    end
    #TODO: faster to preallocate this to the largest known operator
    str = Char[c]
    c   = pc
    while !(eof(c)) && is_opchar(c)
        push!(str, c)
	newop = utf32(str)
	opsym = symbol(newop)
	if is_operator(opsym)
	    str = newop
	    c   = readchar(io)
        else 
	    return opsym 
	end
    end
    return symbol(utf32(str))
end

#=
function accum_digits(io::IO, pred::Function, c::Char, lz)
    if !(bool(lz)) && c == '_'
        return ('_', false)
    end
    str = Char[]
    if c == '_'
        readchar(io)
        c = peekchar(io)
	if !eof(c) && pred(c)
	    @goto :loop
	else
	    # ungetc(io, '_')
	    seek(io, -1)
	    return (utf32(str), false)
	end
    else
        if !eof(c) & pred(c)
            readchar(io)
	    push!(str, c)
	    @goto :loop
	else
	    return (utf32(str), true)
	end
    end
end
=#

is_char_hex(c::Char) = is_char_numeric(c) || ('a' <= c <= 'f')  || ('A' <= c <= 'F')
is_char_oct(c::Char) = '0' <= c <= '7'
is_char_bin(c::Char) = c == '0' || c == '1'

function skip_multiline_comment(io::IO, count::Int)
    while true
        c = readchar(io)
	@show c
        if eof(io)
            error("incomplete: unterminated multi-line comment #= ... =#")
        end
        if c == '='
            c = peekchar(io)
	    if c == '#'
	        seek(io, 1)
	        if count > 1
		    skip_multiline_comment(io, count - 1)
                end
	        return io 
            end
	    skip_multiline_comment(io, count)
        elseif c == '#'
            count = peekchar(io) == '=' ? count + 1 : count
	    skip_multiline_comment(io, count)
        end
        skip_multiline_comment(io, count) 
    end
    return io
end
       
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

function skipwhitespace(io::IO)
    while !eof(io)
        c = readchar(io)
	if !isspace(c)
            break
        end 
    end
    return io
end
        
function skip_ws_comments(io::IO)
    while !eof(io)
        skipwhitespace(io)
	if peekchar(io) != '#'
	    break
	end
	skipcomment(io)
    end
    return io
end

end
