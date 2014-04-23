# Julia Source Parser
module Parser

import ..Lexer

typealias Token Union(Symbol, Char, Number, Nothing)

type TokenStream
    io::IO
    tokens::Vector{Token}
    lasttoken::Token
    putback::Token
    isspace::Bool
end

TokenStream(io::IO) = TokenStream(io, {}, nothing, false, false) 

Base.isspace(ts::TokenStream) = ts.isspace
Base.eof(ts::TokenStream) = eof(ts.io)

function set_token!(ts::TokenStream, t::Token)
    push!(ts.tokens, t)
    return ts
end

function last_token(ts::TokenStream)
    if isempty(ts.tokens)
        return nothing
    end
    return ts.tokens[end]
end 

function put_back!(ts::TokenStream, t::Token)
    if ts.putback != nothing
        error("too many pushed back tokens (internal error)")
    end
    ts.putback = t
    return ts
end

function peek_token(ts::TokenStream)
    local t::Token
    if ts.putback != nothing
        return ts.putback
    end
    lt = last_token(ts)
    if lt != nothing
        return lt
    end
    set_token!(ts, next_token(ts))
    return last_token(ts)
end
        
isnewline(t::Token) = t == '\n'

function take_token(ts::TokenStream)
    local t::Token 
    if ts.putback != nothing
        t = putback
        ts.putback = nothing
    else
        t = last_token(ts)
        set_token!(ts, nothing)
    end
    return t
end

function require_token(ts::TokenStream)
    local t::Token
    if ts.putback != nothing
        t = ts.putback
    elseif last_token(ts) != nothing
        t = ts.lasttoken
    else
        t = next_token(ts)
    end
    eof(t) && error("incomplete: premature end of input")
    if isnewline(t)
        take_token(ts)
        return require_token(ts)
    end 
    if ts.putback == nothing
        set_token!(ts, t)
    end
    return t
end

filename(ts::TokenStream) = "test.jl"
curline(ts::TokenStream)  = 0

function line_number_node(ts)
    line = curline(ts)
    return Expr(:line, {curline})
end

function line_number_filename_node(ts::TokenStream)
    line  = curline(ts)
    fname = filename(ts) 
    return Expr(:line, {line, fname}) 
end

const sym_do      = symbol("do")
const sym_else    = symbol("else")
const sym_elseif  = symbol("elseif")
const sym_catch   = symbol("catch")
const sym_finally = symbol("finally")
const sym_squote  = symbol("'")

const is_invalid_initial_token = let
    invalid = Set({')', ']', '}', sym_else, sym_elseif, sym_catch, sym_finally}) 
    is_invalid_initial_token(t::Token) = t in invalid
end

const is_closing_token = let
    closing = Set({',', ')', ']', '}', ';', sym_else, sym_elseif, sym_catch, sym_finally})
    is_closing_token(t::Token) = t in closing
end

function parse_nary(io::IO)
    invalid_initial_token(require_token(io))
end

function parse_chain(ts::TokenStream, down, op; 
                     space_sensitive::Bool=true)
    chain = Token[down(ts)]
    while !eof(ts)
        t = peek_token(ts)
        if t != op
            return chain
        end
        take_token(ts)
        #TODO: this could be refactored
        if (space_sensitive && isspace(ts) && 
            (t in Lexer.unary_and_binary_ops) &&
            Lexer.peekchar(ts.io) != '\\')
            # here we have "x -y"
            put_back!(ts, t) 
            return chain
        end
        push!(chain, down(ts))
    end
end

# parse left to right chains of certain binary operator
# ex. a + b + c => (call + a b c)
function parse_with_chains(ts::TokenStream, down, ops, chain_op; 
                           space_sensitive::Bool=true) 
    ex = down(ts)
    while !eof(ts)
        t = peek_token(ts)
        if !(t in ops)
            return ex
        end
        take_token(ts)
        if (space_sensitive &&
            isspace(ts) &&
            (t in Lexer.unary_and_binary_ops) &&
            Lexer.peekchar(ts.io) != '\\')
            # here we have "x -y"
            put_back!(ts, t)
            return chain
        elseif t == chain_op
            args = append!({t, ex}, parse_chain(ts, down, t))
            ex = Expr(:call, args)
        else
            args = {t, ex, down(ts)}
            ex = Expr(:call, args)
        end
    end
    error("end of file in parse_with_chains")
end

function parse_LtoR(ts::TokenStream, down, ops;
                    space_sensitive=false)
    ex = down(ts)
    t  = peek_token(ts)
    while !eof(ts)
        if !(t in ops)
            return ex
        end
        _ = take_token(ts)
        if Lexer.is_syntactic_op(t) || t === :(in)
            ex = {t, ex, down(ts)}
            t  = peek_token(ts)
        else
            ex = {:call, t, ex, down(s)}
            t  = peek_token(ts)
        end
    end
    error("end of file in parse_LtoR")
end

function parse_RtoL(ts::TokenStream, down, ops;
                    space_sensitive::Bool=false)
    while !eof(ts)
        ex = down(ts)
        t  = peek_token(ts)
        if !(t in ops)
            return ex
        end
        _ = take_token(ts)
        if (space_sensitive && 
            isspace(ts) &&
            (t in Lexer.unary_and_binary_ops) &&
            Lexer.peekchar(ts.io) != '\\')
            put_back!(ts, t)
            return ex
        elseif Lexer.is_syntactic_op(t)
            return {t, ex, parse_RtoL(ts, down, ops)}
        elseif t === :(~)
            args = parse_chain(ts, down, :(~))
            if peek_token(ts) in ops
                #XXX: this is wrong
                return {:macrocall, symbol("@~"), ex, args[1:end-1]}
            else
                return {:macrocall, symbol("@~"), ex, args}
            end
        else
            return {:call, t, ex, parse_RtoL(ts, down, ops)}
        end
    end
end

macro without_range_colon(body)
    quote
        let
            range_colon_enabled = false
            ret = esc($body)
        end
        ret
    end
end

function parse_cond(ts::TokenStream)
    ex = parse_or(ts)
    if peek_token(ts) == '?'
        take_token(ts)
        then = @without_range_colon parse_eq(ts)
        take_token(ts) == ':' || error("colon expected in \"?\" expression")
        #return Expr(:if, {ex, then, parse_cond(ts)})
    end
    args = ex.args
    while !eof(ts)
        next = peek_token(ts)
        if eof(next) || is_closing_token(next) || is_newline(next)
            return Expr(:call, {:(top), :string, args})
        end
        push!(args, parse_or(ts))
    end
    error("end of file in parse_cond")
end


const expr_ops = Lexer.precedent_ops(9)
const term_ops = Lexer.precedent_ops(11)

parse_expr(ts::TokenStream) = parse_with_chains(ts, parse_shift, expr_ops, :(+))
parse_term(ts::TokenStream) = parse_with_chains(ts, parse_rational, term_ops, :(*))

parse_shift(ts::TokenStream)    = parse_LtoR(ts, parse_term, Lexer.precedent_ops(10))
parse_rational(ts::TokenStream) = parse_LtoR(ts, parse_unary, Lexer.precedent_ops(12))
parse_pipes(ts::TokenStream)    = parse_LtoR(ts, parse_range, Lexer.precedent_ops(7))
parse_in(ts::TokenStream)       = parse_LtoR(ts, parse_pipes, :(in))

# parse-equal is used where commas are special for example in an argument list 
parse_eqs(ts::TokenStream) =  parse_RtoL(ts, parse_cond, Lexer.precedent_ops(1))

# parse-comma is neeed for commas outside parens, for example a = b, c
parse_comma(ts::TokenStream) = parse_Nary(ts, parse_cond, ',', :tuple, {}, false)

parse_or(ts::TokenStream)    = parse_LtoR(ts, parse_and, Lexer.precedent_ops(3))
parse_and(ts::TokenStream)   = parse_LtoR(ts, parse_arrow, Lexer.precedent_ops(4))
parse_arrow(ts::TokenStream) = parse_RtoL(ts, parse_ineq, Lexer.precedent_ops(5))
parse_ineq(ts::TokenStream)  = parse_comparison(ts, Lexer.precedent_ops(6))


function parse_comparison(ts::TokenStream, ops)
    ex = parse_in(ts)
    first = true
    while !eof(ts)
        t = peek_token(ts)
        if !(t in ops)
            return ex
        end
        _ = take_token(ts)
        if first
            args = append!({ex, t}, parse_range(ts))
            ex = Expr(:comparison, args)
            first = false
        else
            #TODO: fix
            args = append!({t}, parse_range(ts)) 
            append!(ex.args, args) 
        end
    end
    error("end of file in parse_comparison")
end

is_large_number(n::Number) = false

function maybe_negate(op, num)
    if op !== :(-)
      return num
    end
    if is_large_number(num)
        if num[3] == "-170141183460469231731687303715884105728"
            return BigInt(170141183460469231731687303715884105728)
        else
            # return tail of string
            return Expr(num[1], {num[2], num[3][2:end]})
        end
    end
    if num == -9223372036854775808
        return int128(9223372036854775808)
    end
    return Expr(:-, num)
end


const is_juxtaposed = let
    invalid_chars = Set{Char}({'(', '[', '{'})

    is_juxtaposed(ex::Expr, t::Token) = begin
        return !(Lexer.is_operator(t)) &&
               !(Lexer.is_operator(ex)) &&
               !(t in Lexer.reserved_words) &&
               !(is_closing_token(t)) &&
               !(is_newline(t)) &&
               !(ex.head === :(...)) &&
               (isnumber(ex) || islargenumber(ex) || !(t in invalid_chars))
    end
end


function parse_juxtaposed(ts::TokenStream, ex::Expr) 
    next = peek_token(ts)
    # numeric literal juxtaposition is a unary operator
    if is_juxtaposed(ex, next) && !isspace(ts)
        error("juxtaposition with literal \"0\"")
    end
    args = append!({:(*)}, parse_unary(ts))
    return Expr(:call, args)
end


function parse_range(ts::TokenStream)
    ex = parse_expr(ts)
    isfirst = first(ex) == true
    while !eof(ts)
        t   = peek_token(ts)
        spc = isspace(ts)
        
        if isfirst && t == :(..)
           _ = take_token(ts)
           return Expr(:call, {t, ex, parse_expr(ts)})
        end

        if range_colon_enabled && t == :(:)
            _ = take_token(ts)
            if (space_sensitive && spc && 
                (peek_token(ts) || true) &&
                ts.isspace == false)
                # "a :b" in space sensitive mode
                put_back!(ts, :(:))
                return ex
            end
            if is_closing_token(peek_token(ts))
                error("deprecated syntax x[i:]")
            elseif is_newline(peek_token(ts))
                error("line break in \":\" expression")
            end
            arg = parse_expr(ts)
            if ts.isspace == false && (arg === :(<) || arg === :(>))
                error("\":$argument\" found instead of \"$argument:\"")
            end
            if isfirst
                ex = {t, ex, arg}
                isfirst = false
            else
                push!(arg, ex)
                isfirst = true
            end
            continue
        elseif t == :(...)
            _ = take_token(ts)
            return Expr(:(...), ex)
        else
            return ex
        end
    end
end 

function parse_call(ts::TokenStream)
end

function parse_decl(ts::TokenStream)
    local ex::Expr
    if peek_token(ts) == :(::)
        take_token(ts)
        ex = Expr(:(::), parse_call(ts))
    else
        ex = parse_call(ts)
    end
    while !eof(ts)
        t = peek_token(ts)
        if t == :(::)
            take_token(ts)
            call = parse_call(ts)
            ex = Expr(t, {ex, parse_call(ts)})
        elseif t == :(->)
            take_token(ts)
            # -> is unusual it binds tightly on the left and loosely on the right
            lno = line_number_filename_node(ts)
            return Expr(:(->), {ex, Expr(:block, {lno, parse_eqs(ts)})})
        else
            return ex
        end
    end
end

function parse_factorh(ts::TokenStream, down::Function, ops)
    ex = down(ts)
    t  = peek_token(ts)
    if !(t in ops)
        return ex
    else
        t  = take_token(ts)
        pf = parse_factorh(ts, parse_unary, ops)  
        return Expr(:call, {t, ex, pf})
    end
end

function parse_factor(ts::TokenStream)
    return parse_factorh(ts, parse_decl, Lexer.precedent_ops[12])
end

function parse_unary(ts::TokenStream)
    t = require_token(ts)
    is_closing_token(t) && error("unexpected $t")
    if !(t in Lexer.unary_ops)
        return parse_juxtaposed(parse_factor(ts), ts)
    end
    op = take_token(ts)
    nc = Lexer.peekchar(ts.io)
    if (op == :(-) || op == :(+)) && (Lexer.isnumber(nc) || nc == '.')
        neg = op == :(-)
        leadingdot = nc == '.'
        leadingdot && Lexer.readchar(ts.io)
        n = Lexer.read_number(ts.io, leadingdot, neg) 
        num = parse_juxtaposed(ts, n)
        if peek_token(ts) in (:(^), :(.^))
            # -2^x parsed as (- (^ 2 x))
            put_back!(ts, maybe_negate(op, num))
            return Expr(:call, {op, parse_factor(ts)})
        else
            return num
        end
    else
        nt = peek_token(ts)
        if is_closing_token(nt) || is_newline(nt)
            # return operator by itself, as in (+)
            return op
        elseif nt == '{'
            # this case is +{T}(x::T)
            put_back!(ts, op)
            return pase_factor(ts)
        else
            arg = parse_unary(ts)
            if isa(arg, Expr) && arg.head === :tuple
                return Expr(:call, {op, arg[1]})
            else
                return Expr(:call, {op, arg})
            end
        end
    end
end

function subtype_syntax(ex::Expr)
    if ex.head == :comparison && length(ex.args) == 3 && ex.args[2] == :(<:)
        return Expr(:(<:), {ex.args[1], ex.args[3]})
    else 
        return ex
    end
end

function parse_unary_prefix(ts::TokenStream)
    op = peek_token(ts)
    if !(Lexer.is_syntactic_unary_op(op))
        return parse_atom(ts)
    end
    _  = take_token(ts)
    if is_closing_token(peek_token(ts))
        return op
    elseif op == :(&)
        return Expr(op, {parse_call(ts)})
    else
        return Expr(op, {parse_atom(ts)})
    end
end

# parse function all, indexing, dot, and transpose expressions
# also handles looking for reserved words 

function parse_call(ts::TokenStream)
    ex = parse_unary_prefix(ts)
    if ex in Lexer.reserved_words
        return parse_resword(ts, ex)
    else
        return pase_call_chain(ts, ex, false)
    end
end


function separate(func::Function, collection)
    tcoll, fcoll = {}, {}
    for c in collection
        func(c) ? push!(tcoll, c) : push!(fcoll, c)
    end
    return (tcoll, fcoll)
end

macro with_end_symbol(body)
    quote
        esc(let
            space_sensitive    = false
            inside_vector      = true
            whitespace_newline = false
            $body
        end)
    end
end

function parse_call_chain(ts::TokenStream, ex, one_call::Bool)
    temp = ['(', '[', '{', ''', '"']
    while !eof(ts)
        t = peek_token(ts)
        if (space_sensitive && ts.isspace && (t in temp)) ||
           ((isnumber(ex) || islargenumber(ex)) && t == '('))
           return ex
        end
        if t == '('
            take_token(ts)
            arglist = parse_arglist(ts, ')')
            params, args = separate((ex) -> isa(ex, Expr) && 
                                            length(ex.args) == 1 &&
                                            ex.head == :parameters,
                                    arglist)
            if peek_token(ts) == sym_do
                take_token(ts)
                ex = Expr(:call, {ex, params..., parse_do(ts), args...})
            else
                ex = Expr(:call, {ex, arglist...})
            end
            one_call && return ex
            continue
        
        elseif t == '['
            take_token(ts)
            # ref is syntax so can distinguish a[i] = x from ref(a, i) = x
            al = @with_end_symbol parse_cat(ts, ']')
            if al == nothing
                if is_dict_literal(ex)
                    ex = Expr(:typed_dict, {ex})
                else
                    ex = Expr(:ref, {ex})
                end
                continue
            end
            if al.head == :dict
                ex = Expr(:typed_dict, {ex, al.args...})
            elseif al.head == :hcat
                ex = Expr(:typed_hcat, {ex, al.args...})
            elseif al.head == :vcat
                fn = (ex) -> isa(ex, Expr) && length(ex.args) == 1 && ex.head == :row
                if any(fn, al.args)
                    ex = Expr(:typed_vcat, {ex, al.args...})
                else
                    ex = Expr(:ref, {ex, al.args...})
                end
            elseif al.head == :comprehension
                ex = Expr(:typed_comprehension, {ex, al.args...})
            elseif al.head == :dict_comprehension
                ex = Expr(:typed_dict_comprehension, {ex, al.args...})
            else
                error("unknown parse-cat result (internal error)")
            end
            continue

        elseif t == :(.)
            take_token(ts)
            nt = peek_token(ts)
            if nt == '('
                ex = Expr(:(.), {ex, parse_atom(ts)})
            elseif nt == :($)
                dollar_ex = parse_unary(ts)
                call_ex   = Expr(:call, {Expr(:top, :Expr), 
                                         Expr(:quote, :quote), 
                                         dollar_ex.args[1]})
                ex = Expr(:macrocall, {ex, Expr(:($), call_ex)})
            else
                name = parse_atom(ts)
                if isa(name, Expr) && name.head == :macrocall
                    ex = Expr(:macrocall, {:(.), 
                                             ex, 
                                             Expr(:quote, name.args[1]),
                                             name.args[2:end]})
                else
                    ex = Expr(:(.), {ex, Expr(:quote, name)})
                end
            end
            continue

        elseif t == :(.') || t == sym_squote
            take_token(ts)
            ex = Expr(t, ex)
            continue

        elseif t == '{'
            take_token(ts)
            args = map(subtype_syntax, parse_arglist(ts, '}'))
            ex = Expr(:curly, {ex, args...})
            continue

        elseif t == '"'
            if isa(ex, Symbol) && !is_operator(ex) && !ts.isspace
                # custom prefexed string literals x"s" => @x_str "s"
                take_token(ts)
                str = parse_string_literal(ts, true)
                nt  = peek_token(ts)
                suffix  = is_triplequote_str_literal(str) ? :(_mstr) : :(_str)
                macname = symbol(string('@', ex, suffix))
                macstr  = str[2:end]
                
                if isa(nt, Symbol) && !is_operator(nt) && !ts.isspace
                    # string literal suffix "s"x
                    ex = Expr(:macrocall, {macname, macstr, string(take_token(ts))})
                else
                    ex = Expr(:macrocall, {macrocall, macstr})
                end
                continue
            else
                return ex
            end
        
        else
            return ex
        end
    end
end 




function parse_resword(ts::TokenStream)
end

function parse(ts::TokenStream)
    Lexer.skip_ws_and_comments(ts.io)
    while !eof(ts)
        t = Lexer.next_token(ts.io, nothing)
        if isnewline(t)
            continue
        end
        break
    end
    return ts
end

end
