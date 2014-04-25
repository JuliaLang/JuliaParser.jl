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
    return Expr(:line, curline)
end

function line_number_filename_node(ts::TokenStream)
    line  = curline(ts)
    fname = filename(ts) 
    return Expr(:line, line, fname) 
end

const sym_do      = symbol("do")
const sym_quote   = symbol("quote")
const sym_begin   = symbol("begin")
const sym_else    = symbol("else")
const sym_elseif  = symbol("elseif")
const sym_end     = symbol("end")
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
            return Expr(:call, :(top), :string, args...)
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
            ex = Expr(:comparison, args...)
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
            return Expr(num[1], num[2], num[3][2:end]...)
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
    return Expr(:call, args...)
end


function parse_range(ts::TokenStream)
    ex = parse_expr(ts)
    isfirst = first(ex) == true
    while !eof(ts)
        t   = peek_token(ts)
        spc = isspace(ts)
        
        if isfirst && t == :(..)
           _ = take_token(ts)
           return Expr(:call, t, ex, parse_expr(ts))
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
            ex = Expr(t, ex, parse_call(ts))
        elseif t == :(->)
            take_token(ts)
            # -> is unusual it binds tightly on the left and loosely on the right
            lno = line_number_filename_node(ts)
            return Expr(:(->), ex, Expr(:block, lno, parse_eqs(ts)))
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
        return Expr(:call, t, ex, pf)
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
            return Expr(:call, op, parse_factor(ts))
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
                return Expr(:call, op, arg[1])
            else
                return Expr(:call, op, arg)
            end
        end
    end
end

function subtype_syntax(ex::Expr)
    if ex.head == :comparison && length(ex.args) == 3 && ex.args[2] == :(<:)
        return Expr(:(<:), ex.args[1], ex.args[3])
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
        return Expr(op, parse_call(ts))
    else
        return Expr(op, parse_atom(ts))
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
    temp = ['(', '[', '{', '\'', '"']

    while !eof(ts)
        t = peek_token(ts)
        if (space_sensitive && ts.isspace && (t in temp)) ||
           ((isnumber(ex) || islargenumber(ex)) && t == '(')
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
                ex = Expr(:call, ex, params..., parse_do(ts), args...)
            else
                ex = Expr(:call, ex, arglist...)
            end
            one_call && return ex
            continue
        
        elseif t == '['
            take_token(ts)
            # ref is syntax so can distinguish a[i] = x from ref(a, i) = x
            al = @with_end_symbol parse_cat(ts, ']')
            if al == nothing
                if is_dict_literal(ex)
                    ex = Expr(:typed_dict, ex)
                else
                    ex = Expr(:ref, ex)
                end
                continue
            end
            if al.head == :dict
                ex = Expr(:typed_dict, ex, al.args...)
            elseif al.head == :hcat
                ex = Expr(:typed_hcat, ex, al.args...)
            elseif al.head == :vcat
                fn = (ex) -> isa(ex, Expr) && length(ex.args) == 1 && ex.head == :row
                if any(fn, al.args)
                    ex = Expr(:typed_vcat, ex, al.args...)
                else
                    ex = Expr(:ref, ex, al.args...)
                end
            elseif al.head == :comprehension
                ex = Expr(:typed_comprehension, ex, al.args...)
            elseif al.head == :dict_comprehension
                ex = Expr(:typed_dict_comprehension, ex, al.args...)
            else
                error("unknown parse-cat result (internal error)")
            end
            continue

        elseif t == :(.)
            take_token(ts)
            nt = peek_token(ts)
            if nt == '('
                ex = Expr(:(.), ex, parse_atom(ts))
            elseif nt == :($)
                dollar_ex = parse_unary(ts)
                call_ex   = Expr(:call, Expr(:top, :Expr), 
                                        Expr(:quote, :quote), 
                                        dollar_ex.args[1])
                ex = Expr(:macrocall, ex, Expr(:($), call_ex))
            else
                name = parse_atom(ts)
                if isa(name, Expr) && name.head == :macrocall
                    ex = Expr(:macrocall, :(.), 
                                          ex, 
                                          Expr(:quote, name.args[1]),
                                          name.args[2:end])
                else
                    ex = Expr(:(.), ex, Expr(:quote, name))
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
            ex = Expr(:curly, ex, args...)
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
                    ex = Expr(:macrocall, macname, macstr, string(take_token(ts)))
                else
                    ex = Expr(:macrocall, macrocall, macstr)
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

const expect_end_current_line = 0

function _expect_end(ts::TokenStream, word)
    t = peek_token(ts)
    if t == sym_end
        take_token(ts)
    elseif eof(t)
        err_msg = "incomplete: \"$word\" at {current_filename} : {expected} requires end"
        error(err_msg)
    else
        err_msg = "incomplete: \"$word\" at {current filename} : {expected} \"end\", got \"$t\""
        error(err_msg)
    end
end

macro with_normal_ops(body)
    body  
end

macro without_newspace_newline(body)
    body
end

short_form_function_loc(ex, lno) = nothing

parse_subtype_spec(ts::TokenStream) = subtype_syntax(parse_ineq(ts))

# parse expressions or blocks introduced by syntatic reserved words
function parse_resword(ts::TokenStream, word)
    #XXX: with bindings
    expect_end_current_line = curline(ts)
    
    expect_end(ts::TokenStream) = _expect_end(ts, word) 

   # @with_normal_ops begin
   #     @without_newspace_newline begin

            if word == :quote || word == :begin
                Lexer.skip_ws_and_comments(ts.io)
                loc = line_number_filename_node(ts)
                blk = parse_block(ts)
                expect_end(ts)
                if length(blk.args) > 1
                    blk = Expr(:block, loc, blk.args...)
                end
                if word == :quote
                    return Expr(sym_quote, blk)
                else
                    return blk
                end

            elseif word == :while
                ex = Expr(:while, parse_cond(ts), parse_block(ts))
                expect_end(ts)
                return ex

            elseif word == :for
                ranges = parse_comma_sep_iters(ts)
                body   = parse_block(ts)
                expect_end(ts)
                r = nothing #TODO: 
                if r != nothing
                    return body
                else
                    return Expr(:for, r.head, nest(r.args))
                end

            elseif word == :if
                test = parse_cond(ts)
                t = require_token(ts)
                local then::Expr
                if t == sym_else || t == sym_elseif
                    then = Expr(:block)
                else
                    then = parse_block(ts) 
                end
                nxt = require_token(ts)
                if nxt == sym_end
                    return Expr(:if, test, then)
                elseif nxt == sym_elseif
                    if isnewline(peek_token(ts))
                        error("missing condition in elseif at {filename} : {line}")
                    end
                    blk = Expr(:block, line_number_node(ts), parse_resword(ts, :if))
                    return Expr(:if, test, then, blk)
                elseif nxt == sym_else
                    if peek_token(ts) == :if
                        error("use elseif instead of else if")
                    end
                    ex = Expr(:if, test, then, parse_block(ts))
                    expect_end(ts)
                    return ex
                else
                    error("unexpected $nxt")
                end

            elseif word == :let
                nt = peek_token(ts)
                local binds
                if Lexer.isnewline(nt) || nt == ';'
                    binds = {}
                else
                    binds = parse_comma_sep_assigns(ts)
                end
                nt = peek_token(ts)
                if !(eof(nt) || (nt in ('\n', ';', sym_end)))
                    error("let variables should end in \";\" or newline")
                end
                ex = parse_block(ts)
                expect_end(ts)
                return Expr(:let, ex, binds...)

            elseif word == :global || word == :local
                lno  = curline(ts)
                isconst = false
                if peek_token(ts) == :const
                    take_token(ts)
                    isconst = true
                end
                head = word
                args = map((ex) -> short_form_function_loc(ex, lno), 
                           parse_comma_sep_assigns(ts))
                if isconst
                    return Expr(:const, Expr(head, args))
                else
                    return Expr(head, args)
                end

            elseif word == :function || word == :macro
                paren = require_token(ts) == '('
                sig   = parse_call(ts)
                local def::Expr
                if isa(sig, Symbol) ||
                   (isa(sig, Expr) && sig.head == :(::) && isa(first(sig.args), Symbol))
                   if paren
                       # in function(x) the (x) is a tuple
                       def = Expr(:tuple, sig)
                    else
                       # function foo => syntax error
                       error("expected \"(\" in $word definition")
                    end
                else
                    if (isa(sig, Expr) && (sig.head == :call || sig.head == :tuple))
                        def = sig
                    else
                        error("expected \"(\" in $word definition")
                    end
                end
                if peek_token(ts) != sym_end
                    Lexer.skip_ws_and_comments(ts.io)
                end
                loc = line_number_filename_node(ts)
                body = parse_block(ts)
                expect_end(ts)
                add_filename_to_block!(body, loc)
                return Expr(word, def, body)

            elseif word == :abstract
                return Expr(:abstract, parse_subtype_spec(ts))

            elseif word == :type || word == :immutable
                istype = word == :type
                isimmutable = word == :immutable
                if isimmutable && peek_token == :type
                    # allow "immutable type"
                    take_token(ts)
                end
                sig = parse_subtype_spec(ts)
                ex  = Expr(:type, istype, sig, parse_block(ts))
                expect_end(ts)
                return ex

            elseif word == :bitstype
                stmnt = let space_sensitive = true
                    parse_cond(ts)
                end
                return Expr(:bitstype, stmnt, parse_subtype_spec(ts))

            elseif word == :typealias
                lhs = pase_call(ts)
                if isa(lhs, Expr) && lhs.head == :call 
                    # typealias X (...) is a tuple type alias, not call
                    return Expr(:typealias, lhs.args[1], 
                                            Expr(:tuple, lhs.args[2:end]))
                else
                    return Expr(:typealias, lhs, parse_arrow(ts))
                end

            elseif word == :try
                local try_block::Expr
                if require_token(ts) in (sym_catch, sym_finally)
                    try_block = Expr(:block)
                else
                    try_block = parse_block(ts)
                end
                nxt = require_token(ts)
                catchb = nothing
                catchv = false
                finalb = nothing
                while true
                    take_token(ts)
                    if nxt == sym_end
                        if finalb != nothing
                            return Expr(:try, try_block, catchv, catchb, finalb)
                        else
                            return Expr(:try, try_block, catchv, catchb)
                        end
                    elseif nxt == sym_catch && catchb == nothing
                        nl = isnewline(peek_token(ts))
                        if require_token(ts) in (sym_end, sym_finally)
                            nxt    = require_token(ts)
                            catchb = Expr(:block)
                            catchv = false 
                            continue
                        else
                            var   = parse_eqs(ts)
                            isvar = nl == false && isa(var, Symbol)
                            local catch_block::Expr
                            if require_token(ts) == sym_finally
                                catch_block = Expr(:block)
                            else
                                catch_block = parse_block(ts)
                            end
                            nxt = require_token(ts)
                            catchb = isvar ? catch_block : Expr(:block, var, catch_block.args...)
                            catchv = isvar && var != nothing
                            continue
                        end
                    elseif nxt == sym_finally && finalb != nothing
                        local fb::Expr
                        if require_token(ts) == sym_catch
                            finalb = Expr(:block)
                        else
                            finalb = parse_block(ts)
                        end
                        nxt = require_token(ts)
                        continue 
                    else
                        error("unexpected \"$nxt\"")
                    end
                end

            elseif word == :return
                t = peek_token(ts)
                if isnewline(t) || is_closing_token(t)
                    return Expr(:return, nothing)
                else
                    return Expr(:return, parse_eq(ts))
                end

            elseif word == :break || word == :continue
                return Expr(word)

            elseif word == :const
                assgn = parse_eq(ts)
                if !(isa(assgn, Expr) && 
                     (assgn.head == :(=) ||
                      assgn.head == :global ||
                      assgn.head == :local))
                    error("expected assignment after \"const\"")
                end
                return Expr(:const, assgn)

            elseif word == :module || word == :baremodule
                isbare = word == :baremodule
                name = parse_atom(ts)
                body = parse_block(ts)
                expect_end(ts)
                if !isbare
                    # add definitions for module_local eval
                    block = Expr(:block)
                    x = name == :x ? :y : :x
                    push!(block.args, 
                        Expr(:(=), Expr(:call, :eval, x),
                                   Expr(:call, Expr(:(.), Expr(:top, :Core), :eval), name, x)))
                    push!(block.args,
                        Expr(:(=), Expr(:call, :eval, :m, :x),
                                   Expr(:call, Expr(:(.), Expr(:top, :Core), :eval), :m, :x)))
                    append!(block.ags, body.args)
                    body = block
                end
                return Expr(:module, !isbare, name, body) 

            elseif word == :export
                es = map(macrocall_to_atsym, parse_comma_sep(ts, parse_atom))
                if !all(x -> isa(x, Symbol), es)
                    error("invalid \"export\" statement")
                end
                ex = Expr(:export)
                append!(ex.args, es)
                return ex

            elseif word == :import || word == :using || word == :importall
                imports = parse_imports(ts)
                if isempty(imports.args)
                    return Expr(:imports)
                else
                    return Expr(:toplevel, imports)
                end

            elseif word == :ccall
                if peek_token(ts) != '('
                    error("invalid \"ccall\" syntax")
                end
                take_token(ts)
                al = parse_arglist(ts, ')')
                if length(al) > 1 && al[2] in (:cdecl, :stdcall, :fastcall, :thiscall)
                    # place calling convenction at end of arglist
                    return Expr(:ccall, al[1], al[3:end]..., al[2])
                else
                    return Expr(:ccall, al)
                end

            elseif word == :do
                error("invalid \"do\" syntax")

            else
                error("unhandled reserved word $word")
            end 
        #end
    #end
end

function add_filename_to_block(body::Expr, loc)
    if !isempty(body.args) && 
        isa(body.args[1], Expr) && 
        body.args[1].head == :line
        unshift!(body.args, loc)
    end
    return body
end

function parse_do(ts::TokenStream)
    # TODO: bindings
    local doargs
    if isnewline(peek_token(ts))
        doargs = {}
    else
        doargs = parse_comma_sep(ts, parse_range)
    end
    loc = line_number_filename_node(ts)
    blk = add_filename_to_block!(parse_block(ts), loc)
    expect_end(ts, :do)
    return Expr(:(->), Expr(:tuple, doargs...), blk)
end

function macrocall_to_atsym(ex)
    if isa(ex, Expr) && ex.head == :macrocall
        return first(ex.args)
    else
        return ex
    end
end

function parse_imports(ts::TokenStream, word)
    frst = parse_import(ts, word)
    nt   = peek_token(ts)
    from = nt == :(:) && ts.isspace
    done = false
    if from || nt == ','
        take_token(ts)
        done = false
    elseif nt in ('\n', ';')
        done = true
    elseif eof(nt)
        done = true
    else
        done = false
    end
    rest = done ? {} : parse_comma_sep(ts, (ts) -> parse_import(ts, word))
    if from
        #TODO: this is a mess
        fn = x -> begin
            ret = {x[1]}
            append!(ret, frst[2:end])
            append!(ret, x[2:end])
        end
        return map(fn, rest)
    else
        return append!(first, rest)
    end
end

const sym_1dot  = symbol(".")
const sym_2dots = symbol("..")
const sym_3dots = symbol("...")
const sym_4dots = symbol("....")

function parse_import_dots(ts::TokenStream)
    l = {}
    t = peek_token(ts)
    while !eof(ts)
        if t == sym_1dot
            take_token(ts)
            l = append!({:(.)}, l)
            t = peek_token(ts)
            continue
        elseif t == sym_2dots
            take_token(ts)
            l = append!({:(.), :(.)}, l)
            t = peek_token(ts)
            continue
        elseif t == sym_3dots
            take_token(ts)
            l = append!({:(.), :(.), :(.)}, l)
            t = peek_token(ts)
            continue
        elseif t == sym_4dots
            take_token(ts)
            l = append!({:(.), :(.), :(.), :(.)}, l)
            t = peek_token(ts)
            continue
        else
            ex = macrocall_to_atsym(parse_atom(ts))
            return append!(ex.args, l)
        end
    end
end


function parse_import(ts::TokenStream, word)
    path = parse_import_dots(ts)
    while !eof(ts)
        nxt = peek_token(ts)
        if nxt == :(.)
            take_token(ts)
            ex = macrocall_to_atsym(parse_atom(ts))
            append!(ex.args, path)
            continue
        elseif (nxt in ('\n', ';', ',', :(:))) || eof(nxt)
            # reverse path
            return Expr(word, path)
        elseif false #string sub
            #TODO:
        else
            error("invalid \"$word\" statement")
        end
    end
end

function parse_comma_sep(ts::TokenStream, what)
    exprs = {}
    while !eof(ts)
        r = what(ts)
        if peek_token(ts) == ','
            take_token(ts)
            push!(exprs, r)
            continue
        end 
        push!(exprs, r)
        return exprs
    end
end

parse_comma_sep_assigns(ts::TokenStream) = parse_comma_sep(ts, parse_eqs) 

# as above, but allows both "i=r" and "i in r"
function parse_comma_sep_iters(ts::TokenStream)
    ranges = {}
    while !eof(ts)
        r = parse_eqs(ts)
        if r == :(:)
        elseif isa(r, Expr) && r.head == :(=)
        elseif isa(r, Expr) && r.head == :in
            r = Expr(:(=), r.args[1], r.args[2])
        else
            error("invalid iteration spec")
        end
        if peek_token(ts) == ','
            take_token(ts)
            push!(ranges, r)
            continue
        end
        push!(ranges, r)
        return ranges
    end
end
       
function parse_space_separated_exprs(ts::TokenStream)
    # with_space_sensitive
    exprs = {}
    while !eof(ts)
        nt = peek_token(ts)
        if (is_closing_token(nt) ||
            isnewline(nt) ||
            (inside_vector && nt == :for))
            return exprs
        end
        ex = parse_eq(ts)
        if isnewline(peek_token(ts))
            push!(exprs, ex)
            return exprs
        end
        push!(exprs, ex)
    end
end

has_parameters(lst) = length(lst) == 2 && length(first(lst)) == 2 && first(first(lst)) == :params

to_kws(lst) = map((x) -> is_assignment(x) ? Expr(:kw, x[2:end]...) : x, lst)

# handle function call argument list, or any comma-delimited list
# * an extra comma at the end is allowed
# * expressions after a ; are enclosed in (parameters ....)
# * an expression followed by ... becomes (.... x)
function _parse_arglist(ts::TokenStream, closer::Token)
    lst = {} 
    while !eof(ts)
        t = require_token(ts)
        if t == closer
            take_token(ts)
            if closer == ')'
                # (= x y) inside a function call is a keyword argument
                return to_kws(lst)
            else
                return lst
            end
        elseif t == ';'
            take_token(ts)
            # allow f(a, b; )
            peek_token(ts) == closer && continue
            params = parse_arglist(ts, closer)
            if closer == ')'
                lst = to_kws(lst)
            end
            return append!(unshift!(params, :parameters), lst)
        end
        nxt = parse_eqs(ts)
        nt  = require_token(ts)
        if nt == ','
            take_token(ts)
            push!(lst, nxt)
            continue
        elseif nt == ';'
            push!(lst, nxt)
            continue
        elseif c == closer
            push!(lst, nxt)
            continue
        elseif c in (']', '}')
            error("unexpected \"$c\" in argument list")
        else
            error("missing comma or \"$closer\" in argument list")
        end
    end
end

function parse_arglist(ts::TokenStream, closer)
    # with normal ops
    # with whitespace newline
    _parse_arglist(ts, closer)
end

# parse [] concatenation expres and {} cell expressions
function parse_vcat(ts::TokenStream, first, closer)
    lst = {}
    nxt = first
    while !eof(ts)
        t = require_token(ts)
        if t == closer
            take_token(ts)
            ex = Expr(:vcat)
            append!(ex.args, reverse!(unshift!(nxt, lst)))
            return ex
        end
        if t == ','
            take_token(ts)
            if require_token(ts) == closer
                # allow ending with ,
                take_token(ts)
                ex = Expr(:vcat)
                append!(ex.args, reverse!(unshift!(nxt, lst)))
                return ex
            end
            lst = unshift!(lst, nxt)
            nxt = parse_eqs(ts)
            continue
        elseif t == ';'
            error("unexpected semicolon in array expression")
        elseif t == ']' || t == '}'
            error("unexpected \"$t\" in array expression")
        else
            error("missing separator in array expression")
        end
    end
end


function parse_dict(ts::TokenStream, first, closer)
    v = parse_vcat(ts, first, closer)
    if any(is_dict_literal, v.args)
        if all(is_dict_literal, v.args)
            ex = Expr(:dict)
            copy!(ex.args, v.args)
            return ex
        else
            error("invalid dict literal")
        end
    end
end

function parse_comprehension(ts::TokenStream, first, closer)
    r = parse_comma_sep_iters(ts)
    if require_token(ts) == closer
        take_token(ts)
    else
        error("expected $closer")
    end
    return Expr(:comprehension, first, r)
end

function parse_dict_comprehension(ts::TokenStream, first, closer)
    c = parse_comprehension(ts, first, closer)
    if is_dict_literal(c.args[1])
        ex = Expr(:dict_comprehension)
        copy!(ex.args, c.args)
        return ex
    else
        error("invalid dict comprehension")
    end
end


function parse_matrix(ts::TokenStream, first, closer) 
    
    function fix(head, v)
        unshift!(reverse(v), head)
    end

    function update_outer(v, outer)
        if v == nothing
            return outer
        elseif isempty(v.args)
            return unshift!(outer, v.head)
        else
            return unshift!(outer, fix(:row, v))
        end
    end

    semicolon = peek_token(ts) == ';'
    vec   = {first}
    outer = {}
    while !eof(ts)
        t::Token = peek_token(ts) == '\n' ? '\n' : require_token(ts)
        if t == closer
            take_token(ts)
            if length(outer.args) == 1
                return fix(:vcat, update_outer(vec, outer))
            elseif isempty(vec) || isempty(vec.args)
                # [x] => (vcat x)
                return fix(:vcat, vec)
            else
                # [x y] => (hcat x y)
                return fix(:hcat, vec)
            end
        end
        if t == ';' || t == '\n'
            take_token(ts)
            outer = update_outer(vec, outer)
            vec   = {}
            continue
        elseif t == ','
            error("unexpected comma in matrix expression")
        elseif t == ']' || t == '}'
            error("unexpected \"$t\"")
        elseif t == :for
            if !semicolon && length(outer) == 1 && isempty(vec)
                take_token(ts)
                return parse_comprehension(ts, first(outer), closer)
            else
                error("invalid comprehension syntax")
            end
        else
            unshift!(vec, parse_eqs(ts))
            continue
        end
    end
end

function peek_non_newline_token(ts::TokenStream)
    t = peek_token(ts)
    while !eof(t)
        if isnewline(t)
            take_token(ts)
            t = peek_token(ts)
            continue
        end
        return t
    end
end

function parse_cat(ts::TokenStream, closer)
    #with normal ops
    #with inside vec
    if require_token(ts) == closer
        take_token(ts)
        return {}
    end
    first = parse_eqs(ts)
    if is_dict_literal(first)
        nt = peek_non_newline_token(ts)
        if nt == :for
            take_token(ts)
            return parse_dict_comprehension(ts, first, closer)
        else
            return parse_dict(ts, first, closer)
        end
    end
    nt = peek_token(ts)
    if nt == ','
        return parse_vcat(ts, first, closer)
    elseif nt == :for
        take_token(ts)
        return parse_comprehension(ts, first, closer)
    else
        return parse_matrix(ts, first, closer)
    end
end


function parse_stmts_within_expr(ts::TokenStream)
    parse_Nary(ts, parse_eqs, [';'], :block, [',', ')'], true)
end
function parse_tuple(ts::TokenStream, first)
    lst = {}
    nxt = first
    while !eof(ts)
        t = require_token(ts)
        if t == ')'
            take_token(ts)
            return Expr(:tuple, reverse(unshift!(lst, nxt)))
        elseif t == ','
            take_token(ts)
            if require_token(ts) == ')'
                # allow ending with ,
                take_token(ts)
                return Expr(:tuple, reverse(unshift!(lst, nxt)))
            end
            lst = unshift!(lst, nxt)
            nxt = parse_eqs(ts)
            continue
        elseif t == ';'
            error("unexpected semicolon in tuple")
        elseif t == ']' || t == '}'
            error(unexpected \"$(peek_token(ts))\" in tuple")
        else
            error("missing separator in tuple")
        end
    end
end

# TODO: these are unnecessary and the fact that base/client.jl code
# relies on parsing the exact string is troubling
function not_eof_1(c)
    if eof(c)
        error("incomplete: invalid character literal")
    end
    return c
end

function not_eof_2(c)
    if eof(c)
        error("incomplete: invalid \"`\" syntax")
    end
    return c
end

function not_eof_3(c)
    if eof(c)
        error("incomplete: invalid string syntax")
    end
    return c
end 

function parse_backquote(ts::TokenStream)
    buf = IOBuffer()
    c   = Lexer.readchar(ts.io)
    while !eof(ts)
        c == '`' && return true
        if c == '\\'
            nextch = Lexer.readchar(ts.io)
            if nexch == '`'
                write(buf, nextch)
            else
                write(buf, '\\')
                write(buf, not_eof_2(nextch))
            end
        else
            write(buf, not_eof_2(nextch))
        end
        c = Lexer.readchar(ts.io)
        continue
    end
    return Expr(:macrocall, :(@cmd), bytestring(buf))
end

function parse_interpolate(ts::TokenStream)
    c = Lexer.peekchar(ts.io)
    if Lexer.is_identifier_char(c)
        return parse_atom(ts)
    elseif c == '('
        Lexer.readchar(ts.io)
        ex = parse_eqs(ts)
        t  = require_token(ts)
        if t == ')'
            take_token(ts)
            return ex
        else
            error("invalid interpolation syntax")
        end
    else
        error("invalid interpolation syntax: \"$c\"")
    end
end

function _parse_string_literal(ex::Expr, n::Integer, ts::TokenStream, custom::Bool)
    c  = Lexer.readchar(ts.io)
    b  = IOBuffer()
    ex = copy(ex)
    quotes = 0

    while !eof(ts)
        if c == '"'
            if quotes < n
                c = Lexer.readchar(ts.io)
                quotes += 1
                continue
            else
                push!(ex.args, bytestring(b))
                return ex
            end
        elseif qs == 1
            custom || write(b, '\\')
            write(b, '"')
            quotes = 0
            continue
        elseif eq == 2
            custom || write(b, '\\')
            write(b, '"')
            custom || write(b, '\\')
            write(b, '"')
            quotes = 0
            continue
        elseif c == '\\'
            nxch = not_eof_3(Lexer.readchar(ts.io))
            if !custom || !(nxch == '"' || nxch == '\\')
                write(b, '\\')
            end
            write(b, nxch)
            c = readchar(ts.io)
            quotes = 0
            continue
        elseif c == '$' && !custom
            nex = parse_interpolate(ts)
            push!(nex.args, bytestring(b), ex)
            c  = Lexer.readchar(ts.io)
            b  = IOBuffer()
            ex = nex
            quotes = 0
            continue
        else
            write(b, not_eof_3(c))
            c = Lexer.readchar(ts.io)
            quotes = 0
            continue
        end
    end
end

interpolate_string_literal(ex) = length(ex.args) > 1
triplequote_string_literal(ex) = ex.head === :triplequote_string_literal

function unescape_string(str)
    return bytestring(str)
end

function parse_string_literal(ts::TokenStream, custom)
    if Lexer.peekchar(ts.io)
        if Lexer.peekchar(Lexer.takechar(ts.io)) == '"'
            Lexer.takechar(io)
            return  _parse_string_literal(:triple_quoted_string, 2, ts, custom)
        else
            return Expr(:single_quoted_string, "")
        end
    else
        return _parse_string_literal(:single_quoted_string, 0, ts, custom)
    end
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
