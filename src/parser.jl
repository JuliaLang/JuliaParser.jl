# Julia Source Parser
module Parser

using ..Lexer

current_filename = ""

# disable range colon for parsing ternary cond op
const range_colon_enabled = true

# in space sensitvie mode "x -y" is 2 exprs, not subtraction
const space_sensitive = false

# treat "end" like a normal symbol, instead of a reserved word 
const inside_vector = false

# treat newline like ordinary whitespace instead of a reserved word
const end_symbol = false

# treat newline like ordinary whitespace instead of as a potential separator 
const whitespace_newline = false

macro with_normal_ops(body)
    esc(quote
            let range_colon_enabled = false,
                space_sensitive = true
                $body
            end
        end)
end

macro without_range_colon(body)
    esc(quote
            let range_colon_enabled = false
                $body
            end
        end)
end

macro with_inside_vec(body)
    esc(quote
            let space_sensitive = true, 
                inside_vector = true, 
                whitespace_newline = false
                $body
            end
        end)
end

macro with_end_symbol(body)
    esc(quote
            let end_symbol = true
                $body
            end
        end)
end

macro with_whitespace_newline(body)
    esc(quote
            let whitespace_newline = true
                $body
            end
        end)
end

macro without_whitespace_newline(body)
    esc(quote
            let whitespace_newline = false
                $body
            end
        end)
end

macro with_space_sensitive(body)
    esc(quote
            let space_sensitive = true,
                whitespace_newline = true
                $body
            end
        end)
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

# insert line/file for short form function defs,
# otherwise leave alone
short_form_function_loc(ex, lno) = begin
    if (isa(ex, Expr) && ex.head === :(=) &&
        isa(ex.args[1], Expr) && ex.args[1].head === :call)
       block = Expr(:block, Expr(:line, lno, current_filename))
       append!(bl.args, ex.args[2:end])
       return Expr(:(=), ex.args[1], block) 
    else
        return ex
    end
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

const EOF = char(-1)

const is_invalid_initial_token = let
    invalid = Set({')', ']', '}', sym_else, sym_elseif, sym_catch, sym_finally}) 
    is_invalid_initial_token(t::Token) = isa(t, Union(Char, Symbol)) && t in invalid
end

const is_closing_token = let
    closing = Set({',', ')', ']', '}', ';', sym_else, sym_elseif, sym_catch, sym_finally, EOF})
    is_closing_token(t::Token) = isa(t, Union(Char, Symbol)) && t in closing
end

is_dict_literal(ex::Expr) = ex.head === :(=>) && length(ex.args) == 2
is_dict_literal(ex) = false

function parse_chain(ts::TokenStream, down, op) 
    chain = {down(ts)}
    while true 
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
function parse_with_chains(ts::TokenStream, down, ops, chain_op) 
    ex = down(ts)
    while true #!eof(ts)
        t = peek_token(ts)
        if !(t in ops)
            return ex
        end
        take_token(ts)
        if (space_sensitive &&
            ts.isspace &&
            (t in Lexer.unary_and_binary_ops) &&
            Lexer.peekchar(ts.io) != '\\')
            # here we have "x -y"
            put_back!(ts, t)
            return chain
        elseif t == chain_op
            ex = Expr(:call, t, ex, parse_chain(ts, down, t)...)
        else
            ex = Expr(:call, t, ex, down(ts))
        end
    end
    error("end of file in parse_with_chains")
end

function parse_LtoR(ts::TokenStream, down::Function, ops)
    ex = down(ts)
    t  = peek_token(ts)
    while true 
        if !(t in ops)
            return ex
        end
        take_token(ts)
        if Lexer.is_syntactic_op(t) || t === :(in)
            ex = Expr(t, ex, down(ts))
            t  = peek_token(ts)
        else
            ex = Expr(:call, t, ex, down(ts))
            t  = peek_token(ts)
        end
    end
    error("end of file in parse_LtoR")
end

function parse_RtoL(ts::TokenStream, down::Function, ops)
    while true #!eof(ts)
        ex = down(ts)
        t  = peek_token(ts)
        if !(t in ops)
            return ex
        end
        take_token(ts)
        if (space_sensitive && isspace(ts) &&
            (t in Lexer.unary_and_binary_ops) &&
            Lexer.peekchar(ts.io) != '\\')
            put_back!(ts, t)
            return ex
        elseif Lexer.is_syntactic_op(t)
            return Expr(t, ex, parse_RtoL(ts, down, ops))
        elseif t === :(~)
            args = parse_chain(ts, down, :(~))
            if peek_token(ts) in ops
                #XXX: this is wrong
                return Expr(:macrocall, symbol("@~"), ex, args[1:end-1]...)
            else
                return Expr(:macrocall, symbol("@~"), ex, args...)
            end
        else
            return Expr(:call, t, ex, parse_RtoL(ts, down, ops)...)
        end
    end
    error("end of file in parse_RtoL")
end

function parse_cond(ts::TokenStream)
    ex = parse_or(ts)
    if peek_token(ts) == '?'
        take_token(ts)
        then = @without_range_colon parse_eq(ts)
        take_token(ts) == ':' || error("colon expected in \"?\" expression")
        return Expr(:if, ex, then, parse_cond(ts))
    end
    #=
    elseif isa(ex, String)
        args = ex.args
        while true #!eof(ts)
            nxt = peek_token(ts)
            if Lexer.eof(nxt) || is_closing_token(nxt) || Lexer.isnewline(nxt)
                ex = Expr(:call, :(top), :string)
                append!(ex.args, args)
                return ex
            end
            push!(args, parse_or(ts))
        end
    =#
    return ex
end

function parse_Nary(ts::TokenStream, down::Function, ops, 
                    head::Symbol, closers, allow_empty::Bool)
    t = require_token(ts)
    if is_invalid_initial_token(t)
        error("unexpected \"$t\"")
    end
    # empty block
    # Note: "\n" has a int value of 10 so we need a typecheck here
    if isa(t, Union(Char, Symbol)) && t in closers
        return Expr(head)
    end
    local args::Vector{Any}
    # in allow empty mode, skip leading runs of operator
    if allow_empty && t in ops 
        args = {}
    elseif '\n' in ops
        # nore line-number must happend before (down s)
        loc = line_number_node(ts)
        args = {loc, down(ts)}
    else
        args = {down(ts)}
    end
    isfirst = true
    t = peek_token(ts)
    while true 
        if !(t in ops)
            if !(Lexer.eof(t) || t == '\n' || (',' in ops) || (t in closers))
                error("extra token \"$t\" after end of expression")
            end
            if isempty(args) || length(args[2:end]) == 2 || !isfirst
                # {} => Expr(:head)
                # {ex1, ex2} => Expr(head, ex1, ex2)
                # (ex1) if operator appeared => Expr(head,ex1) (handles "x;")
                ex = Expr(head)
                append!(ex.args, args)
                return ex 
            else
                # {ex1} => ex1
                return first(args)
            end
        end
        take_token(ts)
        isfirst = false
        nt = peek_token(ts)
        # allow input to end with the operator, as in a;b;
        if Lexer.eof(nt) || (nt in closers) || (allow_empty && nt in ops) ||  
           (length(ops) == 1 && first(ops) == ',' && nt === :(=))
            t = nt
            continue
        end
        if '\n' in ops
            loc = line_number_node(ts)
            append!(args, {loc, down(ts)})
            t = nt
            continue
        else
            push!(args, down(ts))
            continue
        end
    end
end 

# the principal non-terminals follow, in increasing precedence order
function parse_block(ts::TokenStream)
    ex = parse_Nary(ts, parse_eq, ('\n', ';'), :block,
                      (sym_end, sym_else, sym_elseif, sym_catch, sym_finally), true)
    return ex
end

# for sequenced eval inside expressions, e.g. (a;b, c;d)
function parse_stmts_within_expr(ts::TokenStream)
    return parse_Nary(ts, parse_eqs, (';',), :block, (',', ')'), true)
end

#; at the top level produces a sequence of top level expressions
function parse_stmts(ts::TokenStream)
    ex = parse_Nary(ts, parse_eq, (';',), :toplevel, ('\n',), true)
    # check for unparsed junk after an expression
    t = peek_token(ts)
    if !(Lexer.eof(t) || t === '\n')
        error("extra token \"$t\" after end of expression")
    end
    return ex
end

parse_eq(ts::TokenStream) = begin 
    lno = curline(ts)
    ex  = parse_RtoL(ts, parse_comma, Lexer.precedent_ops(1))
    return short_form_function_loc(ex, lno)
end

# parse-eqs is used where commas are special for example in an argument list 
parse_eqs(ts::TokenStream) =  parse_RtoL(ts, parse_cond, Lexer.precedent_ops(1))

# parse-comma is neeed for commas outside parens, for example a = b, c
parse_comma(ts::TokenStream) = parse_Nary(ts, parse_cond, (',',), :tuple, (), false)

parse_or(ts::TokenStream)    = parse_LtoR(ts, parse_and, Lexer.precedent_ops(3))
parse_and(ts::TokenStream)   = parse_LtoR(ts, parse_arrow, Lexer.precedent_ops(4))
parse_arrow(ts::TokenStream) = parse_RtoL(ts, parse_ineq, Lexer.precedent_ops(5))
parse_ineq(ts::TokenStream)  = parse_comparison(ts, Lexer.precedent_ops(6))

const expr_ops = Lexer.precedent_ops(9)
parse_expr(ts::TokenStream)  = parse_with_chains(ts, parse_shift, expr_ops, :(+))
parse_shift(ts::TokenStream) = parse_LtoR(ts, parse_term, Lexer.precedent_ops(10))

const term_ops = Lexer.precedent_ops(11)
parse_term(ts::TokenStream)     = parse_with_chains(ts, parse_rational, term_ops, :(*))
parse_rational(ts::TokenStream) = parse_LtoR(ts, parse_unary, Lexer.precedent_ops(12))
parse_pipes(ts::TokenStream)    = parse_LtoR(ts, parse_range, Lexer.precedent_ops(7))

parse_in(ts::TokenStream)       = parse_LtoR(ts, parse_pipes, (:(in),))

function parse_comparison(ts::TokenStream, ops)
    ex = parse_in(ts)
    isfirst = true
    while true 
        t = peek_token(ts)
        if !(t in ops)
            return ex
        end
        take_token(ts)
        if isfirst
            isfirst = false
            ex = Expr(:comparison, ex, t, parse_range(ts))
        else
            append!(ex.args, {t, parse_range(ts)}) 
        end
    end
end

is_large_number(n::BigInt) = true
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

isnumber(n::Number) = true
isnumber(n) = false

const is_juxtaposed = let
    invalid_chars = Set{Char}({'(', '[', '{'})

    is_juxtaposed(ex, t::Token) = begin
        return !(Lexer.is_operator(t)) &&
               !(Lexer.is_operator(ex)) &&
               !(t in Lexer.reserved_words) &&
               !(is_closing_token(t)) &&
               !(Lexer.isnewline(t)) &&
               !(isa(ex, Expr) && ex.head === :(...)) &&
               (isnumber(ex) || !(isa(t, Char) && t in invalid_chars))
    end
end


function parse_juxtaposed(ts::TokenStream, ex) 
    nxt = peek_token(ts)
    # numeric literal juxtaposition is a unary operator
    if is_juxtaposed(ex, nxt) && !ts.isspace
        return Expr(:call, :(*), parse_unary(ts))
    end
    return ex
end


function parse_range(ts::TokenStream)
    ex = parse_expr(ts)
    isfirst = true
    while true
        t = peek_token(ts)
        spc = ts.isspace
        if isfirst && t == :(..)
           take_token(ts)
           return Expr(:call, t, ex, parse_expr(ts))
        end
        if range_colon_enabled && t == :(:)
            take_token(ts)
            if (space_sensitive && spc && (peek_token(ts) || true) && !ts.isspace)
                # "a :b" in space sensitive mode
                put_back!(ts, :(:))
                return ex
            end
            if is_closing_token(peek_token(ts))
                error("deprecated syntax x[i:]")
            elseif Lexer.isnewline(peek_token(ts))
                error("line break in \":\" expression")
            end
            arg = parse_expr(ts)
            if ts.isspace == false && (arg === :(<) || arg === :(>))
                error("\":$argument\" found instead of \"$argument:\"")
            end
            if isfirst
                ex = Expr(t, ex, arg)
                isfirst = false
            else
                push!(ex.args, arg)
                isfirst = true
            end
            continue
        elseif t == :(...)
            take_token(ts)
            return Expr(:(...), ex)
        else
            return ex
        end
    end
end 

function parse_decl(ts::TokenStream)
    local ex
    if peek_token(ts) == :(::)
        take_token(ts)
        ex = Expr(:(::), parse_call(ts))
    else
        ex = parse_call(ts)
    end
    while true 
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
    return parse_factorh(ts, parse_decl, Lexer.precedent_ops(12))
end

function parse_unary(ts::TokenStream)
    t = require_token(ts)
    is_closing_token(t) && error("unexpected $t")
    if !(t in Lexer.unary_ops)
        pf = parse_factor(ts)
        return parse_juxtaposed(ts, pf) 
    end
    op = take_token(ts)
    nc = Lexer.peekchar(ts.io)
    if (op == :(-) || op == :(+)) && (isdigit(nc) || nc == '.')
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
        if is_closing_token(nt) || Lexer.isnewline(nt)
            # return operator by itself, as in (+)
            return op
        elseif nt == '{'
            # this case is +{T}(x::T)
            put_back!(ts, op)
            return pase_factor(ts)
        else
            arg = parse_unary(ts)
            if isa(arg, Expr) && arg.head === :tuple
                return Expr(:call, op, arg.args[1])
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
subtype_syntax(ex::Symbol) = ex

function parse_unary_prefix(ts::TokenStream)
    op = peek_token(ts)
    if Lexer.is_syntactic_unary_op(op)
        take_token(ts)
        if is_closing_token(peek_token(ts))
            return op
        elseif op === :(&)
            return Expr(op, parse_call(ts))
        else
            return Expr(op, parse_atom(ts))
        end
    end
    return parse_atom(ts)
end

# parse function all, indexing, dot, and transpose expressions
# also handles looking for reserved words 

function parse_call(ts::TokenStream)
    ex = parse_unary_prefix(ts)
    if ex in Lexer.reserved_words
        ex = parse_resword(ts, ex)
    else
        ex = parse_call_chain(ts, ex, false)
    end
    return ex
end

function separate(f::Function, collection)
    tcoll, fcoll = {}, {}
    for c in collection
        f(c) ? push!(tcoll, c) : push!(fcoll, c)
    end
    return (tcoll, fcoll)
end

function parse_call_chain(ts::TokenStream, ex, one_call::Bool)
    temp = ['(', '[', '{', '\'', '"']
    while true 
        t = peek_token(ts)
        if (space_sensitive && ts.isspace && (t in temp)) || 
           (isa(ex, Number) && t == '(')
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
            if isa(ex, Symbol) && !Lexer.is_operator(ex) && !ts.isspace
                # custom prefexed string literals x"s" => @x_str "s"
                take_token(ts)
                str = parse_string_literal(ts, true)
                nt  = peek_token(ts)
                suffix  = triplequote_strng_literal(str) ? :(_mstr) : :(_str)
                macname = symbol(string('@', ex, suffix))
                macstr  = str[2:end]
                
                if isa(nt, Symbol) && !Lexer.is_operator(nt) && !ts.isspace
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

const expect_end_current_line = 0

function _expect_end(ts::TokenStream, word)
    t = peek_token(ts)
    if t == sym_end
        take_token(ts)
    elseif Lexer.eof(t)
        err_msg = "incomplete: \"$word\" at {current_filename} : {expected} requires end"
        error(err_msg)
    else
        err_msg = "incomplete: \"$word\" at {current filename} : {expected} \"end\", got \"$t\""
        error(err_msg)
    end
end


parse_subtype_spec(ts::TokenStream) = subtype_syntax(parse_ineq(ts))

# parse expressions or blocks introduced by syntatic reserved words
function parse_resword(ts::TokenStream, word::Symbol)
    #XXX: with bindings
    expect_end_current_line = curline(ts)
    
    expect_end(ts::TokenStream) = _expect_end(ts, word) 

    @with_normal_ops begin
        @without_whitespace_newline begin
            if word == :quote || word == :begin
                Lexer.skip_ws_and_comments(ts.io)
                loc = line_number_filename_node(ts)
                blk = parse_block(ts)
                expect_end(ts)
                local ex::Expr
                if !isempty(blk.args) && isa(blk.args[1], Expr) && blk.args[1].head === :line
                    ex = Expr(:block, loc)
                    append!(ex.args, blk.args[2:end])
                else
                    ex = blk
                end
                return word === :quote ? QuoteNode(ex) : ex

            elseif word == :while
                ex = Expr(:while, parse_cond(ts), parse_block(ts))
                expect_end(ts)
                return ex

            elseif word == :for
                ranges = parse_comma_sep_iters(ts)
                body   = parse_block(ts)
                expect_end(ts)
                #r = nothing
                #if r == nothing
                #    return body
                #else
                return Expr(:for, ranges[1], body) 
                #end

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
                take_token(ts)
                if nxt == sym_end
                    return Expr(:if, test, then)
                elseif nxt == sym_elseif
                    if Lexer.isnewline(peek_token(ts))
                        error("missing condition in elseif at {filename} : {line}")
                    end
                    blk = Expr(:block, line_number_node(ts), parse_resword(ts, :if))
                    return Expr(:if, test, then, blk)
                elseif nxt == sym_else
                    if peek_token(ts) == :if
                        error("use elseif instead of else if")
                    end
                    blk = parse_block(ts)
                    ex = Expr(:if, test, then, blk) 
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
                if !(Lexer.eof(nt) || (nt in ('\n', ';', sym_end)))
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
                    return Expr(:const, Expr(head, args...))
                else
                    return Expr(head, args...)
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
                if isimmutable && peek_token(ts) == :type
                    # allow "immutable type"
                    take_token(ts)
                end
                sig = parse_subtype_spec(ts)
                blk = parse_block(ts)
                ex  = Expr(:type, istype, sig, blk) 
                expect_end(ts)
                return ex

            elseif word == :bitstype
                stmnt = let space_sensitive=true
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
                        return finalb != nothing ? Expr(:try, try_block, catchv, catchb, finalb) :
                                                   Expr(:try, try_block, catchv, catchb)
                    end
                    if nxt == sym_catch && catchb == nothing
                        nl = Lexer.isnewline(peek_token(ts))
                        if require_token(ts) in (sym_end, sym_finally)
                            nxt    = require_token(ts)
                            catchb = Expr(:block)
                            catchv = false 
                            continue
                        else
                            var   = parse_eqs(ts)
                            isvar = nl == false && isa(var, Symbol)
                            catch_block = require_token(ts) == sym_finally? Expr(:block) : parse_block(ts)
                            nxt = require_token(ts)
                            catchb = isvar ? catch_block : Expr(:block, var, catch_block.args...)
                            catchv = isvar && var != nothing
                            continue
                        end
                    elseif nxt == sym_finally && finalb == nothing
                        finalb = require_token(ts) == sym_catch ? Expr(:block) : parse_block(ts)
                        nxt = require_token(ts)
                        continue 
                    else
                        error("unexpected \"$nxt\"")
                    end
                end

            elseif word == :return
                t = peek_token(ts)
                if Lexer.isnewline(t) || is_closing_token(t)
                    return Expr(:return, nothing)
                else
                    return Expr(:return, parse_eq(ts))
                end

            elseif word == :break || word == :continue
                return Expr(word)

            elseif word == :const
                assgn = parse_eq(ts)
                if !isa(assgn, Expr &&
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
        end
    end
end

function add_filename_to_block!(body::Expr, loc)
    if !isempty(body.args) && isa(body.args[1], Expr) && body.args[1].head == :line
        body.args[1] = loc
    end
    return body
end

function parse_do(ts::TokenStream)
    # TODO: bindings
    local doargs
    if Lexer.isnewline(peek_token(ts))
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
        return append!(frst, rest)
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
    while true 
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
# return a list of range expressions
function parse_comma_sep_iters(ts::TokenStream)
    ranges = {}
    while true 
        r = parse_eqs(ts)
        if r == :(:)
        elseif isa(r, Expr) && r.head == :(=)
        elseif isa(r, Expr) && r.head == :in
            r = Expr(:(=), r.args[1], r.args[2:end]...)
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
    @with_space_sensitive begin
        exprs = {}
        while true 
            nt = peek_token(ts)
            if is_closing_token(nt) || Lexer.isnewline(nt) || (inside_vector && nt == :for)
                return exprs
            end
            ex = parse_eq(ts)
            if Lexer.isnewline(peek_token(ts))
                push!(exprs, ex)
                return exprs
            end
            push!(exprs, ex)
        end
    end
end

has_parameters(lst) = length(lst) == 2 && length(first(lst)) == 2 && first(first(lst)) == :params

to_kws(lst) = map((x) -> Lexer.is_assignment(x) ? Expr(:kw, x[2:end]...) : x, lst)

# handle function call argument list, or any comma-delimited list
# * an extra comma at the end is allowed
# * expressions after a ; are enclosed in (parameters ....)
# * an expression followed by ... becomes (.... x)
function _parse_arglist(ts::TokenStream, closer::Token)
    lst = {} 
    while true #!eof(ts)
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
            if peek_token(ts) == closer
                continue
            end
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
        elseif nt == closer
            push!(lst, nxt)
            continue
        elseif nt in (']', '}')
            error("unexpected \"$nt\" in argument list")
        else
            error("missing comma or \"$closer\" in argument list")
        end
    end
end

function parse_arglist(ts::TokenStream, closer)
    @with_normal_ops begin
        @with_whitespace_newline begin
            return _parse_arglist(ts, closer)
        end
    end
end

# parse [] concatenation expres and {} cell expressions
function parse_vcat(ts::TokenStream, frst, closer)
    lst = {}
    nxt = frst
    while true #!eof(ts)
        t = require_token(ts)
        if t == closer
            take_token(ts)
            ex = Expr(:vcat)
            append!(ex.args, reverse!(unshift!(lst, nxt)))
            return ex
        end
        if t == ','
            take_token(ts)
            if require_token(ts) == closer
                # allow ending with ,
                take_token(ts)
                ex = Expr(:vcat)
                append!(ex.args, reverse!(unshift!(lst, nxt)))
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


function parse_dict(ts::TokenStream, frst, closer)
    v = parse_vcat(ts, frst, closer)
    if any(is_dict_literal, v.args)
        if all(is_dict_literal, v.args)
            ex = Expr(:dict)
            ex.args = v.args 
            return ex
        else
            error("invalid dict literal")
        end
    end
end

function parse_comprehension(ts::TokenStream, frst, closer)
    rs = parse_comma_sep_iters(ts)
    if require_token(ts) == closer
        take_token(ts)
    else
        error("expected $closer")
    end
    ex = Expr(:comprehension, frst)
    append!(ex.args, rs)
    return ex
end

function parse_dict_comprehension(ts::TokenStream, frst, closer)
    c = parse_comprehension(ts, frst, closer)
    if is_dict_literal(c.args[1])
        ex = Expr(:dict_comprehension)
        copy!(ex.args, c.args)
        return ex
    else
        error("invalid dict comprehension")
    end
end

function parse_matrix(ts::TokenStream, frst, closer) 
    
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
    vec   = Expr(frst)
    outer = {}
    while true #!eof(ts)
        t::Token = peek_token(ts) == '\n' ? '\n' : require_token(ts)
        if t == closer
            take_token(ts)
            if isa(outer, Expr) && length(outer.args) == 1
                return fix(:vcat, update_outer(vec, outer))
            elseif isempty(vec) || (isa(vec, Expr) && isempty(vec.args))
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
    while true#!eof(ts)
        if Lexer.isnewline(t)
            take_token(ts)
            t = peek_token(ts)
            continue
        end
        return t
    end
end

function parse_cat(ts::TokenStream, closer)
    @with_normal_ops begin
        @with_inside_vec begin
            if require_token(ts) == closer
                take_token(ts)
                if closer == '}'
                    return Expr(:cell1d)
                elseif closer == ']'
                    return Expr(:vcat)
                else
                    error("unknown closer $closer")
                end
            end
            frst = parse_eqs(ts)
            if is_dict_literal(frst)
                nt = peek_non_newline_token(ts)
                if nt == :for
                    take_token(ts)
                    return parse_dict_comprehension(ts, frst, closer)
                else
                    return parse_dict(ts, frst, closer)
                end
            end
            nt = peek_token(ts)
            if nt == ','
                return parse_vcat(ts, frst, closer)
            elseif nt == :for
                take_token(ts)
                return parse_comprehension(ts, frst, closer)
            else
                return parse_matrix(ts, frst, closer)
            end
        end
    end
end

function parse_tuple(ts::TokenStream, frst)
    args = {}
    nxt = frst
    while true
        t = require_token(ts)
        if t == ')'
            take_token(ts)
            push!(args, nxt)
            ex = Expr(:tuple)
            ex.args = args
            return ex
        end
        if t == ','
            take_token(ts)
            if require_token(ts) == ')'
                # allow ending with ,
                take_token(ts)
                push!(args, nxt)
                ex = Expr(:tuple)
                ex.args = args
                return ex
            end
            args = push!(args, nxt) 
            nxt  = parse_eqs(ts)
            continue
        elseif t == ';'
            error("unexpected semicolon in tuple")
        elseif t == ']' || t == '}'
            error("unexpected \"$(peek_token(ts))\" in tuple")
        else
            error("missing separator in tuple")
        end
    end
end

# TODO: these are unnecessary and the fact that base/client.jl code
# relies on parsing the exact string is troubling
function not_eof_1(c)
    if Lexer.eof(c)
        error("incomplete: invalid character literal")
    end
    return c
end

function not_eof_2(c)
    if Lexer.eof(c)
        error("incomplete: invalid \"`\" syntax")
    end
    return c
end

function not_eof_3(c)
    if Lexer.eof(c)
        error("incomplete: invalid string syntax")
    end
    return c
end 

#TODO; clean up eof handling
function parse_backquote(ts::TokenStream)
    buf = IOBuffer()
    c   = Lexer.readchar(ts.io)
    while true 
        c == '`' && break
        if c == '\\'
            nc = Lexer.readchar(ts.io)
            if nc == '`'
                write(buf, nc)
            else
                write(buf, '\\')
                write(buf, not_eof_2(nc))
            end
        else
            write(buf, not_eof_2(c))
        end
        c = Lexer.readchar(ts.io)
        continue
    end
    return Expr(:macrocall, symbol("@cmd"), bytestring(buf))
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

function _parse_string_literal(head::Symbol, n::Integer, ts::TokenStream, custom::Bool)
    c  = Lexer.readchar(ts.io)
    b  = IOBuffer()
    ex = Expr(head)
    quotes = 0

    while true #!eof(ts)
        if c == '"'
            if quotes < n
                c = Lexer.readchar(ts.io)
                quotes += 1
                continue
            else
                push!(ex.args, bytestring(b))
                return ex
            end
        elseif quotes == 1
            custom || write(b, '\\')
            write(b, '"')
            quotes = 0
            continue
        elseif quotes == 2
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

interpolate_string_literal(ex) = isa(ex, Expr) && length(ex.args) > 1
triplequote_string_literal(ex) = isa(ex, Expr) && ex.head === :triple_quoted_string

function parse_string_literal(ts::TokenStream, custom)
    pc = Lexer.peekchar(ts.io)
    if pc == '"'
        Lexer.takechar(io)
        if Lexer.peekchar(io) == '"'
            return  _parse_string_literal(:triple_quoted_string, 2, ts, custom)
        else
            return Expr(:single_quoted_string, "")
        end
    else
        return _parse_string_literal(:single_quoted_string, 0, ts, custom)
    end
end

function _parse_atom(ts::TokenStream)
    t = require_token(ts)
    #Note: typeof(t) == Char, isa(t, Number) == true
    if !isa(t, Char) && isa(t, Number)
        return take_token(ts)
    
    # char literal
    elseif t == '\''  || t == symbol("'")
        take_token(ts)
        fch = Lexer.readchar(ts.io)
        fch == '\'' && error("invalid character literal")
        if fch != '\\' && !Lexer.eof(fch) && Lexer.peekchar(ts.io) == '\''
            # easy case 1 char no \
            Lexer.takechar(ts.io)
            return fch
        else
            b = IOBuffer()
            c = fch
            while true
                c == '\'' && break
                write(b, not_eof_1(c))
                c == '\\' && write(b, not_eof_1(Lexer.readchar(ts.io)))
                c = Lexer.readchar(ts.io)
                continue
            end
            str = unescape_string(bytestring(b))
            if length(str) == 1
                # one byte e.g. '\xff' maybe not valid UTF-8
                # but we watn to use the raw value as a codepoint in this case
                #wchar str[0] #this would throw an error during the conversion above
                if length(str) != 1  || is_valid_utf8(str)
                    error("invalid character literal")
                end
                return str[0]
            end
        end

    # symbol / expression quote
    elseif t == :(:)
        take_token(ts)
        if is_closing_token(peek_token(ts))
            return :(:)
        else
            ex = _parse_atom(ts)
            return isa(ex, Symbol) ? QuoteNode(ex) : Expr(:quote, ex)
        end
    
    # misplaced =
    elseif t == :(=)
        error("unexpected \"=\"")

    # identifier
    elseif isa(t, Symbol)
        return take_token(ts)

    # parens or tuple
    elseif t == '('
        take_token(ts)
        @with_normal_ops begin
            @with_whitespace_newline begin
                if require_token(ts) == ')'
                    # empty tuple
                    take_token(ts)
                    return Expr(:tuple)
                elseif peek_token(ts) in Lexer.syntactic_ops
                    # allow (=) etc.
                    t = take_token(ts)
                    if require_token(ts) != ')'
                        error("invalid identifier name \"$t\"")
                    end
                    take_token(ts)
                    return t
                else
                    # here we parse the first subexpression separately,
                    # so we can look for a comma to see if it is a tuple
                    # this lets us distinguish (x) from (x,)
                    ex = parse_eqs(ts)
                    t  = require_token(ts)
                    if t == ')'
                        take_token(ts)
                        if isa(ex, Expr) && length(ex.args) == 1 && ex.head == :(...)
                            # (ex...)
                            return Expr(:tuple, ex)
                        else
                            # value in parens (x)
                            return ex
                        end
                    elseif t == ','
                        # tuple (x,) (x,y) (x...) etc
                        return parse_tuple(ts, ex)
                    elseif t == ';'
                        #parenthesized block (a;b;c)
                        take_token(ts)
                        if require_token(ts) == ')'
                            # (ex;)
                            take_token(ts)
                            return Expr(:block, ex)
                        else
                            blk = parse_stmts_within_expr(ts)
                            tok = require_token(ts)
                            if tok == ','
                                error("unexpected comma in statment block")
                            elseif tok != ')'
                                error("missing separator in statement block")
                            end
                            take_token(ts)
                            return Expr(:block, ex, blk)
                        end
                    elseif t == ']' || t == '}'
                        error("unexpected \"$t\" in tuple")
                    else
                        error("missing separator in tuple")
                    end
                end
            end
        end
   
    # cell expression
    elseif t == '{'
        take_token(ts)
        if require_token(ts) == '}'
            take_token(ts)
            return Expr(:cell1d)
        end
        vex = parse_cat(ts, '}')
        if isempty(vex.args)
            return Expr(:cell1d)
        end
        if vex.head === :comprehension
            ex = Expr(:typed_comprehension, TopNode(:Any))
            append!(ex.args, vex.args)
            return ex
        elseif vex.head == :dict_comprehension
            ex = Expr(:typed_comprehension, Expr(:(=>), TopNode(:Any), TopNode(:Any)))
            append!(ex.args, vex.args)
            return ex
        elseif vex.head == :dict
            ex = Expr(:typed_dict, Expr(:(=>), TopNode(:Any), TopNode(:Any)))
            append!(ex.args, vex.args)
            return ex
        elseif vex.head == :hcat
            ex = Expr(:cell2d, 1, length(vex.args))
            append!(ex.args, vex.args)
            return ex
        else # vcat(...)
            if length(vex.args) == 1 && vex.args[1].head == :row
                nr = length(vex.args)
                nc = length(vex.args[1].args)
                # make sure all rows are the same length
                fn = (x::Expr) -> length(x.args) == 1 && 
                                  x.head === :row && 
                                  length(x.args) == nc
                if !(all(fn, vex.args[1].args))
                    error("inconsistent shape in cell expression")
                end
                ex = Expr(:cell2d, nr, nc)
                #XXX transpose to storage order
                return ex
             end
             if any(x -> begin isa(x, Expr) && 
                               length(x.args) == 1 && 
                               x.args[1].head == :row
                         end, vex.args[2:end])
                error("inconsistent shape in cell expression")
             end
             ex = Expr(:cell1d)
             append!(ex.args, vex.args)
             return ex
        end

    # cat expression
    elseif t == '['
        take_token(ts)
        vex = parse_cat(ts, ']')
        return vex

    # string literal
    elseif t == '"'
        take_token(ts)
        ps = parse_string_literal(ts, false)
        if triplequote_string_literal(ps)
            ex = Expr(:macrocall, :(@mstr))
            append!(ex.args, ps.args)
            return ex
        elseif interpolate_string_literal(ps)
            ex = Expr(:string)
            append!(ex.args, filter((s) -> isa(s, String) && length(s) != 0, ps.args))
            return ex
        else
            return ps.args[1]
        end

    # macrocall
    elseif t == '@'
        take_token(ts)
        @with_space_sensitive begin
            head = parse_unary_prefix(ts)
            t = peek_token(ts)
            if ts.isspace
                ex = Expr(:macrocall, macroify_name(head))
                append!(ex.args, parse_space_separated_exprs(ts))
                return ex
            else
                call = parse_call_chain(ts, head, true)
                if isa(call, Expr) && call.head == :call
                    ex = Expr(:macrocall, macroify_name(call.args[1]))
                    append!(ex.args, call.args[2:end])
                    return ex
                else
                    ex = Expr(:macrocall, macroify_name(call))
                    append!(ex.args, parse_space_separated_exprs(ts))
                    return ex
                end
            end
        end
    
    # command syntax
    elseif t == '`'
        take_token(ts)
        return parse_backquote(ts)

    else
        error("invalid syntax: \"$(take_token(ts))\"")
    end
end

function parse_atom(ts::TokenStream)
    ex = _parse_atom(ts)
    if (ex in Lexer.syntactic_ops) || ex == :(...)
        error("invalid identifier name \"$ex\"")
    end
    return ex
end

function is_valid_modref(ex::Expr)
    return length(ex.args) == 2 &&
           ex.head === :(.) &&
           isa(ex.args[2], Expr) && 
           ex.args[2].head == :quote &&
           isa(ex.args[2].args[1], Symbol) &&
           (isa(ex.args[1], Symbol) || valid_modref(ex.args[1]))
end

function macroify_name(ex)
    if isa(ex, Symbol)
        return symbol(string('@', ex))
    elseif is_valid_modref(ex)
        return Expr(:(.), ex.args[1], 
                    Expr(:quote, macroify_name(ex.args[2].args[1])))
    else
        error("invalid macro use \"@($ex)")
    end
end


#========================#
# Parser Entry Method
#========================#

function parse(ts::TokenStream)
    Lexer.skip_ws_and_comments(ts.io)
    t::Token = next_token(ts)
    while true
        Lexer.eof(t) && return nothing
        if Lexer.isnewline(t)
            t = next_token(ts)
            continue
        end
        break
    end
    put_back!(ts, t)
    return parse_stmts(ts)
end

parse(io::IO) = parse(TokenStream(io))
parse(str::String) = parse(TokenStream(IOBuffer(str)))

end
