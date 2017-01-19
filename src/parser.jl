# Julia Source Parser
module Parser

using Compat
using ..Lexer
using ..Lexer: here
using ..Tokens
using ..Tokens: √
using ..Diagnostics: diag, before, after, Incomplete, Diagnostic
using Base.Meta

using AbstractTrees: children

export parse

typealias CharSymbol @compat(Union{Char, Symbol})

type ParseState
    # disable range colon for parsing ternary cond op
    range_colon_enabled::Bool

    # in space sensitvie mode "x -y" is 2 exprs, not subtraction
    space_sensitive::Bool

    # treat "end" like a normal symbol, instead of a reserved word
    inside_vector::Bool

    # treat newline like ordinary whitespace instead of a reserved word
    end_symbol::Bool

    # treat newline like ordinary whitespace instead of as a potential separator
    whitespace_newline::Bool
end

ParseState() = ParseState(true, false, false, false, false)

peek_token(ps::ParseState, ts::TokenStream)    = Lexer.peek_token(ts, ps.whitespace_newline)
next_token(ps::ParseState, ts::TokenStream)    = Lexer.next_token(ts, ps.whitespace_newline)
require_token(ps::ParseState, ts::TokenStream) = Lexer.require_token(ts, ps.whitespace_newline)

macro with_normal_ops(ps, body)
    quote
        local tmp1 = $(esc(ps)).range_colon_enabled
        local tmp2 = $(esc(ps)).space_sensitive
        try
            $(esc(ps)).range_colon_enabled = true
            $(esc(ps)).space_sensitive     = false
            $(esc(body))
        finally
            $(esc(ps)).range_colon_enabled = tmp1
            $(esc(ps)).space_sensitive     = tmp2
        end
    end
end

macro without_range_colon(ps, body)
    quote
        local tmp1 = $(esc(ps)).range_colon_enabled
        try
            $(esc(ps)).range_colon_enabled = false
            $(esc(body))
        finally
            $(esc(ps)).range_colon_enabled = tmp1
        end
    end
end

macro with_inside_vec(ps, body)
    quote
        local tmp1 = $(esc(ps)).space_sensitive
        local tmp2 = $(esc(ps)).inside_vector
        local tmp3 = $(esc(ps)).whitespace_newline
        try
            $(esc(ps)).space_sensitive = true
            $(esc(ps)).inside_vector   = true
            $(esc(ps)).whitespace_newline = false
            $(esc(body))
        finally
            $(esc(ps)).space_sensitive = tmp1
            $(esc(ps)).inside_vector   = tmp2
            $(esc(ps)).whitespace_newline = tmp3
        end
    end
end

macro with_end_symbol(ps, body)
    quote
        local tmp1 = $(esc(ps)).end_symbol
        try
            $(esc(ps)).end_symbol = true
            $(esc(body))
        finally
            $(esc(ps)).end_symbol = tmp1
        end
    end
end

macro with_whitespace_newline(ps, body)
    quote
        local tmp1 = $(esc(ps)).whitespace_newline
        try
            $(esc(ps)).whitespace_newline = true
            $(esc(body))
        finally
            $(esc(ps)).whitespace_newline = tmp1
        end
    end
end

macro without_whitespace_newline(ps, body)
    quote
        local tmp1 = $(esc(ps)).whitespace_newline
        try
            $(esc(ps)).whitespace_newline = false
            $(esc(body))
        finally
            $(esc(ps)).whitespace_newline = tmp1
        end
    end
end

macro space_sensitive(ps, body)
    quote
        local tmp1 = $(esc(ps)).space_sensitive
        local tmp2 = $(esc(ps)).whitespace_newline
        try
            $(esc(ps)).space_sensitive = true
            $(esc(ps)).whitespace_newline = false
            $(esc(body))
        finally
            $(esc(ps)).space_sensitive    = tmp1
            $(esc(ps)).whitespace_newline = tmp2
        end
    end
end

curline(ts::TokenStream)  = ts.lineno
filename(ts::TokenStream) = Symbol(ts.filename)

line_number_node(ts) = LineNumberNode(curline(ts))
line_number_filename_node(lno, filename) = Expr(:line, lno, filename)
line_number_filename_node(ts::TokenStream) =
    line_number_filename_node(curline(ts), filename(ts)) ⤄ Lexer.nullrange(ts)

# insert line/file for short form function defs, otherwise leave alone
function short_form_function_loc(ts, ex, lno, filename)
    if isa(¬ex, Expr) && (¬ex).head === :(=) && isa((¬ex).args[1], Expr) && (¬ex).args[1].head === :call
       block = ⨳(:block, line_number_filename_node(lno, filename)) ⤄ Lexer.nullrange(ts)
       args = collect(children(ex))
       block ⪥ args[2:end]
       return ⨳(:(=), args[1], block)
   end
   return ex
end

const SYM_DO      = Symbol("do")
const SYM_ELSE    = Symbol("else")
const SYM_ELSEIF  = Symbol("elseif")
const SYM_END     = Symbol("end")
const SYM_CATCH   = Symbol("catch")
const SYM_FINALLY = Symbol("finally")
const SYM_SQUOTE  = Symbol("'")

const is_invalid_initial_token = let invalid = Set([')', ']', '}', SYM_ELSE, SYM_ELSEIF, SYM_CATCH, SYM_FINALLY])
    is_invalid_initial_token(t) = isa(t, CharSymbol) && t in invalid
    is_invalid_initial_token(t::AbstractToken) = is_invalid_initial_token(¬t)
end

const is_closing_token = let closing = Set([',', ')', ']', '}', ';', SYM_ELSE, SYM_ELSEIF, SYM_CATCH, SYM_FINALLY])
    is_closing_token(ps::ParseState, t) =
        ((¬t === SYM_END && !ps.end_symbol) || Lexer.eof(t) || (isa(¬t, CharSymbol) && ¬t in closing))
end

is_dict_literal(ex) = isexpr(¬ex, :(=>)) && length((¬ex).args) == 2
is_parameter(ex) = isexpr(¬ex, :parameters)

function parse_chain(ps::ParseState, ts::TokenStream, down::Function, op)
    chain = Any[down(ps, ts)]
    while true
        t = peek_token(ps, ts)
        ¬t !== op && return chain
        take_token(ts)
        if (ps.space_sensitive && ts.isspace &&
            (isa(¬t, Symbol) && ¬t in Lexer.UNARY_AND_BINARY_OPS) &&
            Lexer.peekchar(ts) != ' ')
            # here we have "x -y"
            put_back!(ts, t)
            return chain
        end
        push!(chain, down(ps, ts))
    end
end

# parse left to right chains of certain binary operator
# ex. a + b + c => Expr(:call, :+, a, b, c)
function parse_with_chains{T}(ps::ParseState, ts::TokenStream, down::Function, ops::Set{T}, chain_op)
    ex = down(ps, ts)
    while true
        t = peek_token(ps, ts)
        !(¬t in ops) && return ex
        take_token(ts)
        if (ps.space_sensitive && ts.isspace && (¬t in Lexer.UNARY_AND_BINARY_OPS) && Lexer.peekchar(ts) != ' ')
            # here we have "x -y"
            put_back!(ts, t)
            return ex
        elseif ¬t === chain_op
            ex = ⨳(:call, t, ex) ⪥ parse_chain(ps, ts, down, ¬t)
        else
            ex = ⨳(:call, t, ex, down(ps, ts))
        end
    end
end

function parse_LtoR{T}(ps::ParseState, ts::TokenStream, down::Function, ops::Set{T}, ex=down(ps, ts))
    while true
        t  = peek_token(ps, ts)
        !(¬t in ops) && return ex
        take_token(ts)
        if Lexer.is_syntactic_op(¬t) || ¬t === :(::) ||
                (VERSION < v"0.4.0-dev+573" && ¬t === :(in))
            ex = ⨳(t, ex, down(ps, ts))
        else
            ex = ⨳(:call, t, ex, down(ps, ts))
        end
        t = peek_token(ps, ts)
    end
end

function parse_RtoL{T}(ps::ParseState, ts::TokenStream, down::Function, ops::Set{T}, ex=down(ps, ts))
    while true
        t  = peek_token(ps, ts)
        !(¬t in ops) && return ex
        take_token(ts)
        if (ps.space_sensitive && ts.isspace &&
            (isa(¬t, Symbol) && ¬t in Lexer.UNARY_AND_BINARY_OPS) && Lexer.peekchar(ts) !== ' ')
            put_back!(ts, t)
            return ex
        elseif Lexer.is_syntactic_op(¬t)
            return ⨳(t, ex, parse_RtoL(ps, ts, down, ops))
        elseif ¬t === :(~)
            args = parse_chain(ps, ts, down, :(~))
            nt   = peek_token(ps, ts)
            if isa(¬nt, CharSymbol) && ¬nt in ops
                ex = ⨳(:macrocall, Symbol("@~"), ex)
                for i=1:length(args)-1
                    ex = ex ⪥ (args[i],)
                end
                ex = ex ⪥ (parse_RtoL(ps, ts, down, ops, args[end]),)
                return ex
            else
                ex = ⨳(:macrocall, Symbol("@~"), ex) ⪥ args
                return ex
            end
        else
            return ⨳(:call, t, ex, parse_RtoL(ps, ts, down, ops))
        end
    end
end

function parse_cond(ps::ParseState, ts::TokenStream)
    ex = (VERSION < v"0.4.0-dev+573" ? parse_or : parse_arrow)(ps, ts)
    if ¬peek_token(ps, ts) === :(?)
        t = take_token(ts)
        then = @without_range_colon ps begin
            parse_eqs(ps, ts)
        end
        nt = take_token(ts)
        if ¬nt !== :(:)
            D = diag(√nt, "colon expected in \"?\" expression")
            diag(D, √t, "\"?\" was here")
            throw(D)
        end
        return ⨳(:if, ex, then, parse_eqs(ps, ts))
    end
    return ex
end


function parse_Nary{T1, T2}(ps::ParseState, ts::TokenStream, down::Function, ops::Set{T1},
                            head::Symbol, closers::Set{T2}, allow_empty::Bool, add_linenums::Bool, opener = nothing)
    t = require_token(ps, ts)
    if is_invalid_initial_token(¬t)
        D = diag(√t, "unexpected \"$(¬t)\" in \"$head\" expression")
        diag(D, √opener, "expression started here")
        throw(D)
    end

    loc  = line_number_filename_node(ts) ⤄ Lexer.nullrange(ts)
    # empty block
    if isa(¬t, CharSymbol) && ¬t in closers
        return ⨳(head, add_linenums ? loc : nothing) ⤄ t
    end
    local args::Vector{Any}
    # in allow empty mode, skip leading runs of operator
    if allow_empty && isa(¬t, CharSymbol) && ¬t in ops
        args = Any[]
    else
        args = Any[down(ps, ts)]
        # line-number must happend before (down s)
    end
    add_linenums && unshift!(args, loc)
    isfirst = true
    t = peek_token(ps, ts)
    while true
        if !(¬t in ops)
            if !(Lexer.eof(t) || ¬t === '\n' || ',' in ops || ¬t in closers)
                throw(diag(√t,"extra token \"$(¬t)\" after end of expression"))
            end
            if isempty(args) || length(args) >= 2 || !isfirst
                # [] => Expr(:head)
                # [ex1, ex2] => Expr(head, ex1, ex2)
                # (ex1) if operator appeared => Expr(head,ex1) (handles "x;")
                ex = ⨳(head, args...) ⤄ t
                return ex
            else
                # [ex1] => ex1
                return args[1]
            end
        end
        isfirst = false
        take_token(ts)
        # allow input to end with the operator, as in a;b;
        nt = peek_token(ps, ts)
        if Lexer.eof(nt) || (isa(¬nt, CharSymbol) && ¬nt in closers) ||
           (allow_empty && isa(¬nt, CharSymbol) && ¬nt in ops) ||
           (length(ops) == 1 && ',' in ops && ¬nt === :(=))
           t = nt
           continue
        end
        add_linenums && !(length(args) >= 1 && isexpr(¬(args[end]), :line)) && push!(args, line_number_filename_node(ts))
        push!(args, down(ps, ts))
        t = peek_token(ps, ts)
    end
end

# the principal non-terminals follow, in increasing precedence order
const BLOCK_OPS = Set(['\n', ';'])
const BLOCK_CLOSERS = Set([SYM_END, SYM_ELSE, SYM_ELSEIF, SYM_CATCH, SYM_FINALLY])
function parse_block(ps::ParseState, ts::TokenStream, down = parse_eq)
    parse_Nary(ps, ts, down, BLOCK_OPS, :block, BLOCK_CLOSERS, true, true)
end

# for sequenced eval inside expressions, e.g. (a;b, c;d)
const WEXPR_OPS = Set([';'])
const WEXPR_CLOSERS = Set([',', ')'])
function parse_stmts_within_expr(ps::ParseState, ts::TokenStream)
    parse_Nary(ps, ts, parse_eqs, WEXPR_OPS, :block, WEXPR_CLOSERS, true, false)
end

#; at the top level produces a sequence of top level expressions
const NL_CLOSER = Set(['\n'])
function parse_stmts(ps::ParseState, ts::TokenStream)
    ex = parse_Nary(ps, ts, (ps, ts)->parse_docstring(ps, ts, parse_eq),
        WEXPR_OPS, :toplevel, NL_CLOSER, true, false)
    # check for unparsed junk after an expression
    t = peek_token(ps, ts)
    if !(Lexer.eof(t) || ¬t === '\n')
        throw(diag(√t,"extra token \"$(¬t)\" after end of expression"))
    end
    return ex
end

function parse_assignment(ps, ts, down, ex = down(ps, ts))
    t = peek_token(ps, ts)
    if ¬t in Lexer.ASSIGNMENT_OPS
        take_token(ts)
        if ¬t == :~
            if ps.space_sensitive && ts.isspace && Lexer.peekchar(ts) != ' '
                put_back!(ts, t)
                return ex
            else
                args = collect(children(parse_chain(ps, ts, down, :~)))
                ex = ⨳(:macrocall,Symbol("@~")⤄t,ex) ⪥ args[1:end-1]
                ex = ex ⪥ (parse_assignment(ps, ts, down, args[end]),)
            end
        else
            ex = ⨳(t, ex, parse_assignment(ps, ts, down))
        end
    end
    ex
end

function parse_eq(ps::ParseState, ts::TokenStream)
    lno = curline(ts)
    ex = parse_assignment(ps, ts, parse_comma)
    return short_form_function_loc(ts, ex, lno, filename(ts))
end

# parse-eqs is used where commas are special for example in an argument list
function parse_eqs(ps::ParseState, ts::TokenStream)
    lno = curline(ts)
    ex = parse_assignment(ps, ts, parse_cond)
    return short_form_function_loc(ts, ex, lno, filename(ts))
end

# parse-comma is neeed for commas outside parens, for example a = b, c
const EMPTY_SET = Set()
const COMMA_OPS = Set([','])
parse_comma(ps::ParseState, ts::TokenStream) = parse_Nary(ps, ts, parse_cond, COMMA_OPS, :tuple, EMPTY_SET, false, false)

const OR_OPS = Lexer.precedent_ops(:lazy_or)
function parse_or(ps::ParseState, ts::TokenStream)
    (VERSION < v"0.4.0-dev+573" ? parse_LtoR : parse_RtoL)(
        ps, ts, parse_and, OR_OPS)
end

const AND_OPS = Lexer.precedent_ops(:lazy_and)
function parse_and(ps::ParseState, ts::TokenStream)
    if VERSION < v"0.4.0-dev+573"
        parse_LtoR(ps, ts, parse_arrow, AND_OPS)
    else
        parse_RtoL(ps, ts, parse_comparison, AND_OPS)
    end
end

const ARROW_OPS = Lexer.precedent_ops(:arrow)
parse_arrow(ps::ParseState, ts::TokenStream) =
    parse_RtoL(ps, ts, VERSION < v"0.4.0-dev+573" ? parse_ineq : parse_or,
        ARROW_OPS)

const INEQ_OPS = Lexer.precedent_ops(:comparison)
parse_ineq(ps::ParseState, ts::TokenStream)  = parse_comparison(ps, ts, INEQ_OPS)

const PIPES_OPS = Lexer.precedent_ops(:pipe)
parse_pipes(ps::ParseState, ts::TokenStream) = parse_LtoR(ps, ts, parse_range, PIPES_OPS)

const IN_OPS = Set([:(in)])
parse_in(ps::ParseState, ts::TokenStream)    = parse_LtoR(ps, ts, parse_pipes, IN_OPS)

const EXPR_OPS = Lexer.precedent_ops(:plus)
parse_expr(ps::ParseState, ts::TokenStream)  = parse_with_chains(ps, ts, parse_shift, EXPR_OPS, :(+))

const SHIFT_OPS = Lexer.precedent_ops(:bitshift)
parse_shift(ps::ParseState, ts::TokenStream) = parse_LtoR(ps, ts, parse_term, SHIFT_OPS)

const TERM_OPS = Lexer.precedent_ops(:times)
parse_term(ps::ParseState, ts::TokenStream)  = parse_with_chains(ps, ts, parse_rational, TERM_OPS, :(*))

const RAT_OPS = Lexer.precedent_ops(:rational)
parse_rational(ps::ParseState, ts::TokenStream) = parse_LtoR(ps, ts, parse_unary, RAT_OPS)

function parse_comparison(ps::ParseState, ts::TokenStream, ops=INEQ_OPS)
    ex = VERSION < v"0.4.0-dev+573" ? parse_in(ps, ts) : parse_pipes(ps, ts)
    isfirst = true
    while true
        t = peek_token(ps, ts)
        if !(¬t in ops || ¬t === :in || (VERSION >= v"0.6.0-dev+1471" && ¬t === :isa))
            if VERSION > v"0.5.0-dev+3167" && !isfirst && length((¬ex).args) == 3
                args = collect(children(ex))
                return subtype_syntax(⨳(:call, args[2], args[1], args[3]))
            end
            return subtype_syntax(ex)
        end
        take_token(ts)
        if isfirst
            isfirst = false
            ex = ⨳(:comparison, ex, t, parse_range(ps, ts))
        else
            ex = ex ⪥ (t,parse_range(ps, ts))
        end
    end
end

is_large_number(n::BigInt) = true
is_large_number(n::Number) = false

const is_juxtaposed = let invalid_chars = Set{Char}(['(', '[', '{'])
    is_juxtaposed(ps::ParseState, ex, t) = begin
        return !(Lexer.is_operator(¬t)) &&
               !(Lexer.is_operator(¬ex)) &&
               !(¬t in Lexer.RESERVED_WORDS) &&
               !(is_closing_token(ps, t)) &&
               !(Lexer.isnewline(¬t)) &&
               !(isa(¬ex, Expr) && (¬ex).head === :(...)) &&
               ((isa(¬ex, Number) && !isa(¬ex, Char)) || !in(¬t, invalid_chars))
    end
end

#= This handles forms such as 2x => Expr(:call, :*, 2, :x) =#
function parse_juxtaposed(ps::ParseState, ts::TokenStream, ex)
    # numeric literal juxtaposition is a unary operator
    if is_juxtaposed(ps, ex, peek_token(ps, ts)) && !ts.isspace
        return ⨳(:call, :(*) ⤄ Lexer.nullrange(ts), ex, parse_unary(ps, ts))
    end
    return ex
end

function parse_range(ps::ParseState, ts::TokenStream)
    ex = parse_expr(ps, ts)
    isfirst = true
    while true
        t = peek_token(ps, ts)
        if isfirst && ¬t === :(..)
           take_token(ts)
           return ⨳(:call, t, ex, parse_expr(ps, ts))
        end
        if ps.range_colon_enabled && ¬t === :(:)
            take_token(ts)
            if ps.space_sensitive && ts.isspace
                peek_token(ps, ts)
                if !ts.isspace
                    # "a :b" in space sensitive mode
                    put_back!(ts, t)
                    return ex
                end
            end
            nt = peek_token(ps, ts)
            range_error(D) = (diag(D, √ex ⤄ √t, "start of range was here"); throw(D))
            if is_closing_token(ps, nt)
                # handles :(>:) case
                if isa(ex, Symbol) && Lexer.is_operator(¬ex)
                    op = Symbol(string(ex, t))
                    Lexer.is_operator(¬op) && return op
                end
                range_error(diag(√nt, "missing last argument in range expression"))
            end
            if Lexer.isnewline(¬nt)
                range_error(diag(√nt, "line break in \":\" expression"))
            end
            arg = parse_expr(ps, ts)
            if !ts.isspace && (¬arg === :(<) || ¬arg === :(>))
                loc = √arg ⤄ √t
                D = diag(loc,"Invalid \"$(¬arg)\" in range expression. Did you mean \"$(¬arg):\"?")
                diag(D, loc,"$(¬arg):",:fixit)
                # The user probably didn't intend a range, so don't add the
                # range note
                throw(D)
            end
            if isfirst
                ex = ⨳(t, ex, arg)
                isfirst = false
            else
                ex = ex ⪥ (arg,)
                isfirst = true
            end
            continue
        elseif ¬t === :(...)
            take_token(ts)
            return ⨳(t, ex)
        else
            return ex
        end
    end
end

function parse_decl(ps::ParseState, ts::TokenStream)
    ex = parse_call(ps, ts)
    while true
        nt = peek_token(ps, ts)
        # type assertion => x::Int
        if ¬nt === :(::)
            take_token(ts)
            ex = ⨳(:(::), ex, parse_call(ps, ts))
            continue
        end
        # anonymous function => (x) -> x + 1
        if ¬nt === :(->)
            take_token(ts)
            # -> is unusual it binds tightly on the left and loosely on the right
            lno = line_number_filename_node(ts) ⤄ Lexer.nullrange(ts)
            eqs = parse_eqs(ps, ts)
            return ⨳(:(->), ex, ⨳(:block, lno, eqs))
        end
        return ex
    end
end

# handle ^ and .^
function parse_factorh(ps::ParseState, ts::TokenStream, down::Function, ops)
    ex = down(ps, ts)
    nt = peek_token(ps, ts)
    !(¬nt in ops) && return ex
    take_token(ts)
    pf = parse_factorh(ps, ts, parse_unary, ops)
    return ⨳(:call, nt, ex, pf)
end

function negate(n)
    if isa(¬n, Number)
        if isa(¬n, Int64) && ¬n == -9223372036854775808
            # promote to Int128
            return typeof(n)(9223372036854775808 ⤄ n)
        end
        if isa(¬n, Int128) && ¬n == -170141183460469231731687303715884105728
            # promote to BigInt
            return typeof(n)(Base.parse(BigInt, "170141183460469231731687303715884105728") ⤄ n)
        end
        return typeof(n)(-(¬n)) ⤄ n
    elseif isa(¬n, Expr)
        return (¬n).head === :(-) && length(n.args) == 1 ? (¬n).args[1] : ⨳(:-, n)
    end
    throw(ArgumentError("negate argument is not a Number or Expr"))
end

# -2^3 is parsed as -(2^3) so call parse-decl for the first arg,
# and parse unary from then on (handles 2^-3)
const FAC_OPS = Lexer.precedent_ops(13)
parse_factor(ps::ParseState, ts::TokenStream) = parse_factorh(ps, ts, parse_decl, FAC_OPS)

function parse_unary(ps::ParseState, ts::TokenStream)
    t = require_token(ps, ts)
    if is_closing_token(ps, t)
        D = diag(√t, "unexpected \"$(¬t)\"")
        throw(D)
    end
    if !(isa(¬t, Symbol) && ¬t in Lexer.UNARY_OPS)
        pf = parse_factor(ps, ts)
        return parse_juxtaposed(ps, ts, pf)
    end
    op = take_token(ts)
    nc = Lexer.peekchar(ts)
    if (¬op === :(-) || ¬op === :(+)) && (isdigit(nc) || nc === '.')
        neg = ¬op === :(-)
        leadingdot = nc === '.'
        leadingdot && Lexer.readchar(ts)
        n   = Lexer.read_number(ts, leadingdot, neg)
        num = parse_juxtaposed(ps, ts, n) ⤄ op
        nt  = peek_token(ps, ts)
        if ¬nt === :(^) || ¬nt === :(.^)
            # -2^x parsed as (- (^ 2 x))
            put_back!(ts, neg ? negate(num) : num)
            return ⨳(:call, op, parse_factor(ps, ts))
        end
        return num
    end
    nt = peek_token(ps, ts)
    if is_closing_token(ps, nt) || Lexer.isnewline(¬nt) || (¬nt == :(=))
        # return operator by itself, as in (+)
        return op
    elseif ¬nt === '{'
        # this case is +{T}(x::T)
        put_back!(ts, op)
        return parse_factor(ps, ts)
    else
        arg = parse_unary(ps, ts)
        if isexpr(¬arg, :tuple)
            ex = ⨳(:call, op) ⪥ arg
            return ex
        end
        return ⨳(:call, op, arg)
    end
end

subtype_symbol(sym) = (sym == :(<:) || sym == :(>:))
function subtype_syntax(ex)
    if isa(¬ex, Expr) && length((¬ex).args) == 3 &&
            (((¬ex).head === :comparison && subtype_symbol((¬ex).args[2])) ||
             ((¬ex).head === :call && subtype_symbol((¬ex).args[1])))
        args = collect(children(ex))
        return ⨳((¬ex).head == :call ? (¬ex).args[1] : (¬ex).args[2],
            (¬ex).head == :call ? args[2] : args[1], args[3]) ⤄ ex
    end
    return ex
end

function parse_unary_prefix(ps::ParseState, ts::TokenStream)
    op = peek_token(ps, ts)
    if isa(¬op, Symbol) && Lexer.is_syntactic_unary_op(¬op)
        take_token(ts)
        next = peek_token(ps, ts)
        if is_closing_token(ps, next) || Lexer.isnewline(next)
            return op
        elseif ¬op === :(&) || ¬op === :(::)
            return ⨳(op, parse_call(ps, ts))
        else
            return ⨳(op, parse_atom(ps, ts))
        end
    end
    return parse_atom(ps, ts)
end

# parse function all, indexing, dot, and transpose expressions
# also handles looking for reserved words
function parse_call(ps::ParseState, ts::TokenStream)
    ex = parse_unary_prefix(ps, ts)
    if isa(¬ex, Symbol) && ¬ex in Lexer.RESERVED_WORDS
        return parse_resword(ps, ts, ex)
    end
    return parse_call_chain(ps, ts, ex, false)
end

function separate(f::Function, collection)
    tcoll, fcoll = Any[], Any[]
    for c in collection
        f(c) ? push!(tcoll, c) : push!(fcoll, c)
    end
    return (tcoll, fcoll)
end

const BEGIN_CHARS = Set(['(', '[','{', '"'])

function parse_call_chain(ps::ParseState, ts::TokenStream, ex, one_call::Bool)
    while true
        t = peek_token(ps, ts)
        if (ps.space_sensitive && ts.isspace &&
           (¬t === SYM_SQUOTE || ¬t in BEGIN_CHARS) ||
           (isa(¬ex, Number) && ¬t === '('))
            return ex
        end
        if ¬t === '('
            take_token(ts)
            arglist = parse_arglist(ps, ts, ')', t)
            closer = take_token(ts)
            params, args = separate(is_parameter, arglist)
            nt = peek_token(ps, ts)
            if ¬nt === SYM_DO
                take_token(ts)
                ex = ⨳(:call, ex) ⪥ params
                ex ⪥ (parse_do(ps,ts,nt),)
                ex ⪥ args
            else
                ex = ⨳(:call, ex) ⪥ arglist
            end
            ex = ex ⤄ closer
            one_call && return ex
            continue

        elseif ¬t === '['
            take_token(ts)
            # ref is syntax so can distinguish a[i] = x from ref(a, i) = x
            al = @with_end_symbol ps begin
                parse_cat(ps, ts, ']', t, is_dict_literal(ex))
            end
            if isempty((¬al).args) && ((¬al).head === :cell1d || (¬al).head === :vcat)
                ex = is_dict_literal(ex) ? ⨳(:typed_dict, ex) : ⨳(:ref, ex)
                continue
            end
            if (¬al).head === :dict
                ex = ⨳(is_dict_literal(ex) ? :typed_dict : :ref, ex) ⪥ al
            elseif (¬al).head === :hcat
                ex = ⨳(:typed_hcat, ex) ⪥ al
            elseif (¬al).head === :vect
                @assert VERSION >= v"0.4"
                ex = ⨳(:ref, ex) ⪥ al
            elseif (¬al).head === :vcat && VERSION < v"0.4"
                ex = ⨳(:ref, ex)
                for arg in (¬al).args
                    if isa(¬arg, Expr) && arg.head === :row
                        ex.head = :typed_vcat
                    end
                    push!((¬ex).args, arg)
                end
            elseif (¬al).head === :vcat
                ex = ⨳(:typed_vcat, ex) ⪥ al
            elseif (¬al).head === :comprehension
                ex = ⨳(:typed_comprehension, ex) ⪥ al
            elseif (¬al).head === :dict_comprehension
                ex = ⨳(:typed_dict_comprehension, ex) ⪥ al
            else
                error("unknown parse-cat result (internal error)")
            end
            continue

        elseif ¬t === :(.)
            take_token(ts)
            nt = peek_token(ps, ts)
            if ¬nt === '('
                ex = ⨳(:(.), ex, parse_atom(ps, ts))
            elseif ¬nt === :($)
                dollar_ex = parse_unary(ps, ts)
                inert   = ⨳(:inert, ⨳(nt) ⪥ dollar_ex)
                ex = ⨳(:(.), ex, inert)
            else
                name = parse_atom(ps, ts)
                if isexpr(¬name, :macrocall)
                    args = collect(children(name))
                    ex = ⨳(:macrocall, ⨳(:(.), ex, ⨳(:quote, args[1]))) ⪥ args[2:end]
                else
                    ex = ⨳(:(.), ex, QuoteNode(¬name) ⤄ name)
                end
            end
            continue
        elseif ¬t === :(.') || ¬t === SYM_SQUOTE # '
            take_token(ts)
            ex = ⨳(t, ex)
            continue

        elseif ¬t === '{'
            take_token(ts)
            args = map(subtype_syntax, parse_arglist(ps, ts, '}', t))
            # ::Type{T}
            ex = ⨳(:curly, ex) ⪥ args
            ex = ex ⤄ take_token(ts)
            continue

        elseif ¬t === '"'
            if isa(¬ex, Symbol) && !Lexer.is_operator(¬ex) && !ts.isspace
                # custom prefexed string literals x"s" => @x_str "s"
                take_token(ts)
                str = parse_string_literal(ps, ts, true)
                nt  = peek_token(ps, ts)
                if VERSION < v"0.4"
                    suffix  = triplequote_string_literal(str) ? "_mstr" : "_str"
                    macname = Symbol(string('@', ¬ex, suffix))
                    macstr = (¬str).args[1]
                else
                    macname = Symbol(string('@',¬ex,"_str"))
                    macstr = first(children(str))
                end
                if isa(¬nt, Symbol) && !Lexer.is_operator(¬nt) && !ts.isspace
                    # string literal suffix "s"x
                    t = take_token(ts)
                    ex = ⨳(:macrocall, macname, macstr, string(¬t) ⤄ t)
                else
                    ex = ⨳(:macrocall, macname, macstr)
                end
                continue
            end
            return ex
        end
        return ex
    end
end

const expect_end_current_line = 0

function expect_end(ps::ParseState, ts::TokenStream, word)
    t = peek_token(ps, ts)
    if ¬t === SYM_END
        take_token(ts)
    elseif Lexer.eof(t)
        D = Incomplete(:block, diag(here(ts),"incomplete: \"$(¬word)\" requires end"))
        diag(D,√word,"\"$(¬word)\" began here")
        throw(D)
    else
        D = diag(√t,"\"$(¬word)\" requires \"end\", got \"$(¬t)\"")
        diag(D,√word,"\"$(¬word)\" began here")
        throw(D)
    end
end

parse_subtype_spec(ps::ParseState, ts::TokenStream) = subtype_syntax(parse_ineq(ps, ts))


#TODO: remove this after the 0.4 dev period
@eval function tryexpr(ts, tryb, catchv, catchb, finalb)
    r = Lexer.nullrange(ts)
    if finalb == nothing
        return catchb != nothing ? ⨳(:try, tryb, catchv, catchb) :
                                   $(VERSION > v"0.4.0-dev" ?
                                        :(⨳(:try, tryb, false ⤄ r, ⨳(:block) ⤄ r)) :
                                        :(⨳(:try, tryb, false ⤄ r, false ⤄ r)))
    else
        return catchb != nothing ? ⨳(:try, tryb, catchv, catchb, finalb) :
                                   $(VERSION > v"0.4.0-dev" ?
                                        :(⨳(:try, tryb, false ⤄ r, false ⤄ r, finalb)) :
                                        :(⨳(:try, tryb, false ⤄ r, false ⤄ r, finalb)))
    end
end

const _QuoteNode = QuoteNode

is_symbol_or_interpolate(ex) = isa(¬ex, Symbol) || isexpr(¬ex, :$)

# parse expressions or blocks introduced by syntatic reserved words
function parse_resword(ps::ParseState, ts::TokenStream, word, chain = nothing)
    expect_end_current_line = curline(ts)
    @with_normal_ops ps begin
        @without_whitespace_newline ps begin
            if ¬word === :quote || ¬word === :begin
                Lexer.skipws_and_comments(ts)
                loc = line_number_filename_node(ts)
                blk = parse_block(ps, ts)
                expect_end(ps, ts, word)
                ex = blk
                if !isempty((¬blk).args)
                    arg1 = collect(children(blk))[1]
                    if ((isa(¬arg1, Expr) && (¬arg1).head === :line) ||
                            (isa(¬arg1, LineNumberNode)))
                        ex = ⨳(:block, loc ⤄ Lexer.nullrange(ts))
                        ex = ex ⪥ collect(children(blk))[2:end]
                    end
                else
                    ex = blk
                end
                return ¬word === :quote ? ⨳(word, ex) : ex

            elseif ¬word === :while
                ex = ⨳(word, parse_cond(ps, ts), parse_block(ps, ts))
                expect_end(ps, ts, word)
                return ex

            elseif ¬word === :for
                ranges  = parse_comma_sep_iters(ps, ts, word)
                nranges = length(ranges)
                body = parse_block(ps, ts)
                expect_end(ps, ts, word)
                if nranges == 1
                    return ⨳(word, ranges[1], body)
                else
                    blk = ⨳(:block, ranges...)
                    return ⨳(word, blk, body)
                end

            elseif ¬word === :if || ¬word == :elseif
                if chain == nothing && ¬word == :elseif
                    throw(diag(√word,"\"elseif\" without preceeding if"))
                end
                if Lexer.isnewline(¬peek_token(ps, ts))
                    D = diag(√word, "missing condition in \"$(¬word)\"")
                    diag(D, √chain, "previous \"$(¬chain)\" was here")
                    throw(D)
                end
                test = parse_cond(ps, ts)
                t    = require_token(ps, ts)
                then = (¬t === SYM_ELSE || ¬t === SYM_ELSEIF) ? ⨳(:block) ⤄ Lexer.nullrange(ts) :
                                                              parse_block(ps, ts)
                nxt = require_token(ps, ts)
                take_token(ts)
                if ¬nxt === SYM_END
                    return ⨳(:if, test, then) ⤄ word
                elseif ¬nxt === SYM_ELSEIF
                    blk = ⨳(:block, line_number_filename_node(ts), parse_resword(ps, ts, nxt, word))
                    return ⨳(:if, test, then, blk) ⤄ word
                elseif ¬nxt === SYM_ELSE
                    nnxt = peek_token(ps, ts)
                    if ¬nnxt === :if
                        loc = √nxt ⤄ √nnxt
                        D = diag(loc, "use \"elseif\" instead of \"else if\"")
                        diag(D, loc, "elseif", :fixit)
                        throw(D)
                    end
                    blk = parse_block(ps, ts)
                    ex = ⨳(:if, test, then, blk) ⤄ word
                    expect_end(ps, ts, word)
                    return ex
                else
                    D = diag(√nxt,"Unexpected \"$(¬nxt)\" in if expression")
                    diag(D, √word, "previous \"$(¬word)\" was here")
                    throw(D)
                end

            elseif ¬word === :let
                nt = peek_token(ps, ts)
                lno = curline(ts)
                binds = Lexer.isnewline(¬nt) || ¬nt === ';' ? Any[] : parse_comma_sep_assigns(ps, ts)
                nt = peek_token(ps, ts)
                if !(Lexer.eof(nt) || (isa(¬nt, CharSymbol) && (¬nt === '\n' || ¬nt ===  ';' || ¬nt === SYM_END)))
                    D = diag(√nt, "let variables should end in \";\" or newline")
                    diag(D, √word, "\"let\" was here")
                    throw(D)
                end
                ex = parse_block(ps, ts)
                expect_end(ps, ts, word)
                # Don't need line number node in empty let blocks
                length((¬ex).args) == 1 && (ex = ⨳(:block) ⤄ ex)
                ex = ⨳(:let, ex) ⪥ binds
                return ex

            elseif ¬word === :global || ¬word === :local
                lno = curline(ts)
                isconst = ¬peek_token(ps, ts) === :const ? (take_token(ts); true) : false
                args = parse_comma_sep_assigns(ps, ts)
                if isconst
                    ex = ⨳(word, args...)
                    return ⨳(:const, ex)
                else
                    ex = ⨳(word, args...)
                    return ex
                end

            elseif ¬word === :function || ¬word === :macro || ¬word === :stagedfunction
                paren = ¬require_token(ps, ts) === '('
                sig   = parse_call(ps, ts)
                local def
                if is_symbol_or_interpolate(sig) ||
                    (isa(¬sig, Expr) && (¬sig).head === :(::) && isa((¬sig).args[1], Symbol))
                   if paren
                        # in function(x) the (x) is a tuple
                        def = ⨳(:tuple, sig)
                    else
                        ¬peek_token(ps, ts) !== SYM_END && Lexer.skipws_and_comments(ts)
                        require_token(ps, ts)
                        expect_end(ps, ts, word)
                        return ⨳(word, sig)
                    end
                else
                    if (isa(¬sig, Expr) && ((¬sig).head === :call || (¬sig).head === :tuple))
                        def = sig
                    else
                        D = diag(after(√sig), "expected \"(\" in $(¬word) definition")
                        diag(D, √word, "\"$(¬word)\" was here")
                        throw(D)
                    end
                end
                ¬peek_token(ps, ts) !== SYM_END && Lexer.skipws_and_comments(ts)
                loc  = line_number_filename_node(ts)
                body = parse_block(ps, ts)
                expect_end(ps, ts, word)
                add_filename_to_block!(body, loc)
                ex = ⨳(word, def, body)
                return ex

            elseif ¬word === :abstract
                return ⨳(:abstract, parse_subtype_spec(ps, ts))

            elseif ¬word === :type || ¬word === :immutable
                istype = ¬word === :type
                if VERSION < v"0.4"
                    # allow "immutable type"
                    (!istype && ¬peek_token(ps, ts) === :type) && take_token(ts)
                end
                nxt = peek_token(ps, ts)
                if ¬nxt in Lexer.RESERVED_WORDS
                    D = diag(√nxt, "invalid type name \"$(¬nxt)\"")
                    diag(D, √word, "$(¬word) was here")
                    throw(D)
                end
                sig = parse_subtype_spec(ps, ts)
                blk = parse_block(ps, ts)
                ex  = ⨳(:type, istype ⤄ word, sig, blk)
                expect_end(ps, ts, word)
                return ex

            elseif ¬word === :bitstype
                stmnt = @space_sensitive ps begin
                    parse_cond(ps, ts)
                end
                return ⨳(:bitstype, stmnt, parse_subtype_spec(ps, ts))

            elseif ¬word === :typealias
                lhs = parse_call(ps, ts)
                return ⨳(:typealias, lhs, parse_arrow(ps, ts))

            elseif ¬word === :try
                t = require_token(ps, ts)
                tryb = ¬t === SYM_CATCH || ¬t === SYM_FINALLY ? ⨳(:block) : parse_block(ps, ts)
                tryb = (tryb ⤄ word) ⤄ t
                t = require_token(ps, ts)
                catchb = nothing
                catchv = false
                finalb = nothing
                while true
                    take_token(ts)
                    if ¬t === SYM_END
                        return tryexpr(ts, tryb, catchv, catchb, finalb)
                    end
                    if ¬t === SYM_CATCH && catchb == nothing
                        nb = false # do we delineate a new block after a catch token (with ; or \n)?
                        nt = peek_token(ps, ts)
                        if Lexer.isnewline(¬nt) || ¬nt === ';'
                            nb = true
                            ¬nt === ';' && take_token(ts)
                        end
                        nt = require_token(ps, ts)
                        if ¬nt === SYM_END || ¬nt === SYM_FINALLY
                            catchb = ⨳(:block) ⤄ t
                            catchv = false
                            t = nt
                            continue
                        else
                            loc = line_number_filename_node(ts)
                            var = parse_eqs(ps, ts)
                            isvar = nb == false && (isa(¬var, Symbol) || (isa(¬var, Expr) && (¬var).head == :($)))
                            et = ¬require_token(ps, ts)
                            catch_block = et === SYM_FINALLY || et === SYM_END ? ⨳(:block, loc) :
                                                                                  parse_block(ps, ts)
                            catch_block = catch_block ⤄ nt
                            t = require_token(ps, ts)
                            if isvar
                                catchb = catch_block
                            else
                                exb = ⨳(:block, loc, var)
                                if length(catch_block.args) == 1
                                    arg1 = catch_block.args[1]
                                    if (isa(¬arg1, Expr) && (¬arg1).head === :line) ||
                                       (isa(¬arg1, LineNumberNode))
                                       catchb = exb
                                    else
                                        catchb = exb ⪥ catch_block
                                    end
                                else
                                    catchb = exb ⪥ catch_block
                                end
                            end
                            catchv = isvar ? var : false
                            continue
                        end
                    elseif ¬t === SYM_FINALLY && finalb == nothing
                        finalb = require_token(ps, ts) === SYM_CATCH ? Expr(:block) :
                                                                       parse_block(ps, ts)
                        t = require_token(ps, ts)
                        continue
                    else
                        D = diag(√t,"Unexpected \"$(¬t)\" in try expression")
                        diag(D, √word, "\"$(¬word)\" was here")
                        throw(D)
                    end
                end

            elseif ¬word === :return
                t  = peek_token(ps, ts)
                return Lexer.isnewline(¬t) || is_closing_token(ps, t) ? ⨳(word, nothing ⤄ Lexer.nullrange(ts)) :
                                                                       ⨳(word, parse_eq(ps, ts))
            elseif ¬word === :break || ¬word === :continue
                return ⨳(word)

            elseif ¬word === :const
                assgn = parse_eq(ps, ts)
                if !(isa(¬assgn, Expr) && ((¬assgn).head === :(=) ||
                                          (¬assgn).head === :global ||
                                          (¬assgn).head === :local))
                    D = diag(√assgn, "expected assignment after \"$(¬word)\"")
                    diag(D, √word, "\"$(¬word)\" was here")
                    throw(D)
                end
                return ⨳(:const, assgn)

            elseif ¬word === :module || ¬word === :baremodule
                isbare = ¬word === :baremodule
                location = line_number_filename_node(ts)
                name = parse_unary_prefix(ps, ts)
                body = parse_block(ps, ts, (ps, ts)->parse_docstring(ps, ts, parse_eq))
                expect_end(ps, ts, word)
                return ⨳(:module, !isbare, name, ⨳(:block, location) ⪥ body)

            elseif ¬word === :export
                exports = map(macrocall_to_atsym, parse_comma_sep(ps, ts, parse_unary_prefix))
                for x in exports
                    if !is_symbol_or_interpolate(x)
                        D = diag(√x, "invalid literal \"$(¬x)\" in \"$(¬word)\" statement")
                        diag(D, √word, "\"$(¬word)\" was here")
                        throw(D)
                    end
                end
                ex = ⨳(word) ⪥ exports
                return ex

            elseif ¬word === :import || ¬word === :using || ¬word === :importall
                imports = parse_imports(ps, ts, word)
                length(imports) == 1 && return imports[1]
                ex = (⨳(:toplevel) ⤄ Lexer.nullrange(ts)) ⪥ imports
                return ex

            elseif ¬word === :ccall
                nt = peek_token(ps, ts)
                if ¬nt != '('
                    throw(diag(√nt,"Expected '('"))
                end
                take_token(ts)
                al = parse_arglist(ps, ts, ')', nt)
                if length(al) > 1
                    al1, al2 = al[1], al[2]
                    if ¬al2 === :cdecl || ¬al2 === :stdcall || ¬al2 == :fastcall || ¬al2 == :thiscall
                        # place calling convention at end of arglist
                        ex = ⨳(:ccall, al1)
                        for i = 3:length(al)
                            ex = ex ⪥ (al[i],)
                        end
                        ex = ex ⪥ (⨳(al2),)
                        ex = ex ⤄ take_token(ts)
                        return ex
                    end
                end
                ex = ⨳(word, al...)
                ex = ex ⤄ take_token(ts)
                return ex

            else
                @assert ¬word === :do
                throw(diag(√word,"invalid \"$(¬word)\" syntax"))
            end
        end
    end
end

function add_filename_to_block!(body, loc)
    if !isempty((¬body).args)
        if isa((¬body).args[1], Expr) && (¬body).args[1].head === :line
            (¬body).args[1] = loc
        elseif isa((¬body).args[1], LineNumberNode)
            (¬body).args[1] = loc
        end
    end
    return body
end

function parse_do(ps::ParseState, ts::TokenStream, word)
    expect_end_current_line = curline(ts)
    @without_whitespace_newline ps begin
        t = peek_token(ps, ts)
        doargs = (Lexer.isnewline(¬t) || ¬t == ';') ? Any[] : parse_comma_sep(ps, ts, parse_range)
        loc = line_number_filename_node(ts)
        blk = parse_block(ps, ts)
        add_filename_to_block!(blk, loc)
        expect_end(ps, ts, word)
        return ⨳(:(->), ⨳(:tuple, doargs...) ⤄ Lexer.nullrange(ts), blk)
    end
end

macrocall_to_atsym(ex) = isa(¬ex, Expr) && (¬ex).head === :macrocall ? collect(children(ex))[1] : ex

function parse_imports(ps::ParseState, ts::TokenStream, word)
    frst = Any[parse_import(ps, ts, word)]
    nt   = peek_token(ps, ts)
    from = ¬nt === :(:) && !ts.isspace
    done = false
    if from || ¬nt === ','
        take_token(ts)
        done = false
    elseif ¬nt === '.'
        done = false
    else
        done = true
    end
    rest = done? Any[] : parse_comma_sep(ps, ts, (ps, ts) -> parse_import(ps, ts, word))
    if from
        module_syms = frst[1]
        imports = Any[]
        for expr in rest
            ex = (⨳((¬expr).head) ⪥ module_syms) ⪥ expr
            push!(imports, ex)
        end
        return imports
    end
    return append!(frst, rest)
end

const sym_1dot  = Symbol(".")
const sym_2dots = Symbol("..")
const sym_3dots = Symbol("...")
const sym_4dots = Symbol("....")

function parse_import_dots(ps::ParseState, ts::TokenStream)
    l = Any[]
    t = peek_token(ps, ts)
    while true
        if ¬t === sym_1dot
            take_token(ts)
            push!(l, :(.))
            t = peek_token(ps, ts)
            continue
        elseif ¬t === sym_2dots
            take_token(ts)
            append!(l, [:(.), :(.)])
            t = peek_token(ps, ts)
            continue
        elseif ¬t === sym_3dots
            take_token(ts)
            append!(l, [:(.), :(.), :(.)])
            t = peek_token(ps, ts)
            continue
        elseif ¬t === sym_4dots
            take_token(ts)
            append!(l, [:(.), :(.), :(.), :(.)])
            t = peek_token(ps, ts)
            continue
        end
        return push!(l, macrocall_to_atsym(parse_unary_prefix(ps, ts)))
    end
end

function parse_import(ps::ParseState, ts::TokenStream, word)
    path = parse_import_dots(ps, ts)
    while true
        # this handles cases such as Base.* where .* is a valid operator token
        nc = Lexer.peekchar(ts)
        if nc === '.'
            Lexer.takechar(ts)
            push!(path, macrocall_to_atsym(parse_unary_prefix(ps, ts)))
        else
            ex = ⨳(word) ⪥ path
            return ex
        end
    end
end

function parse_comma_sep(ps::ParseState, ts::TokenStream, what::Function)
    exprs = Any[]
    while true
        r = what(ps, ts)
        if ¬peek_token(ps, ts) === ','
            take_token(ts)
            push!(exprs, r)
            continue
        end
        push!(exprs, r)
        return exprs
    end
end

parse_comma_sep_assigns(ps::ParseState, ts::TokenStream) = parse_comma_sep(ps, ts, parse_eqs)

# as above, but allows both "i=r" and "i in r"
# return a list of range expressions
function parse_comma_sep_iters(ps::ParseState, ts::TokenStream, word)
    ranges = Any[]
    while true
        r = parse_iteration_spec(ps, ts, word)
        push!(ranges, r)
        if ¬peek_token(ps, ts) === ','
            take_token(ts)
            continue
        end
        return ranges
    end
end

function parse_iteration_spec(ps, ts, word)
    lhs = parse_pipes(ps, ts)
    t = peek_token(ps, ts)
    is_in_symbol(sym) = ¬sym in (:in, :∈, :(=))
    if is_in_symbol(t)
        take_token(ts)
        rhs = parse_pipes(ps, ts)
        t = peek_token(ps, ts)
        if !(is_closing_token(ps, t) || Lexer.isnewline(t))
            # TODO: Syntax deprecation here
            #=D = diag(√t, "invalid iteration spec")
            diag(D, √word, "for this `for`")
            throw(D)=#
        end
        return ⨳(:(=), lhs, rhs)
    elseif ¬lhs == :(:) && is_closing_token(ps, t)
        return lhs
    else
        D = diag(√lhs, "invalid iteration spec")
        diag(D, √word, "for this `for`")
        throw(D)
    end
end

function parse_space_separated_exprs(ps::ParseState, ts::TokenStream)
    @space_sensitive ps begin
        exprs = Any[]
        while true
            nt = peek_token(ps, ts)
            if is_closing_token(ps, nt) ||
               Lexer.isnewline(¬nt) ||
               (ps.inside_vector && ¬nt === :for)
                return exprs
            end
            ex = parse_eq(ps, ts)
            if Lexer.isnewline(¬peek_token(ps, ts))
                push!(exprs, ex)
                return exprs
            end
            push!(exprs, ex)
        end
    end
end

function to_kws(lst)
    n = length(lst)
    kwargs = Array{Any}(n)
    for i = 1:n
        ex = lst[i]
        if isa(¬ex, Expr) && (¬ex).head === :(=) && length((¬ex).args) == 2
            nex = ⨳(:kw) ⪥ ex
            kwargs[i] = nex
        else
            kwargs[i] = ex
        end
    end
    return kwargs
end

# handle function call argument list, or any comma-delimited list
# * an extra comma at the end is allowed
# * expressions after a ; are enclosed in (parameters ....)
# * an expression followed by ... becomes (.... x)
#
# Note: We do not take the closer. The caller is responsible for taking it
# (e.g. for range extension)
function _parse_arglist(ps::ParseState, ts::TokenStream, closer, opener)
    lst = Any[]
    loc = here(ts)
    while true
        t = require_token(ps, ts)
        if ¬t === closer
            # take_token(ts) - Do not take closer
            # x=y inside a function call is a keyword argument
            return closer === ')' ? to_kws(lst) : lst
        elseif ¬t === ';'
            take_token(ts)
            # allow f(a, b; )
            ¬peek_token(ps, ts) === closer && continue
            params = parse_arglist(ps, ts, closer)
            lst = closer === ')' ? to_kws(lst) : lst
            return unshift!(lst, ⨳(:parameters, params...))
        end
        nxt = try
            parse_eqs(ps, ts)
        catch D
            isa(D, Diagnostic) || rethrow(D)
            diag(D, √opener, "in argument list beginning here")
            throw(D)
        end
        loc = here(ts)
        nt  = require_token(ps, ts)
        if ¬nt === ','
            take_token(ts)
            push!(lst, nxt)
            continue
        elseif ¬nt === ';'
            push!(lst, nxt)
            continue
        elseif ¬nt === closer
            push!(lst, nxt)
            continue
        elseif ¬nt === :for
            take_token(ts)
            push!(lst, parse_generator(ps, ts, nxt, nt))
            continue
        else
            D = diag(loc,"Expected '$(¬closer)' or ','")
            diag(D, √opener, "to match '$(¬opener)' here", :note)
            throw(D)
        end
    end
end

function parse_arglist(ps::ParseState, ts::TokenStream, closer, opener = nothing)
    @with_normal_ops ps begin
        @with_whitespace_newline ps begin
            return _parse_arglist(ps, ts, closer, opener)
        end
    end
end

# parse [] concatenation exprs and {} cell exprs
function parse_vect(ps::ParseState, ts::TokenStream, frst, closer, opener)
    lst = Any[]
    nxt = frst
    head = VERSION < v"0.4" ? :vcat : :vect
    while true
        t = require_token(ps, ts)
        if ¬t === closer
            take_token(ts)
            ex = ⨳(head,push!(lst,nxt)...)
            return ex
        end
        take_token(ts)
        if ¬t === ','
            if ¬require_token(ps, ts) === closer
                # allow ending with ,
                take_token(ts)
                push!(lst, nxt)
                ex = ⨳(VERSION < v"0.4" ? :vcat : :vect) ⪥ lst
                return ex
            end
            lst = push!(lst, nxt)
            nxt = parse_eqs(ps, ts)
            continue
        elseif ¬t === ';'
            head = :vcat
            ¬require_token(ps, ts) === closer && continue
            return ((⨳(:vcat) ⪥ parse_arglist(ps, ts, closer, opener)) ⪥ reverse!(lst)) ⪥
                (nxt,)
        elseif ¬t === ']' || ¬t === '}'
            D = diag(√t, "Expected \"$closer\", got \"$(¬t)\"")
            diag(D, √opener, "Expression began here")
            throw(D)
        else
            D = diag(√t,"missing separator in array expression")
            diag(D, √opener, "Expression began here")
            throw(D)
        end
    end
end


function parse_dict(ps::ParseState, ts::TokenStream, frst, closer, opener)
    v = parse_vect(ps, ts, frst, closer, opener)
    for arg in collect(children(v))
        alldl = is_dict_literal(¬arg)
        alldl || throw(diag(√arg,"invalid dict literal"))
    end
    return ⨳(:dict) ⪥ v
end

function parse_comprehension(ps::ParseState, ts::TokenStream, frst, closer, opener, word)
    if VERSION >= v"0.5-"
        gen = parse_generator(ps, ts, frst, closer)
    else
        itrs = parse_comma_sep_iters(ps, ts, word)
    end
    t = require_token(ps, ts)
    if ¬t !== closer
        D = diag(√t,"expected '$closer' not \"$(¬t)\"")
        diag(D, √opener, "in comprehension that began here")
        throw(D)
    end
    take_token(ts)
    if VERSION >= v"0.5-"
        ex = ⨳(:comprehension, gen)
    else
        ex = ⨳(:comprehension, frst) ⪥ itrs
    end
    return ex
end

function parse_dict_comprehension(ps::ParseState, ts::TokenStream, frst, closer, opener, word)
    c = parse_comprehension(ps, ts, frst, closer, opener, word)
    if is_dict_literal((¬c).args[1])
        ex = ⨳(:dict_comprehension) ⪥ c
        return ex
    else
        throw(diag(√(first(children(args))),"non-dict-literal in dict comprehension"))
    end
end

function parse_generator(ps, ts, first, word)
    ⨳(:generator, first) ⪥ parse_comma_sep_iters(ps, ts, word)
end

function parse_matrix(ps::ParseState, ts::TokenStream, frst, closer, gotnewline, opener)

    function update_outer!(v, outer)
        len = length(v)
        len == 0 && return outer
        len == 1 && return push!(outer, v[1])
        row = ⨳(:row,v...)
        return push!(outer, row)
    end

    function report_error(D)
        diag(D, √opener, "In matrix expression that began here")
        throw(D)
    end

    semicolon = ¬peek_token(ps, ts) === ';'
    vec   = Any[frst]
    outer = Any[]
    while true
        t = (¬peek_token(ps, ts) === '\n' || gotnewline) ? '\n' : require_token(ps, ts)
        if ¬t === closer
            take_token(ts)
            local ex
            if !isempty(outer)
                ex = ⨳(:vcat,update_outer!(vec, outer)...)
            elseif length(vec) <= 1
                # [x] => (vect x)
                ex = ⨳(VERSION < v"0.4" ? :vcat : :vect, vec...) ⤄ t
            else
                # [x y] => (hcat x y)
                ex = ⨳(:hcat, vec...)
            end
            return ex
        end
        if ¬t === ';' || ¬t === '\n'
            gotnewline || take_token(ts)
            gotnewline = false
            outer = update_outer!(vec, outer)
            vec   = Any[]
            continue
        elseif ¬t === ',' || ¬t === ']' || ¬t === '}'
            report_error(diag(√t,"unexpected \"$(¬t)\" in matrix expression"))
        elseif ¬t === :for
            if !semicolon && length(outer) == 1 && isempty(vec)
                take_token(ts)
                return parse_comprehension(ps, ts, outer[1], closer, opener, t)
            else
                report_error(diag(√t,"invalid comprehension syntax"))
            end
        else
            push!(vec, parse_eqs(ps, ts))
            continue
        end
    end
end

function peek_non_newline_token(ps::ParseState, ts::TokenStream)
    while true
        t = peek_token(ps, ts)
        if Lexer.isnewline(¬t)
            take_token(ts)
            continue
        end
        return t
    end
end

function parse_cat(ps::ParseState, ts::TokenStream, closer, opener, isdict::Bool=false)
    @with_normal_ops ps begin
        @with_inside_vec ps begin
            t = require_token(ps, ts)
            if ¬t === closer
                take_token(ts)
                if closer === '}'
                    return ⨳(:cell1d) ⤄ t
                elseif closer === ']'
                    return ⨳(:vcat) ⤄ t
                else
                    error("unknown closer $closer")
                end
            end
            frst = parse_eqs(ps, ts)
            if is_dict_literal(frst)
                nt = peek_non_newline_token(ps, ts)
                if ¬nt === :for
                    take_token(ts)
                    return parse_dict_comprehension(ps, ts, frst, closer, opener, nt)
                else
                    return parse_dict(ps, ts, frst, closer, opener)
                end
            end
            nt = peek_token(ps, ts)
            if ¬nt === ','
                return parse_vect(ps, ts, frst, closer, opener)
            elseif ¬nt === :for
                take_token(ts)
                return parse_comprehension(ps, ts, frst, closer, opener, nt)
            elseif Lexer.isnewline(nt)
                take_token(ts)
                nnt = peek_token(ps, ts)
                if isa(¬nnt, Char) && (¬nnt == ',' || ¬nnt == closer)
                    return parse_vect(ps, ts, frst, closer, opener)
                else
                    return parse_matrix(ps, ts, frst, closer, true, opener)
                end
            else
                return parse_matrix(ps, ts, frst, closer, false, opener)
            end
        end
    end
end

kw_to_eq(e) = isexpr(¬e, :kw) ? (⨳(:(=)) ⪥ e) : e

function parameters_to_block(ts, e)
    if isexpr(¬e, :parameters)
        if length((¬e).args) == 0
            return Expr(:tuple) ⤄ Lexer.nullrange(ts)
        elseif length((¬e).args) == 1
            return parameters_to_block(ts, collect(children(e))[1])
        elseif length((¬e).args) == 2
            fst, snd = collect(children(e))
            if isexpr(¬fst, :parameters) && snd !== nothing
                rec = parameters_to_block(ts, fst)
                snd = parameters_to_block(ts, snd)
                append!(snd,rec)
                return snd
            else
                return nothing
            end
        else
            return nothing
        end
    else
        return Any[kw_to_eq(e)]
    end
end

# convert an arglist to a tuple or block expr
# leading-semi? means we saw (; ...)
# comma? means there was a comma after the first expression
function arglist_to_tuple(ts, leading_semi, comma, args, first)
     if isempty(args) && !leading_semi && !comma
         return ⨳(:block) ⪥ first
     elseif !comma && length(args) == 1 && isexpr(¬(args[1]), :parameters)
         blk = parameters_to_block(ts, args[1])
         if blk != nothing
             b = ⨳(:block) ⤄ Lexer.nullrange(ts)
             if !leading_semi
                 return (b ⪥ first) ⪥ blk
             elseif isempty((¬first).args) && isempty(blk)
                 return b
             end
         end
     end
     if !comma && (first == ()) && length(args) == 0
         return ⨳(:block)
     else
         if length(args) >= 1 && isexpr(¬(args[1]), :parameters)
             return (⨳(:tuple, args[1]) ⪥ first) ⪥ map(kw_to_eq,args[2:end])
         else
             return ((⨳(:tuple) ⤄ Lexer.nullrange(ts)) ⪥ first) ⪥ map(kw_to_eq,args)
         end
     end
end


# TODO: these are unnecessary if base/client.jl didn't need to parse error string
function not_eof_1(ts)
    Lexer.eof(ts) && throw(Incomplete(:char, diag(here(ts),"incomplete: invalid character literal")))
    return Lexer.readchar(ts)
end

function not_eof_2(ts)
    Lexer.eof(ts) && throw(Incomplete(:cmd, diag(here(ts),"incomplete: invalid \"`\" literal")))
    return Lexer.readchar(ts)
end

function not_eof_3(ts)
    Lexer.eof(ts) && throw(Incomplete(:string, diag(here(ts),"incomplete: invalid string syntax")))
    return Lexer.readchar(ts)
end

function parse_backquote(ps::ParseState, ts::TokenStream)
    buf = IOBuffer()
    c   = not_eof_2(ts)
    r = Lexer.startrange(ts)
    while true
        c === '`' && break
        if c === '\\'
            nc = not_eof_2(ts)
            if nc === '`'
                write(buf, nc)
            else
                write(buf, '\\')
                write(buf, nc)
            end
        else
            write(buf, c)
        end
        c = not_eof_2(ts)
        continue
    end
    return ⨳(:macrocall, Symbol("@cmd") ⤄ Lexer.nullrange(ts),
        String(take!(buf)) ⤄ Lexer.makerange(ts, r))
end

function parse_interpolate(ps::ParseState, ts::TokenStream, start, srange)
    c = Lexer.peekchar(ts)
    function report_error(D)
        !isa(D, Diagnostic) && rethrow(D)
        diag(D, √start, "In interpolation syntax starting here")
        diag(D, srange, "In string expression beginning here")
        throw(D)
    end
    if Lexer.is_identifier_char(c)
        try
            return parse_atom(ps, ts)
        catch err
            report_error(err)
        end
    elseif c === '('
        Lexer.readchar(ts)
        ex = try
            parse_eqs(ps, ts)
        catch err
            report_error(err)
        end
        nt = require_token(ps, ts)
        ¬nt !== ')' && report_error(diag(√nt, "Expected ')'"))
        take_token(ts)
        return ex
    else
        report_error(diag(here(ts),"invalid interpolation syntax: \"$c\""))
    end
end

function tostr(buf::IOBuffer, custom::Bool)
    str = String(take!(buf))
    custom && return str
    str = unescape_string(str)
    if !(@compat isvalid(String,str))
        throw(diag(√str,"string contains invalid UTF8 sequence"))
    end
   return str
end

function _parse_string_literal(ps::ParseState, ts::TokenStream, head::Symbol, n::Integer, custom::Bool)
    # We're now past the first quote
    r       = Lexer.startrange(ts)
    c       = not_eof_3(ts)
    b       = IOBuffer()
    args    = Any[]
    quotes  = 0
    orange  = Lexer.makerange(ts, r)
    srange  = Lexer.makerange(ts, r)
    while true
        if c == '"'
            if quotes < n
                c = not_eof_3(ts)
                quotes += 1
                continue
            end
            push!(args,tostr(b, custom) ⤄ srange)
            return (⨳(head) ⤄ srange) ⪥ args
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
        elseif c === '\\'
            nxch = not_eof_3(ts)
            if !custom || nxch !== '"'
                write(b, '\\')
            end
            write(b, nxch)
            srange = Lexer.makerange(ts, r)
            c = Lexer.readchar(ts)
            quotes = 0
            continue
        elseif c === '$' && !custom
            str = tostr(b, custom) ⤄ srange
            iex = parse_interpolate(ps, ts, before(here(ts)), before(orange))
            push!(args, str)
            push!(args, isa(¬iex, AbstractString) ? ⨳(:single_quoted_string,
                iex) : iex)
            r = Lexer.startrange(ts)
            srange = Lexer.makerange(ts, r)
            c = Lexer.readchar(ts)
            b = IOBuffer()
            quotes = 0
            continue
        else
            write(b, c)
            srange = Lexer.makerange(ts, r)
            c = not_eof_3(ts)
            quotes = 0
            continue
        end
    end
end

interpolate_string_literal(ex) = isa(¬ex, Expr) && length((¬ex).args) > 1
triplequote_string_literal(ex) = isa(¬ex, Expr) && (¬ex).head === :triple_quoted_string

function parse_string_literal(ps::ParseState, ts::TokenStream, custom)
    r = Lexer.startrange(ts)
    if Lexer.peekchar(ts)  === '"'
        Lexer.takechar(ts)
        if Lexer.peekchar(ts) === '"'
            Lexer.takechar(ts)
            ex = _parse_string_literal(ps, ts, :triple_quoted_string, 2, custom)
            ret = dedent_triple_quoted_string(ex)
        else
            ret = ⨳(:single_quoted_string, "" ⤄ Lexer.makerange(ts, r))
        end
    else
        ret = _parse_string_literal(ps, ts, :single_quoted_string, 0, custom)
    end
    # Used to protect against interpolated strings in dedenting
    for (i, arg) in enumerate((¬ret).args)
        isexpr(arg, :single_quoted_string) && ((¬ret).args[i] = arg.args[1])
    end
    ret
end

function longest_common_prefix(prefixa, prefixb)
    maxplength = min(length(prefixa.data), length(prefixb.data))
    maxplength == 0 && return ""
    idx = findfirst(i->(prefixa.data[i] != prefixb.data[i]),1:maxplength)
    idx = idx == 0 ? maxplength : idx - 1
    prefixa[1:idx]
end

function drop_leading_newline(mstr)
    arg1 = (¬mstr).args[1]
    if !isempty(arg1) && arg1[1] == '\n'
        if length(arg1) == 1
            mstr = ⨳((¬mstr).head) ⪥ collect(children(mstr))[2:end]
        else
            (¬mstr).args[1] = arg1[2:end]
        end
    end
    mstr
end

function dedent_triple_quoted_string(mstr)
    # Compute longest common prefix of ' ' and '\t'
    els = filter(x->!isempty(x),
        map(str->collect(drop(EachLine(IOBuffer(¬str)), 1)),
        filter(x->isa(¬x, AbstractString), children(mstr))))
    isempty(els) && return drop_leading_newline(mstr)
    # Shortcur if the closing """ is at the beginning of the line
    lastline = (¬mstr).args[end]
    isa(lastline,AbstractString) && (!isempty(lastline) && lastline[end] == '\n') &&
        return drop_leading_newline(mstr)
    prefixes = AbstractString[]
    maxlength = 0
    for (i,lines) in enumerate(els)
        for (j,line) in enumerate(lines)
            maxlength = max(maxlength,length(line))
            idx = findfirst(c->(c != ' ' && c != '\t' && c != '\n'),line)
            if idx == 0
                if i == length(els) && j == length(lines)
                    idx = length(line)+1
                else
                    continue # All whitespace lines in the middle are ignored
                end
            end
            push!(prefixes, line[1:(idx-1)])
        end
    end
    prefix = isempty(prefixes) ? " "^maxlength : reduce(longest_common_prefix, prefixes)
    for (i,str) in enumerate(children(mstr))
        !isa(¬str, AbstractString) && continue
        (¬mstr).args[i] = replace(¬str,string('\n',prefix),"\n")
    end
    drop_leading_newline(mstr)
end

function _parse_atom(ps::ParseState, ts::TokenStream)
    t = require_token(ps, ts)
    if !isa(¬t, Char) && isa(¬t, Number)
        return take_token(ts)

    # char literal
    elseif ¬t === Symbol("'")
        take_token(ts)
        r = Lexer.startrange(ts)
        fch = not_eof_1(ts)
        fch === '\'' && throw(diag(Lexer.makerange(ts,r)⤄√t,"empty char literal not allowed"))
        if fch !== '\\' && !Lexer.eof(fch) && Lexer.peekchar(ts) === '\''
            # easy case 1 char no \
            Lexer.takechar(ts)
            return fch ⤄ Lexer.makerange(ts, r)
        else
            c, b = fch, IOBuffer()
            while true
                c === '\'' && break
                write(b, c)
                c === '\\' && write(b, not_eof_1(ts))
                c = not_eof_1(ts)
                continue
            end
            str = unescape_string(String(take!(b)))
            if length(str) == 1
                # one byte e.g. '\xff' maybe not valid UTF-8
                # but we want to use the raw value as a codepoint in this case
                return str[1] ⤄ Lexer.makerange(ts, r)
            else
                if length(str) != 1  || !is_valid_utf8(str)
                    throw(diag(Lexer.makerange(ts,r)⤄√t,
                        "invalid character literal, got \'$str\'"))
                end
                return str[1] ⤄ Lexer.makerange(ts, r)
            end
        end

    # symbol / expression quote
    elseif ¬t === :(:)
        take_token(ts)
        nt = peek_token(ps, ts)
        if is_closing_token(ps, nt) && (ps.space_sensitive || !isa(¬nt, Symbol))
            return :(:) ⤄ t
        end
        return ⨳(:quote, _parse_atom(ps, ts))

    # misplaced =
    elseif ¬t === :(=)
        throw(diag(√t,"unexpected \"$(¬t)\""))

    # identifier
    elseif isa(¬t, Symbol)
        return take_token(ts)

    # parens or tuple
    elseif ¬t === '('
        take_token(ts)
        @with_normal_ops ps begin
            @with_whitespace_newline ps begin
                rt = require_token(ps, ts)
                if ¬rt === ')'
                    # empty tuple
                    take_token(ts)
                    return (⨳(:tuple) ⤄ t) ⤄ rt
                elseif ¬rt in Lexer.SYNTACTIC_OPS
                    # allow (=) etc. since this function is also used to parse
                    # quoted expression. However, anything else will invariably
                    # cause an invalid identifier name
                    t = take_token(ts)
                    nt = require_token(ps, ts)
                    ¬nt !== ')' && throw(diag(after(√t),"Expected ')'"))
                    take_token(ts)
                    return t
                elseif ¬rt === ';'
                    res = arglist_to_tuple(ts, true, false, parse_arglist(ps, ts, ')', t),
                        ())
                    take_token(ts)
                    return res
                else
                    # here we parse the first subexpression separately,
                    # so we can look for a comma to see if it is a tuple
                    # this lets us distinguish (x) from (x,)
                    ex = parse_eqs(ps, ts)
                    nt  = require_token(ps, ts)
                    if ¬nt === ')'
                        take_token(ts)
                        if isa(¬ex, Expr) && (¬ex).head === :(...)
                            # (ex...)
                            return ⨳(:tuple, ex)
                        else
                            # value in parens (x)
                            return ex
                        end
                    elseif ¬nt === :for
                        take_token(ts)
                        gen = parse_generator(ps, ts, ex, nt)
                        tok = require_token(ps, ts)
                        if ¬tok !== ')'
                            D = diag(√tok,"Expected ')'")
                            diag(D,√t,"to match '$(¬t)' here")
                            throw(D)
                        end
                        take_token(ts)
                        return gen
                    else
                        if (¬nt === ',')
                            take_token(ts)
                        elseif ¬nt !== ';'
                            D = diag(before(nt), "Expected ',' or ')'")
                            diag(D,√t,"to match '$(¬t)' here")
                            throw(D)
                        end
                        res = arglist_to_tuple(ts, false, ¬nt == ',',
                            parse_arglist(ps, ts, ')', t), (ex,))
                        take_token(ts)
                        return res
                    end
                end
            end
        end

    # cat expression
    elseif ¬t === '['
        take_token(ts)
        vex = parse_cat(ps, ts, ']', t)
        if isempty((¬vex).args)
            (¬vex).head = :vect
        end
        return vex

    # string literal
    elseif ¬t === '"'
        take_token(ts)
        sl = parse_string_literal(ps, ts, false)
        if VERSION < v"0.4" && triplequote_string_literal(sl)
            return ⨳(:macrocall, Symbol("@mstr"), sl.args...)
        end
        if interpolate_string_literal(sl)
            notzerolen = (s) -> !(isa(¬s, AbstractString) && isempty(¬s))
            return ⨳(:string, filter(notzerolen, children(sl))...)
        end
        return (length((¬sl).args) == 0 ? "" : (¬sl).args[1]) ⤄ sl

    # macro call
    elseif ¬t === '@'
        take_token(ts)
        @space_sensitive ps begin
            head = parse_unary_prefix(ps, ts)
            if (peek_token(ps, ts); ts.isspace)
                name = macroify_name(head)
                ¬name == Symbol("@__LINE__") && return curline(ts) ⤄ name
                ex = ⨳(:macrocall, name ⤄ t)
                ex ⪥ parse_space_separated_exprs(ps, ts)
                return ex
            else
                call = parse_call_chain(ps, ts, head, true)
                if isexpr(¬call, :call)
                    args = collect(children(call))
                    ex = ⨳(:macrocall, macroify_name(args[1]) ⤄ t) ⪥ args[2:end]
                    return ex
                else
                    name = macroify_name(call)
                    ¬name == Symbol("@__LINE__") && return curline(ts) ⤄ name
                    ex = ⨳(:macrocall, name ⤄ t)
                    ex ⪥ parse_space_separated_exprs(ps, ts)
                    return ex
                end
            end
        end

    # command syntax
    elseif ¬t === '`'
        take_token(ts)
        return parse_backquote(ps, ts)

    else
        throw(diag(√t,"invalid syntax: \"$(¬take_token(ts))\""))
    end
end

function parse_atom(ps::ParseState, ts::TokenStream)
    ex = _parse_atom(ps, ts)
    if (¬ex !== :(=>) && (¬ex in Lexer.SYNTACTIC_OPS)) || ¬ex === :(...)
        throw(diag(√ex, "invalid identifier name \"$(¬ex)\""))
    end
    return ex
end

function is_valid_modref(ex::Expr)
    return length(ex.args) == 2 &&
            ex.head === :(.) &&
           ((isa(ex.args[2], Expr) &&
             ex.args[2].head === :quote &&
             isa(ex.args[2].args[1], Symbol)) ||
            (isa(ex.args[2], QuoteNode) &&
             isa(ex.args[2].value, Symbol))) &&
           (isa(ex.args[1], Symbol) || is_valid_modref(ex.args[1]))
end

function macroify_name(ex)
    if isa(¬ex, Symbol)
        return Symbol(string('@', ¬ex)) ⤄ ex
    elseif isa(¬ex, QuoteNode) && isa((¬ex).value, Symbol)
        return QuoteNode(Symbol(string('@', (¬ex).value))) ⤄ ex
    elseif is_valid_modref(¬ex)
        args = collect(children(ex))
        return ⨳(:(.), args[1], macroify_name(args[2]))
    else
        throw(diag(√ex,"invalid macro use \"@($(¬ex))\""))
    end
end

function is_doc_string_literal(e)
    isa(¬e, AbstractString) ||
        isexpr(¬e, :string) ||
        (isexpr(¬e, :macrocall) && (¬e).args[1] == Symbol("@doc_str"))
end

function parse_docstring(ps::ParseState, ts::TokenStream, down)
    ex = down(ps, ts)
    if is_doc_string_literal(ex)
        while true
            t = peek_token(ps, ts)
            if is_closing_token(ps, t)
                return ex
            elseif Lexer.isnewline(t)
                take_token(ts)
            else
                break
            end
        end
        Lexer.eof(Lexer.peek_token(ts, false)) && return ex
        return ⨳(:macrocall, Symbol("@doc") ⤄ Lexer.nullrange(ts), ex, down(ps, ts))
    end
    return ex
end

#========================#
# Parser Entry Method
#========================#

function parse(ts::TokenStream; production = parse_stmts)
    Lexer.skipws_and_comments(ts)
    t = Lexer.peek_token(ts, false)
    while true
        Lexer.eof(t) && return nothing
        if Lexer.isnewline(¬t)
            take_token(ts)
            t = Lexer.peek_token(ts, false)
            continue
        end
        break
    end
    ps = ParseState()
    ret = production(ps, ts)
    return isa(ret, Lexer.AbstractToken) ? ¬ret : ret
end

parse(io::IO) = parse(TokenStream(io), production = parse_stmts)
parse(str::AbstractString)  = parse(TokenStream(IOBuffer(str)), production = parse_stmts)

end
