module Lexer

using Compat

import Base.UTF8proc
import Base: position
import ..Diagnostics: diag, Incomplete, before
using ..Tokens
using ..Tokens: √

export Token, TokenStream, next_token, set_token!, last_token,
       put_back!, peek_token, take_token, require_token

const SYM_TRUE  = Symbol("true")
const SYM_FALSE = Symbol("false")
const SYM_CTRANSPOSE = Symbol("'")

const EOF_CHAR = convert(Char,typemax(UInt32))

add_dot(s::Symbol) = Symbol(string(".",s))
add_dots!(ops) = append!(ops,add_dot.(ops))

const ALL_OPS = Dict{Symbol,Any}(
:assignment  =>
       [add_dots!([:(=), :(+=), :(-=), :(*=), :(/=), :(//=), :(|=),
            :(^=), :(÷=), :(%=), :(<<=), :(>>=), :(>>>=), :(\=), :(&=)]);
        [:(:=), :(=>), :(~), :($=)]],
:conditional => [:(?)],
:lazy_or     => [:(||)],
:lazy_and    => [:(&&)],
# NOTE: -- is deprecated in 0.4
:arrow       =>
       [Symbol("--"); :(-->);
        add_dots!([:(←), :(→), :(↔), :(↚), :(↛), :(↠), :(↣), :(↦),
        :(↮), :(⇎), :(⇏), :(⇒), :(⇔), :(⇴), :(⇶), :(⇷), :(⇸), :(⇹),
        :(⇺), :(⇻), :(⇼), :(⇽), :(⇾), :(⇿), :(⟵), :(⟶), :(⟷), :(⟷),
        :(⟹), :(⟺), :(⟻), :(⟼), :(⟽), :(⟾), :(⟿), :(⤀), :(⤁), :(⤂),
        :(⤃), :(⤄), :(⤅), :(⤆), :(⤇), :(⤌), :(⤍), :(⤎), :(⤏), :(⤐),
        :(⤑), :(⤔), :(⤕), :(⤖), :(⤗), :(⤘), :(⤝), :(⤞), :(⤟), :(⤠),
        :(⥄), :(⥅), :(⥆), :(⥇), :(⥈), :(⥊), :(⥋), :(⥎), :(⥐), :(⥒),
        :(⥓), :(⥖), :(⥗), :(⥚), :(⥛), :(⥞), :(⥟), :(⥢), :(⥤), :(⥦),
        :(⥧), :(⥨), :(⥩), :(⥪), :(⥫), :(⥬), :(⥭), :(⥰), :(⧴), :(⬱),
        :(⬰), :(⬲), :(⬳), :(⬴), :(⬵), :(⬶), :(⬷), :(⬸), :(⬹), :(⬺),
        :(⬻), :(⬼), :(⬽), :(⬾), :(⬿), :(⭀), :(⭁), :(⭂), :(⭃), :(⭄),
        :(⭇), :(⭈), :(⭉), :(⭊), :(⭋), :(⭌), :(￩), :(￫)])],
:comparison =>
       [Symbol(".!"); :(<:); :(>:);
        add_dots!(
       [:(>),  :(<), :(>=), :(≥), :(<=), :(≤), :(==), :(===), :(≡),
        :(!=), :(≠), :(!==), :(≢), :(∈),
        :(∉), :(∋), :(∌), :(⊆), :(⊈), :(⊂), :(⊄), :(⊊), :(∝), :(∊), :(∍),
        :(∥), :(∦), :(∷), :(∺), :(∻), :(∽), :(∾), :(≁), :(≃), :(≄), :(≅),
        :(≆), :(≇), :(≈), :(≉), :(≊), :(≋), :(≌), :(≍), :(≎), :(≐), :(≑),
        :(≒), :(≓), :(≔), :(≕), :(≖), :(≗), :(≘), :(≙), :(≚), :(≛), :(≜),
        :(≝), :(≞), :(≟), :(≣), :(≦), :(≧), :(≨), :(≩), :(≪), :(≫), :(≬),
        :(≭), :(≮), :(≯), :(≰), :(≱), :(≲), :(≳), :(≴), :(≵), :(≶), :(≷),
        :(≸), :(≹), :(≺), :(≻), :(≼), :(≽), :(≾), :(≿), :(⊀), :(⊁), :(⊃),
        :(⊅), :(⊇), :(⊉), :(⊋), :(⊏), :(⊐), :(⊑), :(⊒), :(⊜), :(⊩), :(⊬),
        :(⊮), :(⊰), :(⊱), :(⊲), :(⊳), :(⊴), :(⊵), :(⊶), :(⊷), :(⋍), :(⋐),
        :(⋑), :(⋕), :(⋖), :(⋗), :(⋘), :(⋙), :(⋚), :(⋛), :(⋜), :(⋝), :(⋞),
        :(⋟), :(⋠), :(⋡), :(⋢), :(⋣), :(⋤), :(⋥), :(⋦), :(⋧), :(⋨), :(⋩),
        :(⋪), :(⋫), :(⋬), :(⋭), :(⋲), :(⋳), :(⋴), :(⋵), :(⋶), :(⋷), :(⋸),
        :(⋹), :(⋺), :(⋻), :(⋼), :(⋽), :(⋾), :(⋿), :(⟈), :(⟉), :(⟒), :(⦷),
        :(⧀), :(⧁), :(⧡), :(⧣), :(⧤), :(⧥), :(⩦), :(⩧), :(⩪), :(⩫), :(⩬),
        :(⩭), :(⩮), :(⩯), :(⩰), :(⩱), :(⩲), :(⩳), :(⩴), :(⩵), :(⩶), :(⩷),
        :(⩸), :(⩹), :(⩺), :(⩻), :(⩼), :(⩽), :(⩾), :(⩿), :(⪀), :(⪁), :(⪂),
        :(⪃), :(⪄), :(⪅), :(⪆), :(⪇), :(⪈), :(⪉), :(⪊), :(⪋), :(⪌), :(⪍),
        :(⪎), :(⪏), :(⪐), :(⪑), :(⪒), :(⪓), :(⪔), :(⪕), :(⪖), :(⪗), :(⪘),
        :(⪙), :(⪚), :(⪛), :(⪜), :(⪝), :(⪞), :(⪟), :(⪠), :(⪡), :(⪢), :(⪣),
        :(⪤), :(⪥), :(⪦), :(⪧), :(⪨), :(⪩), :(⪪), :(⪫), :(⪬), :(⪭), :(⪮),
        :(⪯), :(⪰), :(⪱), :(⪲), :(⪳), :(⪴), :(⪵), :(⪶), :(⪷), :(⪸), :(⪹),
        :(⪺), :(⪻), :(⪼), :(⪽), :(⪾), :(⪿), :(⫀), :(⫁), :(⫂), :(⫃), :(⫄),
        :(⫅), :(⫆), :(⫇), :(⫈), :(⫉), :(⫊), :(⫋), :(⫌), :(⫍), :(⫎), :(⫏),
        :(⫐), :(⫑), :(⫒), :(⫓), :(⫔), :(⫕), :(⫖), :(⫗), :(⫘), :(⫙), :(⫷),
        :(⫸), :(⫹), :(⫺), :(⊢), :(⊣)])],
:pipe       => [:(|>),  :(<|)],
:colon      => [:(:), :(..)],
:plus       => [:($);
        add_dots!([:(+), :(-), :(++), :(⊕), :(⊖), :(⊞), :(⊟), :(|), :(∪), :(∨),
        :(⊔), :(±), :(∓), :(∔), :(∸), :(≂), :(≏), :(⊎), :(⊻), :(⊽),
        :(⋎), :(⋓), :(⧺), :(⧻), :(⨈), :(⨢), :(⨣), :(⨤), :(⨥), :(⨦), :(⨧),
        :(⨨), :(⨩), :(⨪), :(⨫), :(⨬), :(⨭), :(⨮), :(⨹), :(⨺), :(⩁), :(⩂),
        :(⩅), :(⩊), :(⩌), :(⩏), :(⩐), :(⩒), :(⩔), :(⩖), :(⩗), :(⩛), :(⩝),
        :(⩡), :(⩢), :(⩣)])],
:bitshift   => add_dots!([:(<<), :(>>), :(>>>)]),
:times      =>
       add_dots!([:(*), :(/), :(÷), :(%), :(⋅), :(∘), :(×),
        :(\), :(&), :(∩), :(∧), :(⊗), :(⊘), :(⊙), :(⊚), :(⊛), :(⊠), :(⊡),
        :(⊓), :(∗), :(∙), :(∤), :(⅋), :(≀), :(⊼), :(⋄), :(⋆), :(⋇), :(⋉), :(⋊),
        :(⋋), :(⋌), :(⋏), :(⋒), :(⟑), :(⦸), :(⦼), :(⦾), :(⦿), :(⧶), :(⧷), :(⨇),
        :(⨰), :(⨱), :(⨲), :(⨳), :(⨴), :(⨵), :(⨶), :(⨷), :(⨸), :(⨻), :(⨼), :(⨽),
        :(⩀), :(⩃), :(⩄), :(⩋), :(⩍), :(⩎), :(⩑), :(⩓), :(⩕), :(⩘), :(⩚), :(⩜),
        :(⩞), :(⩟), :(⩠), :(⫛), :(⊍)]),
:rational   => add_dots!([:(//)]),
:power      => add_dots!(
       [:(^), :(↑), :(↓), :(⇵), :(⟰), :(⟱), :(⤈), :(⤉), :(⤊), :(⤋),
        :(⤒), :(⤓),  :(⥉), :(⥌), :(⥍), :(⥏), :(⥑), :(⥔), :(⥕), :(⥘), :(⥙),
        :(⥜), :(⥝),  :(⥠), :(⥡), :(⥣), :(⥥), :(⥮), :(⥯), :(￪), :(￬)]),
:decl       => [:(::)],
:dot        => [:(.)])

if VERSION < v"0.4.0-dev+573"
    const OPS_BY_PRECEDENT = Any[
        :assignment, :conditional, :lazy_or, :lazy_and, :arrow, :comparison, :pipe,
        :colon, :plus, :bitshift, :times, :rational, :power, :decl, :dot]
else
    const OPS_BY_PRECEDENT = Any[
        :assignment, :conditional, :arrow, :lazy_or, :lazy_and, :comparison, :pipe,
        :colon, :plus, :bitshift, :times, :rational, :power, :decl, :dot]
end

precedent_ops(n::Integer) = Set{Symbol}(ALL_OPS[OPS_BY_PRECEDENT[n]])
precedent_ops(s::Symbol) = Set{Symbol}(ALL_OPS[s])

const ASSIGNMENT_OPS = precedent_ops(:assignment)

const UNARY_OPS = Set{Symbol}([:(+),  :(-), :(!), :(~), :(<:), :(¬),
                               :(>:), :(√), :(∛), :(∜)])

const UNARY_AND_BINARY_OPS = Set{Symbol}([:(+), :(-), :($), :(&), :(~)])

"Operators are special forms, not function names."
const SYNTACTIC_OPS = Set{Symbol}([:(:=); :(-->); :($=); :(=>); :(||); :(.);
                                    :(...); :(->); add_dots!([
                                    :(=)  , :(+=)  ,  :(-=),
                                    :(/=) , :(//=) ,  :(*=),
                                    :(\=) , :(^=)  ,  :(%=),
                                    :(|=) , :(&=)  ,  :(<<=),
                                    :(>>=), :(>>>=),  :(&&) ])])

const SYNTACTIC_UNARY_OPS = Set{Symbol}([:($), :(&), :(::)])

const OPERATORS = union(Set([:(~), :(!), :(->), :(√), :(∛), :(∜), :(...), :(¬),
                             :(.'), SYM_CTRANSPOSE]),
                             [Set(ALL_OPS[ops]) for ops in OPS_BY_PRECEDENT]...)

const RESERVED_WORDS = Set{Symbol}([:begin,  :while, :if, :for, :try, :return,
                                    :break, :continue, :function, :stagedfunction,
                                    :macro, :quote, :let, :local, :global, :const,
                                    :abstract, :typealias, :type, :bitstype, :immutable,
                                    :ccall, :do, :module, :baremodule, :using, :import,
                                    :export, :importall])
#= Helper functions =#

const PRECEDENCE_MAP = Dict{Symbol, Int}()

let
    for (i, ops) in enumerate(OPS_BY_PRECEDENT)
        for op in ALL_OPS[ops]
            PRECEDENCE_MAP[op] = i
        end
    end
end

operator_precedence(op::Symbol) = PRECEDENCE_MAP[op]

is_syntactic_op(op::Symbol) = in(op, SYNTACTIC_OPS)
is_syntactic_op(op) = false

is_syntactic_unary_op(op::Symbol) = in(op, SYNTACTIC_UNARY_OPS)
is_syntactic_unary_op(op) = false

const SPECIAL_CHARS = Set{Char}("()[]{},;\"`@")

is_special_char(c::Char) = in(c, SPECIAL_CHARS)

isnewline(c::Char) = c === '\n'
isnewline(t::AbstractToken) = isnewline(¬t)
isnewline(c) = false

function isuws(ch::Char)
    c = UInt32(ch)
    return (c==9    || c==10   || c==11   || c==12   || c==13   || c==32 ||
            c==133  || c==160  || c==5760 || c==6158 || c==8192 ||
            c==8193 || c==8194 || c==8195 || c==8196 || c==8197 ||
            c==8198 || c==8199 || c==8200 || c==8201 || c==8202 ||
            c==8232 || c==8233 || c==8239 || c==8287 || c==12288)
end

isbom(c::Char) = UInt32(c) == 0xFEFF

is_zero_width_space(c::Char) = c === '\u200b' || c === '\u2060' || c === '\ufeff'

is_ignorable_char(c::Char) = is_zero_width_space(c) ||
                             ('\u200c' <= c <= '\u200f') ||
                             (c === '\u00ad' || c === '\u2061' || c === '\u115f')

function is_cat_id_start(ch::Char, cat::Integer)
    c = UInt32(ch)
    return (cat == UTF8proc.UTF8PROC_CATEGORY_LU || cat == UTF8proc.UTF8PROC_CATEGORY_LL ||
            cat == UTF8proc.UTF8PROC_CATEGORY_LT || cat == UTF8proc.UTF8PROC_CATEGORY_LM ||
            cat == UTF8proc.UTF8PROC_CATEGORY_LO || cat == UTF8proc.UTF8PROC_CATEGORY_NL ||
            cat == UTF8proc.UTF8PROC_CATEGORY_SC ||  # allow currency symbols
            cat == UTF8proc.UTF8PROC_CATEGORY_SO ||  # other symbols

            # math symbol (category Sm) whitelist
            (c >= 0x2140 && c <= 0x2a1c &&
             ((c >= 0x2140 && c <= 0x2144) || # ⅀, ⅁, ⅂, ⅃, ⅄
              c == 0x223f || c == 0x22be || c == 0x22bf || # ∿, ⊾, ⊿
              c == 0x22a4 || c == 0x22a5 || # ⊤ ⊥
              (c >= 0x22ee && c <= 0x22f1) || # ⋮, ⋯, ⋰, ⋱

              (c >= 0x2202 && c <= 0x2233 &&
               (c == 0x2202 || c == 0x2205 || c == 0x2206 || # ∂, ∅, ∆
                c == 0x2207 || c == 0x220e || c == 0x220f || # ∇, ∎, ∏
                c == 0x2210 || c == 0x2211 || # ∐, ∑
                c == 0x221e || c == 0x221f || # ∞, ∟
                c >= 0x222b)) || # ∫, ∬, ∭, ∮, ∯, ∰, ∱, ∲, ∳

              (c >= 0x22c0 && c <= 0x22c3) ||  # N-ary big ops: ⋀, ⋁, ⋂, ⋃
              (c >= 0x25F8 && c <= 0x25ff) ||  # ◸, ◹, ◺, ◻, ◼, ◽, ◾, ◿

              (c >= 0x266f &&
               (c == 0x266f || c == 0x27d8 || c == 0x27d9 || # ♯, ⟘, ⟙
                (c >= 0x27c0 && c <= 0x27c2) ||  # ⟀, ⟁, ⟂
                (c >= 0x29b0 && c <= 0x29b4) ||  # ⦰, ⦱, ⦲, ⦳, ⦴
                (c >= 0x2a00 && c <= 0x2a06) ||  # ⨀, ⨁, ⨂, ⨃, ⨄, ⨅, ⨆
                (c >= 0x2a09 && c <= 0x2a16) ||  # ⨉, ⨊, ⨋, ⨌, ⨍, ⨎, ⨏, ⨐, ⨑, ⨒,
                                                 # ⨓, ⨔, ⨕, ⨖
                c == 0x2a1b || c == 0x2a1c)))) || # ⨛, ⨜

            (c >= 0x1d6c1 && # variants of \nabla and \partial
             (c == 0x1d6c1 || c == 0x1d6db ||
              c == 0x1d6fb || c == 0x1d715 ||
              c == 0x1d735 || c == 0x1d74f ||
              c == 0x1d76f || c == 0x1d789 ||
              c == 0x1d7a9 || c == 0x1d7c3)) ||

            # super- and subscript +-=()
            (c >= 0x207a && c <= 0x207e) ||
            (c >= 0x208a && c <= 0x208e) ||

            # angle symbols
            (c >= 0x2220 && c <= 0x2222) || # ∠, ∡, ∢
            (c >= 0x299b && c <= 0x29af) || # ⦛, ⦜, ⦝, ⦞, ⦟, ⦠, ⦡, ⦢, ⦣, ⦤, ⦥,
                                            # ⦦, ⦧, ⦨, ⦩, ⦪, ⦫, ⦬, ⦭, ⦮, ⦯
            # Other_ID_Start
            c == 0x2118 || c == 0x212E || # ℘, ℮
            (c >= 0x309B && c <= 0x309C)) # katakana-hiragana sound marks
end

function is_identifier_char(c::Char)
    if ((c >= 'A' && c <= 'Z') ||
        (c >= 'a' && c <= 'z') || c == '_' ||
        (c >= '0' && c <= '9') || c == '!')
        return true
    elseif (UInt32(c) < 0xA1 || UInt32(c) > 0x10ffff)
        return false
    end
    cat = UTF8proc.category_code(c)
    is_cat_id_start(c, cat) && return true
    if cat == UTF8proc.UTF8PROC_CATEGORY_MN || cat == UTF8proc.UTF8PROC_CATEGORY_MC ||
       cat == UTF8proc.UTF8PROC_CATEGORY_ND || cat == UTF8proc.UTF8PROC_CATEGORY_PC ||
       cat == UTF8proc.UTF8PROC_CATEGORY_SK || cat == UTF8proc.UTF8PROC_CATEGORY_ME ||
       cat == UTF8proc.UTF8PROC_CATEGORY_NO ||
       (0x2032 <= UInt32(c) <= 0x2034) || # primes
       UInt32(c) == 0x0387 || UInt32(c) == 0x19da ||
       (0x1369 <= UInt32(c) <= 0x1371)
       return true
    end
    return false
end

function is_identifier_start_char(c::Char)
    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_')
        return true
    elseif (UInt32(c) < 0xA1 || UInt32(c) > 0x10ffff)
        return false
    end
    cat = UTF8proc.category_code(c)
    return is_cat_id_start(c, cat)
end

"Characters that can be in an operator."
const OPERATOR_CHARS = union([Set(string(op)) for op in OPERATORS]...)

is_opchar(c::Char) = in(c, OPERATOR_CHARS)

"Characters that can follow a `.` in an operator."
const DOT_OP_CHARS = unique(map(op->op[2], filter(op->length(op)>=2&&op[1]=='.',map(string,OPERATORS))))

is_dot_opchar(c::Char) = in(c, DOT_OP_CHARS)

is_operator(op::Symbol) = in(op, OPERATORS)
is_operator(op::AbstractToken) = is_operator(¬op)
is_operator(op) = false

"""
`peekchar(io::Bufer)` -> `Char`
`peekchar(s::IOStream)` -> `Char`
`peekchar(ts::TokenStream)` -> `Char`

Version of `peekchar` for `IOBuffer`, `IOStream` and `TokenStream`,
modified from `Base` to give the same semantics as the `IOStream` implementation.
"""
function peekchar end

function peekchar(io::IOBuffer)
    if !io.readable || io.ptr > io.size
        return EOF_CHAR
    end
    ch = convert(UInt8,io.data[io.ptr])
    if ch < 0x80
        return convert(Char,ch)
    end
    # mimic utf8.next function
    trailing = Base.utf8_trailing[ch+1]
    c::UInt32 = 0
    for j = 1:trailing
        c += ch
        c <<= 6
        ch = convert(UInt8,io.data[io.ptr+j])
    end
    c += ch
    c -= Base.utf8_offset[trailing+1]
    return convert(Char,c)
end

# this implementation is copied from Base
const _CHTMP = Array{Char}(1)

peekchar(s::IOStream) = begin
    if ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Char}), s, _CHTMP) < 0
        return EOF_CHAR
    end
    return _CHTMP[1]
end

eof(io::IO) = Base.eof(io)
eof(t::AbstractToken) = eof(¬t)
eof(c) = c===EOF_CHAR

readchar(io::IO) = eof(io) ? EOF_CHAR : read(io, Char)
takechar(io::IO) = (readchar(io); io)

#= Token Stream =#

type TokenStream{T, I <: IO}
    io::I
    lineno::Int
    lasttoken
    putback
    isspace::Bool
    ateof::Bool
    filename::AbstractString
end

(::Type{TokenStream{T}}){T}(io::IO) = TokenStream{T, typeof(io)}(io, 1, nothing, nothing, false, eof(io), "none")
(::Type{TokenStream{T}}){T}(str::AbstractString) = TokenStream{T}(IOBuffer(str))
TokenStream(x) = TokenStream{Token}(x)



eof(ts::TokenStream) = ts.ateof || eof(ts.io)
skip(ts::TokenStream, n)  = Base.skip(ts.io, n)
position(ts::TokenStream) = Base.position(ts.io)
peekchar(ts::TokenStream) = peekchar(ts.io)

# The last case is the replacement character 0xfffd (3 bytes)
utf8sizeof(c::Char) = UInt32(c) < 0x80 ? 1 : UInt32(c) < 0x800 ? 2 :
    UInt32(c) < 0x10000 ? 3 : UInt32(c) < 0x110000 ? 4 : 3

readchar(ts::TokenStream) = begin
    @assert !eof(ts)
    loc = position(ts.io)
    c = read(ts.io, Char)
    c === '\n' && (ts.lineno += 1)
    return c
end

takechar(ts::TokenStream) = (readchar(ts); ts)

function skipws(ts::TokenStream, newlines::Bool=false)
    nc = peekchar(ts)
    nc === EOF_CHAR && return false
    skipped = false
    while !eof(ts) && (isuws(nc) || isbom(nc)) && (newlines || nc !== '\n')
        takechar(ts)
        nc, skipped = peekchar(ts), true
    end
    return skipped
end

#= Lexer =#
function skip_to_eol(io::IO)
    while !eof(io)
        nc = peekchar(io)
        nc === '\n' && break
        Base.skip(io, utf8sizeof(nc))
    end
    return io
end
skip_to_eol(ts::TokenStream) = skip_to_eol(ts.io)

function read_operator(ts::TokenStream, c::Char, r)
    nc = peekchar(ts)
    if (c === '*') && (nc === '*')
        readchar(ts); loc = Lexer.makerange(ts, r)
        D = diag(loc, "use '^' instead of '**'")
        diag(D, loc, "^", :fixit)
        throw(D)
    end
    # 1 char operator
    if eof(nc) || !is_opchar(nc) || (c === ':' && nc === '-')
        return Symbol(c)
    end
    str, c = [c], nc
    opsym  = Symbol(String(copy(str)))
    while true
        if !eof(c) && is_opchar(c)
            push!(str, c)
            newop = Symbol(String(copy(str)))
            if is_operator(newop)
                skip(ts, utf8sizeof(c))
                c, opsym = peekchar(ts), newop
                continue
            end
        end
        return opsym
    end
end

#=============#
# Read Number
#=============#

function sized_uint_literal(s::AbstractString, b::Integer)
    i = s[1] === '-' ? 3 : 2
    l = (length(s) - i) * b
    l <= 8   && return @compat parse(UInt8,   s)
    l <= 16  && return @compat parse(UInt16,  s)
    l <= 32  && return @compat parse(UInt32,  s)
    l <= 64  && return @compat parse(UInt64,  s)
    l <= 128 && return @compat parse(UInt128, s)
    return @compat parse(BigInt,s)
end

function sized_uint_oct_literal(s::AbstractString)
    contains(s, "o0") && return sized_uint_literal(s, 3)
    len = length(s)
    (len < 5  || (len == 5  && s <= "0o377")) && return @compat parse(UInt8,s)
    (len < 8  || (len == 8  && s <= "0o177777")) && return @compat parse(UInt16, s)
    (len < 13 || (len == 13 && s <= "0o37777777777")) && return @compat parse(UInt32,s)
    (len < 24 || (len == 24 && s <= "0o1777777777777777777777")) && return @compat parse(UInt64,s)
    (len < 45 || (len == 45 && s <= "0o3777777777777777777777777777777777777777777")) && return @compat parse(UInt128,s)
    return @compat parse(BigInt,s)
end

function compare_num_strings(s1::AbstractString, s2::AbstractString)
    s1, s2 = lstrip(s1, '0'), lstrip(s2, '0')
    l1, l2 = length(s1), length(s2)
    return l1 == l2 ? s1 <= s2 : l1 <= l2
end

function is_oct_within_uint128(s::AbstractString)
    len = length(s)
    max = "0o3777777777777777777777777777777777777777777"
    len < 45  && return true
    len > 45  && return false
    len == 45 && s[1] === '-' ? s[2:end] <= max : s <= max
end

function is_within_int128(s::AbstractString)
    len = length(s)
    if s[1] === '-'
        len < 40  && return true
        len > 40  && return false
        len == 40 && return s <= "-170141183460469231731687303715884105728"
    else
        len < 39  && return true
        len > 39  && return false
        len == 39 && s <= "170141183460469231731687303715884105727"
    end
end

# Notes:
# expressions starting with 0x are always hexadecimal literals
# expressions starting with a numeric literal followed by e or E
# are always floating point literals

function string_to_number(str::AbstractString, loc)
    len = length(str)
    len > 0 || throw(ArgumentError("empty string"))
    neg = str[1] === '-'
    # NaN and Infinity
    (str == "NaN" || str == "+NaN" || str == "-NaN") && return NaN
    (str == "Inf" || str == "+Inf" || str == "-Inf") && return Inf
    # floating point literals
    didx, fidx = 0, 0
    isfloat32, isfloat64 = false, false
    for i=1:len
        c = str[i]
        if c === '.'
            @assert isfloat64 == false
            didx, isfloat64 = i, true
        elseif c === 'f'
            @assert i > didx && i != len
            fidx, isfloat32 = i, true
        elseif c === 'e' || c === 'E' || c === 'p' || c === 'P'
            isfloat64 = true
        end
    end
    if isfloat32
        base = @compat parse(Float64,str[1:fidx-1])
        expn = @compat parse(Int,str[fidx+1:end])
        return convert(Float32, base * 10.0 ^ expn)
    elseif isfloat64
        return @compat parse(Float64,str)
    else
        try
            return @compat parse(Int64,str)
        catch ex
            # its better to ask for forgiveness...
            !isa(ex, OverflowError) && rethrow(ex)
            if is_within_int128(str)
                return @compat parse(Int128,str)
            else
                return @compat parse(BigInt,str)
            end
        end
    end
end

# accum digit predicates
is_char_hex(c::Char) = isdigit(c) || ('a' <= c <= 'f')  || ('A' <= c <= 'F')
is_char_oct(c::Char) = '0' <= c <= '7'
is_char_bin(c::Char) = c === '0' || c === '1'

function accum_digits(ts::TokenStream, pred::Function, c::Char, leading_zero::Bool)
    if !leading_zero && c == '_'
        return (Char[], false)
    end
    charr = Char[]
    while true
        if c == '_'
            skip(ts, 1)
            c = peekchar(ts)
            if !eof(c) && pred(c)
                continue
            else
                skip(ts, -1)
                break
            end
        elseif !eof(c) && pred(c)
            skip(ts, 1)
            push!(charr, c)
            c = peekchar(ts)
            continue
        end
        break
    end
    return (charr, true)
end

fix_uint_neg(neg::Bool, n::Number) = neg? -n : n

function disallow_dot!(ts::TokenStream, charr::Vector{Char}, loc)
    if peekchar(ts) === '.'
        skip(ts, 1)
        if is_dot_opchar(peekchar(ts))
            skip(ts, -1)
        else
            throw(diag(loc, "invalid numeric constant \"$(String(charr)).\""))
        end
    end
end

function read_digits!(ts::TokenStream, pred::Function, charr::Vector{Char}, leading_zero::Bool, loc)
    digits, ok = accum_digits(ts, pred, peekchar(ts), leading_zero)
    ok || throw(diag(loc, "invalid use of '_' in numeric constant"))
    isempty(digits) && return false
    append!(charr, digits)
    return true
end

#TODO: try to remove neg as it is not needed for the lexer
function read_number(ts::TokenStream, leading_dot::Bool, neg::Bool)
    r = startrange(ts)
    charr = Char[]
    pred::Function = isdigit

    leading_zero = false
    is_float32_literal  = false
    is_hexfloat_literal = false

    neg && push!(charr, '-')
    if leading_dot
        push!(charr, '.')
    else
        if peekchar(ts) == '0'
            push!(charr, readchar(ts))
            leading_zero = true
            nc = peekchar(ts)
            if nc === 'x'
                skip(ts, 1); push!(charr, nc)
                leading_zero = false
                pred = is_char_hex
            elseif nc === 'o'
                skip(ts, 1); push!(charr, nc)
                leading_zero = false
                pred = is_char_oct
            elseif nc === 'b'
                skip(ts, 1); push!(charr, nc)
                leading_zero = false
                pred = is_char_bin
            end
        else
            nc = peekchar(ts)
            nc === '.' && (skip(ts, 1); push!(charr, nc))
        end
    end
    read_digits!(ts, pred, charr, leading_zero, makerange(ts, r))
    if peekchar(ts) == '.'
        skip(ts, 1)
        if is_dot_opchar(peekchar(ts))
            skip(ts, -1)
        else
            push!(charr, '.')
            read_digits!(ts, pred, charr, false, makerange(ts, r))
            disallow_dot!(ts, charr, Lexer.makerange(ts, r))
        end
    end
    c = peekchar(ts)
    ispP = c === 'p' || c === 'P'
    if (pred === is_char_hex && ispP) || (c === 'e' || c === 'E' || c === 'f')
        skip(ts, 1)
        nc = peekchar(ts)
        if !eof(nc) && (isdigit(nc) || nc === '+' || nc === '-')
            skip(ts, 1)
            is_float32_literal = c === 'f'
            is_hexfloat_literal = ispP
            push!(charr, c)
            push!(charr, nc)
            read_digits!(ts, pred, charr, false, makerange(ts, r))
            disallow_dot!(ts, charr, makerange(ts, r))
        else
            skip(ts, -1)
        end
    # disallow digits after binary or octal literals, e.g. 0b12
    elseif (pred == is_char_bin || pred == is_char_oct) && !eof(c) && isdigit(c)
        push!(charr, c)
        throw(diag(makerange(ts, r), "invalid numeric constant \"$(String(charr))\""))
    end
    str = String(charr)
    base = pred == is_char_hex ? 16 :
           pred == is_char_oct ? 8  :
           pred == is_char_bin ? 2  : 10
    # for an unsigned literal starting with -,
    # remove the - and parse instead as a call to unary -
    (neg && base != 10 && !is_hexfloat_literal) && (str = str[2:end])
    loc = makerange(ts, r)
    try
        if is_hexfloat_literal
            return make_token((@compat parse(Float64,str)), loc)
        elseif pred == is_char_hex
            return make_token(fix_uint_neg(neg, sized_uint_literal(str, 4)), loc)
        elseif pred == is_char_oct
            return make_token(fix_uint_neg(neg, sized_uint_oct_literal(str)), loc)
        elseif pred == is_char_bin
            return make_token(fix_uint_neg(neg, sized_uint_literal(str, 1)), loc)
        elseif is_float32_literal
            return make_token(convert(Float32, string_to_number(str, loc)), loc)
        else
            return make_token(string_to_number(str, loc), loc)
        end
    catch e
        isa(e, ArgumentError) && throw(diag(loc, e.msg))
        rethrow(e)
    end
end

#============================#
# Skip whitespace / comments
#============================#


"""
`skip_multiline_comment(ts::TokenStream, count::Int)` -> `TokenStream`

Skips multiline comments and maintains a count of the number of enclosed `#= =#` pairs
to allow nesting of multi-line comments.

`count`: the number of read `(#=)` tokens, ie:

```
(#= test =#)  (#= test =#)  (#= #= test =# =#)
 ^              ^               ^        ^
cnt 0           cnt 1         cnt 2    cnt 1
```
"""
function skip_multiline_comment(ts::TokenStream, count::Int)
    start, unterminated = -1, true
    startloc = before(here(ts))
    while !eof(ts)
        c = readchar(ts)
        # if "=#" token, decrement the count.
        # The loop is exited when this count goes below zero.
        if c === '='
            start > 0 || (start = position(ts))
            if peekchar(ts) === '#' && position(ts) != start
                takechar(ts)
                count <= 1 || (count -= 1; continue)
                unterminated = false
                break
            end
            continue
        # if "#=" token increase count
        elseif c === '#' && peekchar(ts) === '='
            count += 1
        end
    end
    if unterminated
        D = diag(here(ts),"incomplete: unterminated multi-line comment #= ... =#")
        diag(D, startloc, "starting here", :note)
        throw(Incomplete(:comment, D))
   end
    return ts
end

"""
`skipcomment(ts::TokenStream)` -> `TokenStream`

Skips to the end if this is a mulitiline comment,
otherwise skips to end of line.
"""
function skipcomment(ts::TokenStream)
    @assert readchar(ts) === '#'
    if peekchar(ts) === '='
        skip_multiline_comment(ts, 1)
    else
        skip_to_eol(ts)
    end
    return ts
end

"""
`skipws_and_comments(ts::TokenStream)` -> `TokenStream`

Skips all whitespace before a comment, upon reaching the comment, if it is a
single line comment, skips to the end of the line, otherwise skip to the end
of the multiline comment block.
"""
function skipws_and_comments(ts::TokenStream)
    while !eof(ts)
        skipws(ts, true)
        peekchar(ts) !== '#' && break
        skipcomment(ts)
    end
    return ts
end

function accum_julia_symbol(ts::TokenStream, c::Char)
    nc, charr = c, Char[]
    while is_identifier_char(nc)
        c, nc = readchar(ts), peekchar(ts)
        # make sure that != is always an operator
        if c === '!' && nc === '='
            skip(ts, -1)
            break
        end
        push!(charr, c)
        eof(nc) && break
    end
    str = normalize_string(String(charr), :NFC)
    sym = Symbol(str)
    return sym === SYM_TRUE ? true : sym === SYM_FALSE ? false : sym
end

#= Token stream methods =#

make_token(::Type{Token},val,start,offset) = Token(val)
make_token(::Type{SourceLocToken},val,start,length) =
    SourceLocToken(val,start,length,0)
@noinline make_token(val, r::Void) = Token(val)
make_token(val, r::SourceRange) = SourceLocToken(val,r)

eof_token(ts) = Token(EOF_CHAR)
eof_token(ts::TokenStream{SourceLocToken}) = SourceLocToken(EOF_CHAR,here(ts))

macro tok(val)
    esc(quote
        loc = position(ts.io)
        v = $val
        make_token(T,v,loc,position(ts.io)-loc)
    end)
end

startrange(ts::TokenStream{SourceLocToken}) = position(ts.io)
makerange(ts::TokenStream{SourceLocToken}, r) = SourceRange(r,position(ts.io)-r,0)
nullrange(ts::TokenStream{SourceLocToken}) = SourceRange(position(ts.io),0,0)
function here(ts::TokenStream{SourceLocToken})
    if ts.putback !== nothing
        SourceRange(Tokens.normalize(√(ts.putback)).offset,1,0)
    elseif ts.lasttoken !== nothing
        SourceRange(Tokens.normalize(√(ts.lasttoken)).offset,1,0)
    else
        SourceRange(position(ts.io),1,0)
    end
end

startrange(ts::TokenStream) = nothing
makerange(ts::TokenStream, r) = nothing
nullrange(ts::TokenStream) = nothing
here(ts::TokenStream) = nothing

function next_token{T}(ts::TokenStream{T}, whitespace_newline::Bool)
    ts.ateof && return eof_token(ts)
    tmp = skipws(ts, whitespace_newline)
    eof(ts) && return eof_token(ts)
    ts.isspace = tmp
    while !eof(ts.io)
        c = peekchar(ts)
        if eof(c)
            ts.ateof = true
            return eof_token(T)
        elseif c === ' ' || c === '\t'
            skip(ts, 1)
            continue
        elseif c == '\r'
            skip(ts, 1)
            peekchar(ts) === '\n' || throw(diag(here(ts),"'\\r' not followed by '\\n' is invalid"))
            continue
        elseif c === '#'
            skipcomment(ts)
            if whitespace_newline && peekchar(ts) === '\n'
                takechar(ts)
            end
            continue
        elseif isnewline(c)
            return @tok readchar(ts)
        elseif is_special_char(c)
            return @tok readchar(ts)
        elseif isdigit(c)
            return read_number(ts, false, false)
        elseif c === '.'
            r = startrange(ts)
            skip(ts, 1)
            nc = peekchar(ts)
            if isdigit(nc)
                return read_number(ts, true, false)
            elseif is_opchar(nc)
                return @tok begin
                    op = read_operator(ts, c, r)
                    if op === :(..) && is_opchar(peekchar(ts))
                        throw(diag(makerange(ts, r),
                            string("invalid operator \"", op, peekchar(ts), "\"")))
                    elseif op == Symbol(".!")
                        throw(diag(makerange(ts, r),
                            string("invalid operator \"", op, "\"")))
                    end
                    op
                end
            end
            return @tok :(.)
        elseif is_opchar(c)
            r = startrange(ts)
            return @tok read_operator(ts, readchar(ts), r)
        elseif is_identifier_start_char(c)
            return @tok accum_julia_symbol(ts, c)
        else
            @assert readchar(ts) === c
            if is_ignorable_char(c)
                throw(diag(here(ts),"invisible character \\u$(hex(c))"))
            else
                throw(diag(here(ts),"invalid character \"$c\""))
            end
        end
    end
    ts.ateof = true
    return eof_token(ts)
end

next_token(ts::TokenStream) = next_token(ts, false)
last_token(ts::TokenStream) = ts.lasttoken
set_token!(ts::TokenStream, t::Union{AbstractToken,Void}) = (ts.lasttoken = t; ts)

function put_back!(ts::TokenStream, t::Union{AbstractToken,Void})
    if ts.putback !== nothing
        error("too many pushed back tokens (internal error)")
    end
    ts.putback = t
    return ts
end

function peek_token{T}(ts::TokenStream{T}, whitespace_newline::Bool)
    ts.ateof && return eof_token(ts)
    if ts.putback !== nothing
        return ts.putback::AbstractToken
    end
    lt = last_token(ts)
    if lt !== nothing
        return lt::AbstractToken
    end
    set_token!(ts, next_token(ts, whitespace_newline))
    return last_token(ts)::AbstractToken
end
peek_token(ts::TokenStream) = peek_token(ts, false)::AbstractToken

function take_token{T}(ts::TokenStream{T})
    ts.ateof && return eof_token(ts)
    if ts.putback !== nothing
        t = ts.putback
        ts.putback = nothing
    else
        t = last_token(ts)
        set_token!(ts, nothing)
    end
    return t
end

function require_token(ts::TokenStream, whitespace_newline::Bool)
    if ts.putback !== nothing
        t = ts.putback
    elseif ts.lasttoken !== nothing
        t = ts.lasttoken
    else
        t = next_token(ts, whitespace_newline)
    end
    eof(t) && throw(Incomplete(:other, diag(here(ts), "incomplete: premature end of input")))
    while isnewline(t)
        take_token(ts)
        t = next_token(ts, whitespace_newline)
    end
    eof(t) && throw(Incomplete(:other, diag(here(ts), "incomplete: premature end of input")))
    ts.putback === nothing && set_token!(ts, t)
    return t
end
require_token(ts::TokenStream) = require_token(ts, false)

end
