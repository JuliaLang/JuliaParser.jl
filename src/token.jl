module Tokens

using Compat
using AbstractTrees

import AbstractTrees: children, AbstractShadowTree, make_zip, first_tree, second_tree
import Base: convert

export ¬, ⨳, ⪥, ⤄, √, AbstractToken, Token, SourceLocToken, SourceRange

abstract AbstractToken

const ASTVerbatim = Union{Symbol,String,LineNumberNode,Integer,Void,Char,Bool,AbstractFloat}
const ASTExprs = Union{Expr, QuoteNode, GlobalRef}

# Defensive definitions
Base.isequal(x::AbstractToken, y::ASTVerbatim) = error("Comparing token to raw AST Node")

global √

immutable Token <: AbstractToken
  val::ASTVerbatim
end
val(t::Token) = t.val
val(t::Union{ASTVerbatim,ASTExprs}) = t
√(t::Union{ASTVerbatim, ASTExprs, Expr, Symbol}) = SourceRange()
√(t::Token) = SourceRange()

immutable SourceRange
    offset::UInt32
    length::UInt32
    file::UInt8
end
SourceRange() = SourceRange(-1 % UInt32,-1 % UInt16,-1 % UInt8)

immutable SourceLocToken <: AbstractToken
  val::ASTVerbatim
  loc::SourceRange
end
SourceLocToken(tok::SourceLocToken) = tok
SourceLocToken(val, offset, length, file) =
    SourceLocToken(val, SourceRange(offset, length, file))
SourceLocToken(val) = SourceLocToken(val, SourceRange())
val(t::SourceLocToken) = t.val

const ¬ = val
√(tok::SourceLocToken) = tok.loc
√(r::SourceRange) = r

# Expr and source tree Construction
immutable SourceNode
    loc::Any # SourceRange
    children::Vector
end
Base.copy(x::SourceNode) = SourceNode(x.loc,copy(x.children))
SourceNode(_::Void) = SourceNode(SourceRange(),SourceNode[])
convert(::Type{SourceNode},loc::SourceRange) = SourceNode(loc,SourceNode[])
SourceNode(x::SourceNode) = copy(x)
children(node::SourceNode) = node.children
Base.show(io::IO, node::SourceNode) = print(io, node.loc)

immutable SourceExpr <: AbstractShadowTree
    expr::Any
    loc::SourceNode
end
make_zip(x::SourceExpr) = AbstractTrees.zip_min(children(x.expr),children(x.loc))
first_tree(x::SourceExpr) = x.expr
second_tree(x::SourceExpr) = x.loc
val(expr::SourceExpr) = expr.expr
√(expr::SourceExpr) = expr.loc

normalize(loc::SourceNode) = normalize(loc.loc)
normalize(x) = x
typealias NodeOrRange Union{SourceNode,SourceRange}
function merge(loc1::NodeOrRange, loc2::NodeOrRange)
    loc1, loc2 = normalize(loc1), normalize(loc2)
    loc1 == SourceRange() && return loc2
    loc2 == SourceRange() && return loc1
    @assert loc1.file == loc2.file
    start = min(loc1.offset, loc2.offset)
    stop = max(loc1.offset + loc1.length, loc2.offset + loc2.length)
    res = SourceRange(start, stop-start, loc1.file)
    res
end

const ⤄ = merge
function ⤄(ex::SourceExpr, x::SourceRange)
    SourceExpr(ex.expr,SourceNode(ex.loc.loc ⤄ x, ex.loc.children))
end
⤄(ex::SourceExpr, x::SourceExpr) = ⤄(ex, √x)
⤄(ex::SourceExpr, x::SourceNode) = ⤄(ex, x.loc)
⤄(x::Void, y::SourceRange) = SourceExpr(x,SourceNode(y))
⤄(ex::Union{ASTVerbatim, ASTExprs}, x::NodeOrRange) = SourceExpr(ex,SourceNode(x))
⤄(ex::Union{ASTVerbatim, ASTExprs}, x::Union{SourceExpr,SourceLocToken}) = SourceExpr(ex,SourceNode(normalize(√x)))
⤄(ex::ASTVerbatim, x::Union{SourceExpr,SourceLocToken}) = SourceLocToken(ex,normalize(√x))
⤄(ex::SourceExpr, x::SourceLocToken) = ⤄(ex,√x)
⤄(tok::SourceLocToken, x::SourceRange) = SourceLocToken(tok.val, tok.loc ⤄ x)
⤄(tok::SourceLocToken, x::SourceLocToken) = SourceLocToken(tok.val, tok.loc ⤄ x.loc)
⤄(x::Union{ASTExprs, ASTVerbatim, Token},y::Union{ASTVerbatim,ASTExprs,Void,Token}) = x

function sortedcomplement(of::SourceRange, set)
    complement = SourceRange[]
    lastend = of.offset
    laststart = lastend
    for x in set
        x = normalize(x)
        @assert x.offset >= laststart
        if x.offset >= lastend
            push!(complement,SourceRange(lastend,x.offset-lastend,0))
        end
        laststart = max(laststart, x.offset)
        lastend = max(lastend, x.offset + x.length)
    end
    if lastend < of.offset + of.length
        push!(complement,SourceRange(lastend,of.offset+of.length-lastend-1,0))
    end
    complement
end

⨳(sym::Symbol) = Expr(sym)
⨳(sym::Symbol, args::LineNumberNode...) = Expr(sym::Symbol, args...)
⨳(sym::Symbol, args::Union{ASTVerbatim}...) = Expr(sym,args...)
function ⨳(sym::Symbol, args::Union{Token,ASTVerbatim,ASTExprs}...)
    ex = Expr(sym)
    for arg in args
        push!(ex.args, ¬arg)
    end
    ex
end
function ⨳(sym::Symbol, args::Union{ASTVerbatim,SourceLocToken,LineNumberNode,SourceExpr}...)
    accum = SourceRange()
    children = SourceNode[]
    ex = Expr(sym)
    for arg in args
        loc = √arg
        accum = accum ⤄ loc
        push!(children, SourceNode(loc))
        push!(ex.args, ¬arg)
    end
    SourceExpr(ex,SourceNode(normalize(accum),children))
end
⨳(sym::SourceLocToken,args...) = (SourceExpr(Expr(¬sym), SourceRange()) ⪥ args) ⤄ √sym
⨳(sym::Token,args...) = ⨳(¬sym,args...)

_push!(ex::Expr, t) = push!(ex.args, ¬t)
function _push!(ex::SourceExpr, t)
    push!((¬ex).args, ¬t); push!(ex.loc.children, SourceNode(√t))
end

function expr_append!(ex::SourceExpr, args::Union{Array,Tuple})
    !isempty(args) || return ex
    append!(ex.expr.args,[¬x for x in args])
    append!(ex.loc.children,[SourceNode(√x) for x in args])
    ex = SourceExpr(ex.expr,
        SourceNode(normalize(reduce(⤄,ex.loc.children)),ex.loc.children))
    ex
end
function expr_append!(ex::SourceExpr, new::SourceExpr)
    expr_append!(ex, collect(map(x->SourceExpr(x[1],SourceNode(x[2])),zip(new.expr.args,children(new.loc)))))
end
function expr_append!(ex::Expr, new::SourceExpr)
    expr_append!(SourceExpr(ex, SourceRange()), new)
end
expr_append!(ex::Expr, args::Array) = (for t in args; ex = ex ⤄ t; _push!(ex, t); end; ex)
expr_append!(ex::Expr, args::Tuple) = (for t in args; ex = ex ⤄ t; _push!(ex, t); end; ex)
expr_append!(ex::Expr, new::Expr) = expr_append!(ex, new.args)

const ⪥ = expr_append!

end
