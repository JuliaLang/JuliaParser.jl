using Compat
using Base.Meta
norm_ast(a::Any) = begin
    if isa(a, Expr)
        for (i, arg) in enumerate(a.args)
            a.args[i] = norm_ast(arg)
        end
        if a.head === :line
            return Expr(:line, a.args[1], :none)
        end
        if a.head === :macrocall
            fa = a.args[1]
            if fa === Symbol("@int128_str")
                return @compat parse(Int128,a.args[2])
            elseif fa === Symbol("@uint128_str")
                return @compat parse(UInt128,a.args[2])
            elseif fa === Symbol("@bigint_str")
                return @compat parse(BigInt,a.args[2])
            elseif fa == Symbol("@big_str")
                s = a.args[2]
                n = tryparse(BigInt,s)
                if !isnull(n)
                    return get(n)
                end
                n = tryparse(BigFloat,s)
                if !isnull(n)
                    return isnan(get(n)) ? :NaN : get(n)
                end
                return s
            end
        elseif isexpr(a, :call) && a.args[1] == :- && isa(a.args[2], Number)
            return -a.args[2]
        end
        return a
    elseif isa(a, QuoteNode)
        return Expr(:quote, norm_ast(a.value))
    elseif isa(a, AbstractFloat) && isnan(a)
        return :NaN
    end
    return a
end
