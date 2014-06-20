without_linenums(ex::Expr) = begin
    args = {}
    for a in ex.args
        if isa(a, Expr)
            a.head === :line && continue
            if a.head === :macrocall
                fa = a.args[1]
                if fa === symbol("@int128_str")
                    push!(args, int128(a.args[2]))
                    continue
                elseif fa === symbol("@uint128_str")
                    push!(args, uint128(a.args[2]))
                    continue
                elseif fa === symbol("@bigint_str")
                    push!(args, BigInt(a.args[2]))
                    continue
                end
            end
            push!(args, without_linenums(a))
        elseif isa(a, QuoteNode)
            push!(args, Expr(:quote, without_linenums(a.value)))
        else
            isa(a, LineNumberNode) && continue
            push!(args, without_linenums(a))
        end
    end
    nex = Expr(ex.head); nex.args = args
    return nex
end

without_linenums(ex::QuoteNode) = Expr(:quote, without_linenums(ex.value))
without_linenums(ex) = ex
