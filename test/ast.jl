using Compat
norm_ast(ex::Expr) = begin
    args = Any[]
    for a in ex.args
        if isa(a, Expr)
            if a.head === :line
                push!(args, Expr(:line, a.args[1], :none))
                continue
            end
            if a.head === :macrocall
                fa = a.args[1]
                if fa === symbol("@int128_str")
                    push!(args, (@compat parse(Int128,a.args[2])))
                    continue
                elseif fa === symbol("@uint128_str")
                    push!(args, (@compat parse(UInt128,a.args[2])))
                    continue
                elseif fa === symbol("@bigint_str")
                    push!(args, (@compat BigInt,a.args[2]))
                    continue
                end
            end
            push!(args, norm_ast(a))
        elseif isa(a, QuoteNode)
            push!(args, Expr(:quote, norm_ast(a.value)))
        else
            push!(args, norm_ast(a))
        end
    end
    nex = Expr(ex.head); nex.args = args
    return nex
end

norm_ast(ex::QuoteNode) = Expr(:quote, norm_ast(ex.value))
norm_ast(ex) = ex
