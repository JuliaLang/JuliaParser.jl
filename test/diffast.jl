#!/usr/bin/julia
import JuliaParser.Parser

include("ast.jl")

const src = IOBuffer()

write(src, "begin\n")
write(src, open(readall, ARGS[1]))
write(src, "\nend")

const jlsrc = bytestring(src)

let tmp1 = tempname(), tmp2 = tempname()

    local ast1::Expr
    open("$tmp1", "w+") do io
        ast1 = Parser.parse(jlsrc) |> without_linenums
        Meta.show_sexpr(io, ast1)
    end
    
    local ast2::Expr
    open("$tmp2", "w+") do io
        ast2 = parse(jlsrc) |> without_linenums
        Meta.show_sexpr(io, ast2)
    end

    if ast1 == ast2
        print_with_color(:green, "OK\n")
    else
        print_with_color(:red, "FAILED\n")
    end
    
    run(`gvimdiff $tmp1 $tmp2`)
end
