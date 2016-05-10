using JuliaParser

include(joinpath(dirname(@__FILE__),"../src/interactiveutil.jl"))

function dotest(input)
lines = split(input,'\n')
code = String(lines[1])
ts = Lexer.TokenStream{Lexer.SourceLocToken}(code)
res = Parser.parse(ts)
textrep = create_textrep(res.loc, code)
if strip(input) != strip(textrep)
    println("GOT:")
    println(textrep)
    error()
end
end

"""
f()
x
xxx
""" |> dotest

"""
t...
x
xxxx
""" |> dotest

"""
for i = 1:10; end
        x xx
    x   xxxx
    xxxxxxxx  xxx
xxxxxxxxxxxxxxxxx
""" |> dotest
