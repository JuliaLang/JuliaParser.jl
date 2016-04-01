__precompile__()
module JuliaParser

export Parser, Lexer

include("token.jl")
include("diagnostics.jl")
include("lexer.jl")
include("parser.jl")
include("precompile.jl")
_precompile_()

end # module
