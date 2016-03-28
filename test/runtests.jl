using JuliaParser
using FactCheck

include("lexer.jl")
include("parser.jl")
include("diagnostics.jl")
#include("allsrc.jl")

exitstatus()
