using JuliaParser
using JuliaParser.LineNumbers
using Base.Test

code = "f() = 1 + 2 - 3\n"
@test SourceFile(code)[1:2] == Vector{UInt8}[Vector{UInt8}("f() = 1 + 2 - 3"), Vector{UInt8}("")]
data2 = Vector{UInt8}[Vector{UInt8}(code),[]]
LineBreaking(UInt64(1),SourceFile(code),data2)[7] = 'x'
@test String(data2[1]) == "f() = x + 2 - 3\n"
@test length(SourceFile("f() = 1 + 2 - 3")) == 1
