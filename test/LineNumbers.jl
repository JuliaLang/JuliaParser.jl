using JuliaParser
using JuliaParser.LineNumbers
using Base.Test

code = "f() = 1 + 2 - 3\n"
@test SourceFile(code)[1:2] == Vector{UInt8}["f() = 1 + 2 - 3".data, "".data]
data2 = Vector{UInt8}[copy(code.data),[]]
LineBreaking(UInt64(1),SourceFile(code),data2)[7] = 'x'
@test bytestring(data2[1]) == "f() = x + 2 - 3\n"
@test length(SourceFile("f() = 1 + 2 - 3")) == 1
