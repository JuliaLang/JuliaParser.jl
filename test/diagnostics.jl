using JuliaParser
using Base.Test

function do_diag_test(text)
    ts = Lexer.TokenStream{Lexer.SourceLocToken}(text)
    ts.filename = "test"
    try
        Parser.parse(ts)
        error("Should have failed")
    catch err
        !isa(err, Parser.Diagnostic) && rethrow(err)
        return err
    end
end

let diags = do_diag_test("""
      foo(a,b
      bar(
  """)
#  none:1:6 error: Expected ')' or ','
#  abc(d
#       ^
#  none:1:4 note: to match '(' here
#  abc(d
#     ^
    @test length(diags.elements) == 2
    @test diags.elements[1].severity == :error
    @test diags.elements[2].severity == :note
end

let diags = do_diag_test("a && && b")
    @test diags.elements[1].severity == :error
end

let diags = do_diag_test("print x")
    @test diags.elements[1].severity == :error
end
