using JuliaParser

immutable REPLDiagnostic
    fname::AbstractString
    text::AbstractString
    d::JuliaParser.Parser.Diagnostic
end
function Base.showerror(io::IO, d::REPLDiagnostic, bt)
    println(io,"Syntax")
    print_with_color(:white,io,"")
    JuliaParser.Parser.display_diagnostic(io, d.text, d.d; filename = d.fname)
end

function Core.include(fname::ByteString)
    io = open(fname)
    file = readstring(io)
    ts = Main.JuliaParser.Lexer.TokenStream{Main.JuliaParser.Lexer.SourceLocToken}(file)
    ts.filename = fname
    local result = nothing
    while !Lexer.eof(ts)
        try
            ast = Parser.parse(ts)
        catch e
            !isa(e, Main.JuliaParser.Parser.Diagnostic) && rethrow(e)
            rethrow(REPLDiagnostic(fname, file, e))
        end
        result = ccall(:jl_toplevel_eval_flex, Any, (Any, Cint), ast, 1)
    end
    result
end
