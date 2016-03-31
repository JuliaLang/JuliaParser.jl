using JuliaParser
using JuliaParser.Lexer: ¬

immutable REPLDiagnostic
    fname::AbstractString
    text::AbstractString
    d::Any
end

extract_diag(d) = isa(d, Main.JuliaParser.Diagnostics.Incomplete) ? d.d : d

function Base.showerror(io::IO, d::REPLDiagnostic, bt)
    print_with_color(:white,io,"")
    JuliaParser.Diagnostics.display_diagnostic(io, d.text, extract_diag(d.d); filename = d.fname)
end
function Base.showerror(io::IO, d::REPLDiagnostic)
    print_with_color(:white,io,"")
    JuliaParser.Diagnostics.display_diagnostic(io, d.text, extract_diag(d.d); filename = d.fname)
end
function Base.REPL.display_error(io::IO, d::REPLDiagnostic, bt)
    print_with_color(:white,io,"")
    JuliaParser.Diagnostics.display_diagnostic(io, d.text, extract_diag(d.d); filename = d.fname)
end

function Core.include(fname::ByteString)
    io = open(fname)
    file = readstring(io)
    ts = Main.JuliaParser.Lexer.TokenStream{Main.JuliaParser.Lexer.SourceLocToken}(file)
    ts.filename = fname
    local result = nothing
    while !Lexer.eof(ts)
        ast = try
            Parser.parse(ts)
        catch e
            !isa(e, Main.JuliaParser.Diagnostics.Diagnostic) && rethrow(e)
            rethrow(REPLDiagnostic(fname, file, e))
        end
        result = ccall(:jl_toplevel_eval_flex, Any, (Any, Cint), ¬ast, 1)
    end
    result
end

function Base.parse(str::AbstractString, pos::Int; greedy::Bool=true, raise::Bool=true)
    io = IOBuffer(str)
    seek(io, pos-1)
    ts = Main.JuliaParser.Lexer.TokenStream{Main.JuliaParser.Lexer.SourceLocToken}(io)
    local result = nothing
    ast = try
        Parser.parse(ts)
    catch e
        !isa(e, Main.JuliaParser.Diagnostics.AbstractDiagnostic) && rethrow(e)
        return Expr(isa(e, Main.JuliaParser.Diagnostics.Incomplete) ? :incomplete :
            :error, REPLDiagnostic("REPL", str, e)), position(io) + 1
    end
    ¬ast, position(io) + 1
end

function Base.parse_input_line(code::ByteString)
    ts = Main.JuliaParser.Lexer.TokenStream{Main.JuliaParser.Lexer.SourceLocToken}(code)
    ts.filename = "REPL"
    local result = nothing
    ast = try
        Parser.parse(ts)
    catch e
        !isa(e, Main.JuliaParser.Diagnostics.AbstractDiagnostic) && rethrow(e)
        return Expr(isa(e, Main.JuliaParser.Diagnostics.Incomplete) ? :incomplete :
            :error, REPLDiagnostic("REPL", code, e))
    end
    ¬ast
end

function Base.incomplete_tag(e::Expr)
    isa(e.args[1], REPLDiagnostic) || return :none
    isa(e.args[1].d, Main.JuliaParser.Diagnostics.Incomplete) || return :none
    e.args[1].d.tag
end
