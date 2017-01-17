module JuliaParserHooks
using JuliaParser
using JuliaParser.Lexer: ¬
using JuliaParser: Lexer

# mute override warnings
if ccall(:jl_generating_output, Cint, ()) == 0
    ORIG_STDERR = STDERR
    redirect_stderr()
end

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

function Core.include(fname::String)
    io = open(fname)
    file = readstring(io)
    ts = Lexer.TokenStream{Lexer.SourceLocToken}(file)
    ts.filename = fname
    local result = nothing
    while !Lexer.eof(ts)
        ast = try
            Parser.parse(ts)
        catch e
            !isa(e, Main.JuliaParser.Diagnostics.AbstractDiagnostic) && rethrow(e)
            rethrow(REPLDiagnostic(fname, file, e))
        end
        result = ccall(:jl_toplevel_eval, Any, (Any,), ¬ast)
    end
    result
end

function Base.include_string(code, filename = "string")
    ts = Lexer.TokenStream{Lexer.SourceLocToken}(code)
    ts.filename = filename
    local result = nothing
    while !Lexer.eof(ts)
        ast = try
            Parser.parse(ts)
        catch e
            !isa(e, Main.JuliaParser.Diagnostics.AbstractDiagnostic) && rethrow(e)
            rethrow(REPLDiagnostic(fname, file, e))
        end
        result = ccall(:jl_toplevel_eval, Any, (Any,), ¬ast)
    end
    result
end

# Needs to be replaced due to # 265
function Base.include_from_node1(_path::AbstractString)
    path, prev = Base._include_dependency(_path)
    tls = task_local_storage()
    tls[:SOURCE_PATH] = path
    local result
    try
        if myid()==1
            # sleep a bit to process file requests from other nodes
            nprocs()>1 && sleep(0.005)
            result = Core.include(path)
            nprocs()>1 && sleep(0.005)
        else
            result = include_string(remotecall_fetch(readstring, 1, path), path)
        end
    finally
        if prev === nothing
            delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
    result
end

function Base.parse(str::AbstractString, pos::Int; greedy::Bool=true, raise::Bool=true)
    io = IOBuffer(str)
    seek(io, pos-1)
    ts = Main.JuliaParser.Lexer.TokenStream{Main.JuliaParser.Lexer.SourceLocToken}(io)
    local result = nothing
    ast = try
        Parser.parse(ts; production = greedy ? Parser.parse_stmts : Parser.parse_atom)
    catch e
        !isa(e, Main.JuliaParser.Diagnostics.AbstractDiagnostic) && rethrow(e)
        e = REPLDiagnostic("REPL", str, e)
        raise && rethrow(e)
        return Expr(isa(e, Main.JuliaParser.Diagnostics.Incomplete) ? :incomplete :
            :error, e), position(io) + 1
    end
    ¬ast, position(io) + 1
end

function Base.parse_input_line(code::String; filename::String="none")
    ts = Lexer.TokenStream{Main.JuliaParser.Lexer.SourceLocToken}(code)
    ts.filename = filename
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

if ccall(:jl_generating_output, Cint, ()) == 0
    REDIRECTED_STDERR = STDERR
    redirect_stderr(ORIG_STDERR)
end

end
