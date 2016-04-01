function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    code = readstring(Pkg.dir("JuliaParser","src","parser.jl"))
    buf = IOBuffer(code)
    ts = Lexer.TokenStream{Tokens.Token}(buf)
    while !Lexer.eof(ts); Parser.parse(ts); end
    seekstart(buf)
    ts = Lexer.TokenStream{Tokens.SourceLocToken}(buf)
    while !Lexer.eof(ts); Parser.parse(ts); end
    nothing
end
