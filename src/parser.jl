# Julia Source Parser
module Parser

import ..Lexer

typealias Token Union(Symbol, Char, Number, Nothing)

type TokenStream
    io::IO
    tokens::Vector{Token}
    lasttoken::Token
    putback::Token
    isspace::Bool
end

TokenStream(io::IO) = TokenStream(io, {}, nothing, false, false) 

Base.isspace(ts::TokenStream) = ts.isspace
Base.eof(ts::TokenStream) = eof(ts.io)

function set_token!(ts::TokenStream, t::Token)
    push!(ts.tokens, t)
    return ts
end

function last_token(ts::TokenStream)
    if isempty(ts.tokens)
        return nothing
    end
    return ts.tokens[end]
end 

function put_back!(ts::TokenStream, t::Token)
    if ts.putback != nothing
        error("too many pushed back tokens (internal error)")
    end
    ts.putback = t
    return ts
end

function peek_token(ts::TokenStream)
    local t::Token
    if ts.putback != nothing
        return ts.putback
    end
    lt = last_token(ts)
    if lt != nothing
        return lt
    end
    set_token!(ts, next_token(ts))
    return last_token(ts)
end
        
isnewline(t::Token) = t == '\n'

function take_token(ts::TokenStream)
    local t::Token 
    if ts.putback != nothing
        t = putback
        ts.putback = nothing
    else
        t = last_token(ts)
        set_token!(ts, nothing)
    end
    return t
end

function require_token(ts::TokenStream)
    local t::Token
    if ts.putback != nothing
        t = ts.putback
    elseif last_token(ts) != nothing
        t = ts.lasttoken
    else
        t = next_token(ts)
    end
    eof(t) && error("incomplete: premature end of input")
    if isnewline(t)
        take_token(ts)
        return require_token(ts)
    end 
    if ts.putback == nothing
        set_token!(ts, t)
    end
    return t
end

const sym_else    = symbol("else")
const sym_elseif  = symbol("elseif")
const sym_catch   = symbol("catch")
const sym_finally = symbol("finally")

const invalid_initial_token = let
    invalid = Set({')', ']', '}', 
                   sym_else, sym_elseif, sym_catch, sym_finally}) 
    invalid_initial_token(t) = t in invalid
end

const closing_token = let
    closing = Set({',', ')', ']', '}', ';',
                   sym_else, sym_elseif, sym_catch, sym_finally})
    closing_token(t) = t in closing
end

function parse_nary(io::IO)
    invalid_initial_token(require_token(io))
end

function parse(ts::TokenStream)
    Lexer.skip_ws_and_comments(ts.io)
    while !eof(ts)
        t = Lexer.next_token(ts.io, nothing)
        if isnewline(t)
            continue
        end
        break
    end
    return ts
end

function parse_range(ts::TokenStream)
   ex = parse_expr(ts)
   isfirst = first(ex) == true

   while !eof(ts)
       tok = peek_token(ts)
       spc = isspace(ts)
   end
end

end
