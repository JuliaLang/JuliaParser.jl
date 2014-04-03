# Julia Source Parser
module Parser

import ..Lexer

isnewline(tok) = tok === '\n'

function parse(io::IO)
    Lexer.skip_ws_and_comments(io)
    while !eof(io)
        tok = Lexer.next_token(io, nothing)
        if isnewline(tok)
            continue
        end
        break
    end
    return io
end

end
