using ..Lexer: SourceRange
using LineNumbers: SourceFile, compute_line

immutable Message
    severity::Symbol
    location::SourceRange
    text::AbstractString
end
immutable Diagnostic
    elements::Vector{Message}
end

function diag(loc, message, severity = :error)
    Diagnostic([Message(severity, loc, message)])
end

function diag(D::Diagnostic, loc, message, severity = :note)
    loc !== nothing && push!(D.elements,Message(severity, loc, message))
end

function display_diagnostic(io::IO, code, diag; filename = "none")
    file = SourceFile(code)
    for message in diag.elements
        if message.location == SourceRange()
            # Don't show notes if they don't have a location
            if message.severity == :note
                continue
            end
            print_with_color(message.severity == :error ? :red : :magenta, io, string(message.severity))
            println(io, ": ", message.text)    
            continue
        end
        offset = message.location.offset
        line = compute_line(file, offset)
        col = offset - file.offsets[line] + 1
        print(io, "$filename:$line:$col " )
        print_with_color(message.severity == :error ? :red : :magenta, io, string(message.severity))
        println(io, ": ", message.text)
        println(io, rstrip(bytestring(file[line])))
        print(io, " "^(col-1))
        print_with_color(:green, io, string('^',"~"^(message.location.length-1)))
        println(io)
    end
end
