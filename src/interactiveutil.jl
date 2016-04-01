using AbstractTrees
using VT100
using TerminalUI
using Colors
using JuliaParser: LineNumbers
import JuliaParser: Lexer, Tokens
import JuliaParser.Tokens: ⤄

function do_transform(node,code,create_template,create_portal,from_line)
    max = Tokens.normalize(reduce(⤄,PostOrderDFS(node)))
    file = SourceFile(code)
    startline = compute_line(file, max.offset)
    stopline = compute_line(file, max.offset+max.length)
    nlines = stopline-startline+1
    lines = create_template(file[startline:stopline])
    # First by display line, then by source line, to be reconfigured later
    displaylines = Any[]
    ranges = collect(IndEnumerate(StatelessBFS(node)))
    callback = create_portal(ranges)
    for (i,(ind,range)) in enumerate(ranges)
        if length(displaylines) < length(ind) + 1
            push!(displaylines,deepcopy(lines))
        end
        active_displayline = displaylines[length(ind) + 1]
        range = range.loc
        range = (range.offset):(range.offset + range.length - 1)
        LineBreaking(convert(UInt64,file.offsets[startline]),file,active_displayline)[range] =
          callback()
    end
    # Assemble the cell display
    celllines = Any[]
    for i = startline:stopline
        content = file[i]
        push!(celllines,from_line(content))
        for j = length(displaylines):-1:1
            push!(celllines,displaylines[j][1+i-compute_line(file,max.offset+1)])
        end
    end
    celllines
end

function create_textrep(node, code)
    create_template(lines) = [[Char(' ') for _ in 1:length(line)] for line in lines]
    create_portal(ranges) = ()->'x'
    from_line(content) = [Char(c) for c in content]
    celllines = do_transform(node, code, create_template, create_portal, from_line)
    join(map(x->rstrip(join(x,"")),celllines),'\n')
end

function create_widget(node, code)
    create_template(lines) =
        [Cell[Cell(' ') for i = 1:length(line)] for line in lines]
    function create_portal(ranges)
        n = length(ranges)
        colors = distinguishable_colors(n+1,RGB(0,0,0))[2:end]
        ()->Cell(TerminalUI.dummycell(:white,pop!(colors)), content = ' ')
    end
    from_line(content) = [Cell(TerminalUI.dummycell(:default,:default),
      content = Char(c)) for c in content]
    celllines = do_transform(node, code, create_template, create_portal, from_line)
    TerminalUI.CellDisplay(celllines)
end
