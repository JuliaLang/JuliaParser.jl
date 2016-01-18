#!/usr/bin/env julia
# Simple REPL to test the parser and compare it against the base
# julia parser - ^R reloads the package, ^S switches between base
# and JuliaParser parsers. Entered lines are parsed and printed 
# in tree forms.

using AbstractTrees
using JuliaParser

eval(Base,:(have_color = true))

import AbstractTrees: children, printnode

function children(x::Expr)
    x.args
end

function printnode(io::IO, x::Expr, color = :default)
    print_with_color(:red, io, string(':', x.head))
end

function printnode(io::IO, x::LineNumberNode)
    print_with_color(:blue, io, string(x))
end

function printnode(io::IO, x::QuoteNode)
    print_with_color(:green, io, string(x))
end

import Base: LineEdit, REPL

function RunShell()
    global hp
    prompt = "parse > "

    isBase = false

    # Setup debug panel
    panel = LineEdit.Prompt(prompt;
        prompt_prefix=()->isBase?"\e[38;5;107m":"\e[38;5;166m",
        prompt_suffix=Base.text_colors[:white],
        on_enter = s->true)

    panel.hist = REPL.REPLHistoryProvider(Dict{Symbol,Any}(:debug => panel))

    panel.on_done = (s,buf,ok)->begin
        line = takebuf_string(buf)
        if !ok || strip(line) == "q"
            LineEdit.transition(s, :abort)
        end
        try
          show(STDOUT,Tree(isBase ? Base.parse(line) :
            eval(:(Main.JuliaParser.Parser.parse($line)))))
        catch err
          REPL.display_error(STDERR, err, Base.catch_backtrace())
          REPL.println(STDERR); REPL.println(STDERR)
        end
        LineEdit.reset_state(s)
        println()
    end
    
    hp = REPL.REPLHistoryProvider(Dict{Symbol,Any}(:parse => panel))
    panel.hist = hp
    
    REPL.history_reset_state(hp)

    extra_keymap = Dict{Any,Any}(
      "^R" => (s,o...)->(print("Reloading... "); reload("JuliaParser");
        println("Done!"); LineEdit.refresh_line(s)),
      "^S" => (s,o...)->(isBase = !isBase; LineEdit.refresh_line(s))
    );
    search_prompt, skeymap = LineEdit.setup_prefix_keymap(hp, panel)

    b = Dict{Any,Any}[extra_keymap, skeymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    panel.keymap_dict = LineEdit.keymap(b)
    
    t = Base.Terminals.TTYTerminal("xterm", STDIN, STDOUT, STDERR)
    Base.REPL.run_interface(t, LineEdit.ModalInterface([panel,search_prompt]))
end
RunShell()
