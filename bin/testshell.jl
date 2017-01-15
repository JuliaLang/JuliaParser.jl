#!/usr/bin/env julia
# Simple REPL to test the parser and compare it against the base
# julia parser - ^R reloads the package, ^S switches between base
# and JuliaParser parsers. Entered lines are parsed and printed
# in tree forms.

using AbstractTrees
using JuliaParser
using TerminalUI
using VT100
using Colors

dosl = false
if length(ARGS) == 1 && ARGS[1] == "--sl"
    dosl = true
end

eval(Base,:(have_color = true))

import AbstractTrees: children, printnode

function printnode(io::IO, x::Expr, color = :default)
    print_with_color(:red, io, string(':', x.head))
end

function printnode(io::IO, x::LineNumberNode)
    print_with_color(:blue, io, string(x))
end

function printnode(io::IO, x::QuoteNode)
    print_with_color(:green, io, string(x))
end

function printnode(io::IO, x::GlobalRef)
    print_with_color(:yellow, io, string(x))
end

import Base: LineEdit, REPL

function reload_widget()
    eval(Main,:(module LocWidget
        include($(joinpath(dirname(@__FILE__),"..","src","interactiveutil.jl")))
    end))
end
reload_widget()

function RunShell()
    global hp
    prompt = "parse > "

    isBase = false

    # Setup debug panel
    panel = LineEdit.Prompt(prompt;
        prompt_prefix=()->isBase?"\e[38;5;107m":"\e[38;5;166m",
        prompt_suffix=Base.text_colors[:white],
        on_enter = dosl ? (s)->true : Base.REPL.return_callback)

    panel.hist = REPL.REPLHistoryProvider(Dict{Symbol,Any}(:debug => panel))

    panel.on_done = (s,buf,ok)->begin
        line = String(take!(buf))
        if !ok || strip(line) == "q"
            LineEdit.transition(s, :abort)
        end
        try
            if isBase
                show(STDOUT, Tree(Base.parse(line)))
            elseif dosl
                eval(quote
                    ts = Main.JuliaParser.Lexer.TokenStream{Main.JuliaParser.Lexer.SourceLocToken}($line)
                    try
                        res = Main.JuliaParser.Parser.parse(ts)
                        if res != nothing
                            show(STDOUT,Tree(res.expr))
                            println($line)
                            w = Main.LocWidget.create_widget(res.loc,$(line))
                            TerminalUI.print_snapshot(TerminalUI.InlineDialog(w,
                                Base.Terminals.TTYTerminal("xterm", STDIN, STDOUT, STDERR)
                                ))
                        end
                    catch e
                        !isa(e, Main.JuliaParser.Diagnostics.Diagnostic) && rethrow(e)
                        Main.JuliaParser.Diagnostics.display_diagnostic(STDOUT, $line, e)
                    end
                end)
            else
                show(STDOUT,Tree(eval(:(Main.JuliaParser.Parser.parse($line)))))
            end
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
        reload_widget();
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
