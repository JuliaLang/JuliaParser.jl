using JuliaParser
using FactCheck

const Parser = JuliaParser.Parser
const Lexer  = JuliaParser.Lexer

const TokenStream = JuliaParser.Parser.TokenStream 

include("ast.jl")

const BASEPATH = "/home/jake/Julia/julia"

const RED     = "\x1b[31m"
const GREEN   = "\x1b[32m"
const BOLD    = "\x1b[1m"
const DEFAULT = "\x1b[0m"

colored(s::String, color) = string(color, s, DEFAULT)

red(s::String)   = colored(s, RED)
green(s::String) = colored(s, GREEN)
bold(s::String)  = colored(s, BOLD) 

passed = 0
failed = 0
errors = 0

ptime = 0.0
btime = 0.0

function testall(srcdir::String)
    global passed
    global failed
    global errors
    global ptime
    global btime 

    dirs  = {}
    files = {}

    for fname in sort(readdir(srcdir))
        path = joinpath(srcdir, fname) 
        if isdir(path)
            push!(dirs, path)
            continue
        end
        _, ext = splitext(fname)
        if ext == ".jl"
            push!(files, path)
        end
    end

    if !isempty(files)
        println(bold("test $srcdir"))
        
        for jlpath in files 
            fname = splitdir(jlpath)[end]
            buf = IOBuffer()
            write(buf, "begin\n")
            write(buf, open(readall, jlpath))
            write(buf, "\nend")
            
            src = bytestring(buf)
            try
                tic() 
                past = Parser.parse(src)
                t1 = toq()
                
                tic()
                bast = Base.parse(src)
                t2 = toq()

                if without_linenums(past) == without_linenums(bast)
                    println(green("OK:     $fname"))
                    passed += 1
                    ptime += t1
                    btime += t2
                else
                    println(red("FAILED: $fname"))
                    failed += 1
                end
            catch
                println(bold(red("ERROR:  $fname")))
                errors += 1
            end
        end
        println()
    end
    for dir in dirs
        testall(dir)
    end
end

testall(joinpath(BASEPATH, "base"))
testall(joinpath(BASEPATH, "test"))

println()
println()
println(bold("TOTAL: $(passed + failed + errors)")) 
println(green("Passed:\t$passed"))
println(red("Failed:\t$failed"))
println(red("Errors:\t$errors"))
println()

pct = ptime / btime * 100.0

if pct >= 100
    pctstr = @sprintf("%0.2f", pct-100)
    println(bold("Parser is ~$pctstr% slower than base"))
else
    pctstr = @sprintf("%0.2f", 100-pct)
    println(bold("Parser is ~$pctstr% faster than base"))
end

exit(failed + errors)
