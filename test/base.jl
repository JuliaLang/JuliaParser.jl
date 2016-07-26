using JuliaParser
using FactCheck

const Parser = JuliaParser.Parser
const Lexer  = JuliaParser.Lexer

include("ast.jl")
include("util.jl")

# TODO: Julia does not really have a good way to do this so
# this only currently works from source builds.
if length(ARGS) > 0
    const BASEPATH = ARGS[1]
else
    const BASEPATH = abspath(joinpath(JULIA_HOME, "..", ".."))
end

passed = 0
failed = 0
errors = 0

ptime = 0.0
btime = 0.0

function testall(srcdir::AbstractString)
    global passed
    global failed
    global errors
    global ptime
    global btime

    dirs, files = [], []

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
            write(buf, open(readstring, jlpath))
            write(buf, "\nend")

            src = String(buf)
            try
                tic()
                past = Parser.parse(src)
                t1 = toq()

                tic()
                bast = Base.parse(src)
                t2 = toq()

                if norm_ast(past) == norm_ast(bast)
                    println(green("OK:     $fname"))
                    passed += 1
                    ptime += t1
                    btime += t2
                else
                    println(red("FAILED: $fname"))
                    failed += 1
                end
            catch ex
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

if isdir(BASEPATH) && isdir(joinpath(BASEPATH, "base"))
    testall(joinpath(BASEPATH, "examples"))
    testall(joinpath(BASEPATH, "test"))
    testall(joinpath(BASEPATH, "base"))
else
    warn("""
Could not find julia base sources in $BASEPATH,
perhaps you are using a Julia not built from source?""")
end

println("\n"^2)
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

#exit(failed + errors)
