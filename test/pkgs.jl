using JuliaParser
using FactCheck

const Parser = JuliaParser.Parser
const Lexer  = JuliaParser.Lexer

include("ast.jl")
include("util.jl")

const PKGDIR = Pkg.dir()

passed = {}
failed = {}
errors = {}

ptime = 0.0
btime = 0.0

function testall(srcdir::String)
    global passed
    global failed
    global errors
    global ptime
    global btime 

    dirs, files = {}, {}

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
            
            local bast::Expr
            local past::Expr
            
            local t1::Float64
            local t2::Float64
            
            try
                tic()
                bast = Base.parse(src)
                t2 = toq()
            catch
                warn("Base cannot parse file $jlpath")
                continue
            end

            try
                tic() 
                past = Parser.parse(src)
                t1 = toq()
                 
                if without_linenums(past) == without_linenums(bast)
                    println(green("OK:     $fname"))
                    push!(passed, jlpath)
                    ptime += t1
                    btime += t2
                else
                    println(red("FAILED: $fname"))
                    push!(failed, jlpath)
                end
            catch ex
                if isa(ex, ErrorException) && ex.msg == "deprecated syntax arr[i:]"
                    continue
                end
                println(bold(red("ERROR:  $fname")))
                push!(errors, jlpath)
            end
        end

        println()
    end
    for dir in dirs
        testall(dir)
    end
end


for pkg in Pkg.available()
    pkgpath = joinpath(PKGDIR, pkg)
    if !isdir(pkgpath)
        continue
    end
    testall(pkgpath)
end

npassed, nfailed, nerrors = length(passed), length(failed), length(errors)

println("\n"^2)
println(bold("TOTAL: $(npassed + nfailed + nerrors)")) 
println(green("Passed:\t$npassed"))
println(red("Failed:\t$nfailed"))
for path in failed
    println(red("\t" * path))
end
println(red("Errors:\t$nerrors"))
for path in errors
    println(red("\t" * path))
end
println()

pct = ptime / btime * 100.0

if pct >= 100
    pctstr = @sprintf("%0.2f", pct-100)
    println(bold("Parser is ~$pctstr% slower than base"))
else
    pctstr = @sprintf("%0.2f", 100-pct)
    println(bold("Parser is ~$pctstr% faster than base"))
end

exit(nfailed + nerrors)
