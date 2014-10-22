module JuliaParser

export Parser, Lexer, MyExpr

type MyExpr
    head::Symbol
    args::Vector{Any}
    typ::Any
end

MyExpr(head::Symbol) = MyExpr(head, [], Any)
MyExpr(head::Symbol, args...) = MyExpr(head, Any[args...], Any)

Base.(:(==))(ex1::MyExpr, ex2::Expr) = (ex1.head == ex2.head) &&
                                       (ex1.args == ex2.args) &&
                                       (ex1.typ  == ex2.typ)
Base.(:(==))(ex1::Expr, ex2::MyExpr) = ex2 == ex1
Base.eval(ex::MyExpr) = Base.eval(Expr(ex.head, ex.args))

include("lexer.jl")
include("parser.jl") 

end # module
