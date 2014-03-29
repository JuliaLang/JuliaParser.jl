# Lexer for Julia

module Lexer

const ops_by_precedent =  {[:(=),   :(:=),   :(+=),   :(-=),  :(*=),
			    :(/=),  :(//=),  :(.//=), :(.*=), :(./=),
			    :(\=),  :(.\=),  :(^=),   :(.^=), :(%=),
			    :(.%=), :(|=),   :(&=),   :($=),  :(=>),
			    :(<<=), :(>>=),  :(>>>=), :(~),   :(.+=),
			    :(.-=)],
			   [:(?)],
			   [:(||)],
			   [:(&&)],
			   [:(--), :(-->)],
			   [:(>),   :(<),   :(>=),  :(<=),  :(==), 
			    :(===), :(!=),  :(!==), :(.>),  :(.<),
			    :(.>=), :(.<=), :(.==), :(.!=), :(.=),
			    :(.!),  :(<:),  :(>:)],
			   [:(|>), :(<|)],
			   [:(:),  :(..)],
			   [:(+),  :(-),  :(.+),  :(.-),  :(|),   :($)],
			   [:(<<), :(>>), :(>>>), :(.<<), :(.>>), :(.>>>)],
			   [:(*),  :(/),  :(./),  :(%),   :(.%),  :(&), :(.*), :(\), :(.\)],
			   [:(/), :(./)],
			   [:(^),  :(.^)],
			   [:(::)],
			   [:(.)]}

precedent_ops(n::Integer) = ops_by_precedent[n]::Vector{Symbol}

const assignment_ops = ops_by_precedent[1]::Vector{Symbol}

const unary_ops = Set{Symbol}([:(+), :(-), :(!), :(~), :(<:), :(>:)])

const unary_and_binary = Set{Symbol}([:(+), :(-), :($), :(&), :(~)])

# operators are special forms, not function names
const syntactic_ops = Set{Symbol}([:(=),   :(:=),  :(+=),   :(-=),  :(*=), 
				   :(/=),  :(//=), :(./=),  :(.*=), :(./=),  
				   :(\=),  :(.\=), :(^=),   :(.^=), :(%=),
				   :(.%=), :(|=),  :(&=),   :($=),  :(=>),
				   :(<<=), :(>>=), :(>>>=), :(->),  :(-->),
				   :(\),   :(&&),  :(::),   :(.),   :(...),
				   :(.+=), :(.-=)])

const syntactic_unary_ops = Set{Symbol}([:($), :(&)])


const transpose_op  = symbol(".'")
const ctranspose_op = symbol("'")
const vararg_op     = :(...)

const operators = union(Set([:(~), :(!), :(->), ctranspose_op, transpose_op, vararg_op]), 
			[Set(ops) for ops in ops_by_precedent]...)


const reserved_words = Set{Symbol}([:begin, :while, :if, :for, :try, :return,
				    :break, :continue, :function, :macro, :quote,
				    :let, :local, :global, :const, :abstract,
				    :typealias, :type, :bitstype, :immutable, :ccall,
				    :do, :module, :baremodule, :using, :import,
			            :export, :importall])

#= Helper functions =#

is_assignment(l) = length(l) == 2 && first(l) === :(=)

is_assignment_like(l) = length(l) == 2 && in(first(l), assignment_ops)

is_kwarg(l) = length(l) == 2 && first(l) == :(kw)

is_syntactic_op(op::Symbol) = in(op, syntactic_ops)

is_syntactic_unary_op(op::Symbol) = in(op, syntactic_unary_ops)

is_dict_literal(l) = length(l) == 3 && first(l) === :(=>) 

const is_special_char = 
    let chars = Set{Char}([c for c in  "()[]{},;\"`@"])
	is_special(c::Char)  = in(c, chars)
    end

is_newline(c::Char) = return c === '\n'

function is_identifier_char(c::Char)
   return (('A' <= c <= 'Z') ||
           ('a' <= c <= 'z') ||
	   ('0' <= c <= '0') ||
	   ('\uA1' <= c) ||
	   ('\_' === c))
end 

#= characters that can be in an operator =#
const operator_chars  = union([Set(string(op)) for op in operators]...)

is_opchar(c::Char) = in(c, operator_chars)

#= characters that can follow a . in an operator =#
const is_dot_opchar =
    let chars = Set(".*^/\\+-'<>!=%")
        is_dot_char(c::Char) = in(c, chars)
    end

is_operator(o::Symbol) = in(o, operators)

end
