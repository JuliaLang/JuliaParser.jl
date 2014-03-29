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


#XXX: this is wrong
const operators = union(Set([:(~), :(!), :(->), ctranspose_op, transpose_op, vararg_op]), 
			[Set(ops) for ops in ops_by_precedent]...)

const operator_strs  = [string(op) for op in operators]

const reserved_words = Set{Symbol}([:begin, :while, :if, :for, :try, :return,
				    :break, :continue, :function, :macro, :quote,
				    :let, :local, :global, :const, :abstract,
				    :typealias, :type, :bitstype, :immutable, :ccall,
				    :do, :module, :baremodule, :using, :import,
			            :export, :importall])

#= Helper functions =#

function is_assignment(l)
    return length(l) == 2 && first(l) === :(=)
end


function is_assignment_like(l)
    return length(l) == 2 && in(first(l), assignment_ops)
end


function is_kwarg(l)
    return length(l) == 2 && first(l) == :(kw)
end


function is_syntactic_op(op::Symbol)
    return in(op, syntactic_ops)
end


function is_syntactic_unary_op(op::Symbol)
    return in(op, syntactic_unary_ops)
end


function is_dict_literal(l)
    return length(l) == 3 && first(l) === :(=>) 
end


const special_chars = Set{Char}([c for c in  "()[]{},;\"`@"])

function is_special_char(c::Char)
    return in(c, special_chars)
end


function is_newline(c::Char)
    return c === '\n'
end

function is_identifier_char(c::Char)
   return (('A' <= c <= 'Z') ||
           ('a' <= c <= 'z') ||
	   ('0' <= c <= '0') ||
	   ('\uA1' <= c) ||
	   ('\_' === c))
end 

end
