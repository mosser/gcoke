%%%%
%% This file is part of gCoKe [ http://www.gcoke.org ]
%%
%% Copyright (C) 2010-  Sebastien Mosser
%%
%% gCoKe is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as 
%% published by the Free Software Foundation; either version 2 of 
%% the License, or (at your option) any later version.
%%
%% gCoKe is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public 
%% License along with gCoKe; if not, write to the Free Software Foundation,
%% Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% @author   Main    Sebastien Mosser  [ sm@gcoke.org ]
%%%%

:- module(algorithm, []).

%%%%
%% Algorithm declaration
%%%%

:- dynamic description/3.
declare(Name, Predicate, Signature) :- 
	assert(description(Name, Predicate, Signature)).

%%%%
%%  Algorithm signature verification
%%%%

verify(Algo, _) :- 
 	\+ description(Algo, _, _), writef('Unknown algorithm: %w.', [Algo]), fail.
verify(Algo, Call_values) :- 
 	description(Algo, _, Signature), 
 	verify_parameters(Algo, Signature, Call_values).

verify_parameters(Algo, Signature, Call_values) :- 
 	findall(E, check_param_error(Signature, Call_values, E), Err_list),
 	(Err_list = [] -> true ; writef('%w: %w',[Algo, Err_list]), fail).

check_param_error(Signature, Call_values, is_extra(P)) :- 
 	member([P,_], Call_values), 
 	\+ (member(in(P,_), Signature) | member(out(P), Signature)).
check_param_error(Signature, Call_values, is_missing(P)) :- 
	member(in(P,_), Signature), \+ member([P,_],Call_values).
check_param_error(Signature, Call_values, has_wrong_type(P)) :- 
	member(in(P,Pred), Signature), member([P,Value], Call_values),
	\+ call(Pred, Value).
check_param_error(Signature, Call_values, forgotten_output(P)) :- 
	member(out(P), Signature), \+ member([P,_], Call_values).


%%%%
%% Algorithm execution context
%%%%

%% FIXME


%%%%
%% Algorithm output
%%%%

build_output(Graph, Actions, Output) :- 
	Output = algo_output(Graph, Actions).

extract_graph(algo_output(Graph,_), Graph).

extract_actions(algo_output(_, Actions), Actions).

%%%%
%% Algorithm execution
%%%%

%% execute(Call, Outputs) :- 
%% 	Call = my_call(Algo, Parameters),
%% 	verify(Algo, Parameters), 
%% 	build_exec_term(Algo, Parameters, Term, Outputs),
%% 	call(Term),!.
	
%% build_exec_term(Algo, Call_values, Term, Output_bindings) :- 
%% 	descr(Algo, Predicate, Signature),
%% 	build_term_members(Signature, Call_values, Term_list,  Output_bindings),
%% 	Term =.. [Predicate|Term_list].
	
%% build_term_members([], _, [], []) :- !. 
%% build_term_members([in(Param,_)|Tail], Call_values, Term_list, Outputs) :- 
%% 	member([Param, Val], Call_values), 
%% 	build_term_members(Tail, Call_values, Other_terms, Outputs),
%% 	Term_list = [Val | Other_terms].
%% build_term_members([out(Param) | Tail], Call_values, Term_list, Outputs) :-
%% 	member([Param, Symbol], Call_values),
%% 	build_term_members(Tail, Call_values, Other_terms, Other_outputs),
%% 	Term_list = [Free | Other_terms],
%% 	Outputs = [[Symbol, Free] | Other_outputs].
