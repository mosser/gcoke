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

:- module(constraints,[]).
/** <module> Handling constraints (pre/post/invariant)

Remarks: 
  - a pre/post predicate 'p' is a predicate of arity 2: p/2
       p(+Graph, +Action). Fail => pre/post condition violation.
  - an invariant predicate 'p' is a predicate of arity two
       p(+Graph). Fail => invariant violation.
  In the two previous situations, only the head of the predicate (i.e., 'p')
  is registered in the database. The other variables are automatically added
  by the gCoKe constraint checking engine. 
*/


:- channels:declare(constraints(pre)), 
   channels:declare(constraints(post)),
   channels:declare(constraints(check)).

%%%%
%% Pre- & Post-conditions Registration & Checking
%%%%
:- dynamic gcoke_registered_pred/3.

%% register_predicate(+Kind, +Action, +Predicate)
% register a Predicate for pre/post (Kind) condition checking associated to
% the given Action (head of the action term).
register_predicate(Kind, Action, Predicate) :- 
	assert(gcoke_registered_pred(Kind, Action, Predicate)).

%% get_predicates(+Kind, +Action, -Predicates)
% unify Predicates with a list of predicated associated as pre/post (Kind)
% conditions for Action
get_predicates(Kind, Action, Predicates) :- 
	findall(P, gcoke_registered_pred(Kind, Action, P), Predicates).

%% check_conditions(+Kind, +Action, +Graph)
% Perform pre/post (Kind) condition checking associated to Action in Graph
check_conditions(Kind, Action, Graph) :- 
	gcoke_functor(Action,Head,_), get_predicates(Kind,Head,Predicates),
	ExtraData =.. [Kind, Action], 
	msg(Graph, Kind, 'Checking ~w-conditions for "~w"', [Kind,Action]),
	check_conditions_loop(Predicates, Graph, Action, ExtraData).

%% check_conditions_loop(+Preds, +Graph, +Action, +Err)
% Iterate over Preds list, checking each predicate on Graph & Action. 
% Err contains information for error handling purpose (eg pre(my_action) ).
check_conditions_loop([],_,_,_).
check_conditions_loop([P|Tail], Graph, Action, ErrData) :- 
	msg(Graph, check, 'Condition Predicate "~w"', [P]),
	(call(P,Graph,Action) -> true ; throw_error(Graph, P, ErrData)),
	check_conditions_loop(Tail, Graph, Action, ErrData).

%%%%
%% Invariant Registration & Checking
%%%%
:- dynamic gcoke_registered_inv/1.

%% register_invariant(+Predicate)
% register Predicate as an invariant
register_invariant(Predicate) :- 
	assert(gcoke_registered_inv(Predicate)).

%% get_invariants(-Predicates)
% unify Predicates with a list of registred invariants
get_invariants(Predicates) :- 
	findall(P,gcoke_registered_inv(P),Predicates).

%% check_invariants(+Graph)
% Check all registred invariants 
check_invariants(Graph) :- 
	get_invariants(Predicates), 
	check_invariants_loop(Predicates,Graph).

%% check_invariants_loop(+Predicates, +Graph)
% Iterate over Predicates, checking each predicate on Graph.
check_invariants_loop([],_).
check_invariants_loop([P|Tail], Graph) :- 
	msg(Graph,check,'Invariant predicate "~w"', P),
	(call(P,Graph) -> true ; throw_error(Graph, P, invariant)),
	check_invariants_loop(Tail, Graph).

%%%%
%% Internal Helpers
%%%%

%% build_error(+Graph, +Predicate, +Extra, -Error)
% Unify Error with the associated error term, according to the other variables.
throw_error(Graph, Predicate, Extra) :- 
	graph:read_name(Graph,Name), %swritef(Info,'Graph %w', [Name]), 
	Error = error(gcoke_constraint_error(Predicate,Extra),
	              context(graph(Name),'Constraint violation')),
	throw(Error).

%% msg(+Graph, +Level, +Format, +Args)
% push a message (Format/Args) on channel constraints(Level), for Graph.
msg(Graph, Level, Format, Args) :- 
	graph:read_name(Graph, Name), 
	swritef(Str,'Graph "%w" / %w', [Name, Format]), 
	string_to_atom(Str, Atom),
	channels:push(constraints(Level), Atom, Args).