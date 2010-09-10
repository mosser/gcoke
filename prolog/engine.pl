%%%%
%% This file is part of gCoke [ http://www.gcoke.org ]
%%
%% Copyright (C) 2010-  Sebastien Mosser
%%
%% gCoke is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as 
%% published by the Free Software Foundation; either version 2 of 
%% the License, or (at your option) any later version.
%%
%% gCoke is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public 
%% License along with gCoke; if not, write to the Free Software Foundation,
%% Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% @author   Main    Sebastien Mosser  [ sm@gcoke.org ]
%%%%
:- module(engine,[]).
:- channels:declare(engine(exec)), channels:declare(engine(refine)).

%%%%
%% Remarks:
%%   - Actions are considered as DETERMINISTIC (strong assumption). A cut is 
%%     performed after each action execution or refinment.
%%   - Atomic action are terms of arity N. The associated predicate has the same
%%     name, and has an arity of N+2. The first N variables are action-specific.
%%     The two last FREE variables are (i) the graph to be used as input
%%     and (ii) the resulting graph.
%%   - Composite actions are terms of arity N. The associated predicate is
%%     a N+2 predicate, like the atomic one: the first parameter is the graph, 
%%     and the last is the resulting action list to be executed.
%%   - /!\ Composite actions are refined ON-THE-FLY. And yes, it is a good idea!
%%   - do_sequence is the only predicate which should be used externally. 
%%
%% Relations with 'constraints':
%%   - Pre- and Post-conditions are checked for each atomic action execution.
%%   - Invariants are checked after the execution of a sequence of actions. 
%%%%

%%%%
%% Action Sequence Execution
%%%%

%% do_sequence/4: do_sequence(+Graph, +Action_list, -Result, -Counter)
% Execute a sequence of actions (Action_list) on Graph, and unify the resulting
% graph with Result. Counter is unified with the number of executed actions.
do_sequence(Graph, [], Graph, 0) :- constraints:check_invariants(Graph), !.
do_sequence(Graph, [Action|Tail], Result, Counter) :- 
	do(Graph, Action, Tmp_graph, Increment), 
	do_sequence(Tmp_graph, Tail, Result, Others),
	Counter is Increment + Others.

%% do/4: do(+Graph, +Action, -Result, -Counter)
% Execute Action on Graph, and unify the resulting graph with Result. Counter is
% unified with the number of actions really executed (eg composite).
% /!\ Remark: encountered composite actions are refined on the fly, here.
do(Graph, Action, Result, Counter) :- 
	constraints:check_conditions(pre, Action, Graph),
	do_inner(Graph, Action, Result, Counter),
	constraints:check_conditions(post, Action, Graph).
	
%% do_inner/4: do_inner(+Graph, +Action, -Result, -Counter)
% Refine a composite action, if needed, and execute the associated actions.
do_inner(Graph, Action, Result, Counter) :- 
	is_composite(Action), !, 
	refine_composite_action(Graph, Action, Action_list),
	do_sequence(Graph, Action_list, Result, Counter).
do_inner(Graph, Action, Result, 1) :- 
	execute_atomic_action(Graph, Action, Result).


%%%%
%% Scalar Action Execution
%%%%

%% execute_atomic_action/3: execute_atomic_action(+Graph, +Action, -Result)
% check pre-conditions associated to Action, unify Result with the graph 
% obtained after the execution of Action on Graph, and then check 
% post-conditions.
execute_atomic_action(Graph, Action, Result) :- 
	channels:push(engine(exec), 'Executing "~w"', [Action]), 
	(call(Action,Graph,Result) 
          -> true ; throw_error(Graph, Action, execute)), !.

%%%%
%% Composite Action Handling
%%%%
:- dynamic gcoke_registered_composite_action/1.

%% register_as_composite/1: register_as_composite(+Action_head)
% register in the database that an action with Action_head term as head must
% be refined prior to its execution.
register_as_composite(Action_head) :- 
	assert(gcoke_registered_composite_action(Action_head)).

%% is_composite/1: is_composite(+Action)
% fail if Action is not a composite action.
is_composite(Action) :- 
	gcoke_functor(Action, Head, _), gcoke_registered_composite_action(Head).

%% refine_composite_action/3: refine_composite_action(+G, +Action, -Action_list)
% refine Action (assumed as a composite one) into its associated list of 
% atomic actions
refine_composite_action(Graph, Action, Action_list) :- 
	channels:push(engine(refine), 'Refining "~w"', [Action]), 
	(call(Action, Graph, Action_list) 
          -> true; throw_error(Graph, Action, refine)), !,
	channels:push(engine(refine), '  -> ~w', [Action_list]).
%%%%
%% Internal Helpers
%%%%

%% throw_error/3: throw_error(+Graph, +Action, +Engine_kind)
% Throw an Engine_kind error (i.e., execute or refine), for Action on Graph.
throw_error(Graph, Action, Engine_kind) :- 
	graph:read_name(Graph,Name), swritef(Info,'Graph %w', [Name]), 
	Error = error(gcoke_engine_error(Engine_kind, Action),
	              context(Info,'Engine Error')),
	throw(Error).

%% msg/4: msg(+Graph, +Engine_kind, +Format, +Args)
% push a message (Format/Args) on channel engine(Engine_kind), for Graph.
msg(Graph, Engine_kind, Format, Args) :- 
	graph:read_name(Graph, Name), 
 	swritef(Str,'Graph "%w" / %w', [Name, Format]), 
 	string_to_atom(Str, Atom),
 	channels:push(engine(Engine_kind), Atom, Args).
