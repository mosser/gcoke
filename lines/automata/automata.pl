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
:- module(automata, []).

:- use_module(graphics). %% Handle the graphical customisation of gCoKe

%%%%
%% Domain Specific Builder
%%%%

build(Name, A) :- 
	graph:build_graph(Name, Tmp), graph:build_property(family, moore, P),
	graph:push_property(Tmp, P, A).

%%%%
%% Domain Specific actions
%%%%

:- Action = 'automata:add_start', engine:register_as_composite(Action),
   constraints:register_predicate(pre, Action, automata:single_props).
add_start(Name, _, [actions:add_node(Name, [[kind, start]])]).

:- Action = 'automata:add_state', engine:register_as_composite(Action).
add_state(Name, _, [actions:add_node(Name, [[kind, normal]])]).

:- Action = 'automata:add_final', engine:register_as_composite(Action),
   constraints:register_predicate(pre, Action, automata:single_props).
add_final(Name, _, [actions:add_node(Name, [[kind, final]])]).

:- engine:register_as_composite('automata:add_trans').
add_trans(Source, Target, Symbols, _, [Action]) :-
	findall([symbol, S], member(S, Symbols), Values),
	Action = actions:add_edge(Source,Target,Values).

%%%%
%% Domain Specific constraints
%%%%

single_props(Automata, Action) :-
	(  Action = automata:add_start(_), L = [[kind, start]]
         ; Action = automata:add_final(_), L = [[kind, final]]),
	\+ queries:get_node_by_properties(Automata, L, _).


