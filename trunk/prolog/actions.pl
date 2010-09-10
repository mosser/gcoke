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

:- module(actions,[]).

%%%%
%% Atomic Actions: Add
%%%%

:- Action = 'actions:add_node',
   constraints:register_predicate(pre, Action, actions:non_existing_node).
add_node(Node_name, Key_value_list, Graph, Result) :- 
	graph:build_node(Node_name, Empty_node),
	graph:build_property_list(Key_value_list, Prop_list),
	graph:push_property_list(Empty_node, Prop_list, Node),
	graph:push_node(Graph, Node, Result).

:- Action = 'actions:add_edge',
   constraints:register_predicate(pre, Action, actions:existing_boundaries).
add_edge(Source_node, Target_node, Key_value_list, Graph, Result) :- 
	graph:build_edge(Source_node, Target_node, Empty_edge),
	graph:build_property_list(Key_value_list, Prop_list),
	graph:push_property_list(Empty_edge, Prop_list, Edge),
	graph:push_edge(Graph, Edge, Result).

add_property(Key, Value, Graph, Result) :- 
	graph:build_property(Key, Value, Prop),
	graph:push_property(Graph, Prop, Result).

%%%%
%% Atomic Actions: Del
%%%%

:- Action = 'actions:del_node',
   constraints:register_predicate(pre, Action, actions:existing_node), 
   constraints:register_predicate(pre, Action, actions:unused_node).	
del_node(Node_name, Graph, Result) :- 
	queries:get_node_by_name(Graph, Node_name, Node), 
	graph:pull_node(Graph, Node, Result).
	
:- Action = 'actions:del_edge',
   constraints:register_predicate(pre, Action, actions:unique_selection).
del_edge(Source_name, Target_name, Key_value_list, Graph, Result) :- 
	queries:get_edge_by_properties(Graph, Key_value_list, Source_name, 
	                             Target_name, Edge), !, %% => is det.
	graph:pull_edge(Graph, Edge, Result).


%%%%
%% Composite Action: replace
%%%%

:- Action = 'actions:replace_node',
  engine:register_as_composite(Action),
  constraints:register_predicate(pre,Action, actions:existing_nodes).
replace_node(Old_name, New_name, Graph, Action_list) :- 
	findall([actions:del_edge(S, Old_name, Key_val_list),
	         actions:add_edge(S, New_name, Key_val_list)],
		 ( queries:get_edge_by_boundaries(Graph, S, Old_name,E),
		  graph:get_properties(E, Props),
		  graph:build_key_value_list(Props, Key_val_list)), 
		Predecessors),
	findall([actions:del_edge(Old_name, T, Key_val_list),
	         actions:add_edge(New_name, T, Key_val_list)],
		 (queries:get_edge_by_boundaries(Graph, Old_name, T, E),
		  graph:get_properties(E, Props),
		  graph:build_key_value_list(Props, Key_val_list)), 
		Successors),
	flatten([Predecessors, Successors, actions:del_node(Old_name)], 
	         Action_list).

%%%%	
%% Composite Action: unify
%%%%

:- Action = 'actions:unify_node',
  engine:register_as_composite(Action),
  constraints:register_predicate(pre, Action, actions:non_existing_node).
unify_node(Node_name_list, New_name, Graph, Action_list) :- 
	findall(Props, (member(Node_name, Node_name_list),
	                queries:get_node_by_name(Graph,Node_name, Node),
			graph:get_properties(Node, Props)),
			Properties_list),
	flatten(Properties_list, Raw_prop_list), 
	sort(Raw_prop_list, Prop_list), 
	graph:build_key_value_list(Prop_list, Key_val_list),
	findall(actions:replace_node(N,New_name), member(N, Node_name_list), 
	        Replace_list),
	Action_list = [actions:add_node(New_name, Key_val_list)|Replace_list].

%%%%
%% Constraints associated to these actions, part of the gCoke kernel
%%%%

%% Add node precondition
non_existing_node(G, Action) :-
	(  Action = actions:add_node(Id,_)
         ; Action = actions:unify_node(_,Id) ),
	\+ queries:get_node_by_name(G,Id,_).

%% Del node preconditions
existing_node(G, actions:del_node(Id)) :- 
	queries:get_node_by_name(G,Id,_).
unused_node(G,actions:del_node(Id)) :- 
	\+ queries:get_edge_by_boundaries(G,Id,_,_),
	\+ queries:get_edge_by_boundaries(G,_,Id,_).

%% Add edge precondition
existing_boundaries(Graph, actions:add_edge(Source, Target, _)) :-
	queries:get_node_by_name(Graph,Source,_),
	queries:get_node_by_name(Graph,Target,_).
	
%% Del node precondition
unique_selection(Graph, actions:del_edge(Source, Target, Key_value_list)) :- 
	findall(E, queries:get_edge_by_properties(Graph, Key_value_list,
	                                          Source, Target,E), [E]).
%% Replace node precondition
existing_nodes(Graph, actions:replace_node(Old, New)) :- 
	queries:get_node_by_name(Graph, Old, _), 
	queries:get_node_by_name(Graph, New, _). 