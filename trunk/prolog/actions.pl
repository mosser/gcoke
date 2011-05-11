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
%
:- module(actions,[]).
/** <module> Actions Implementation

This file implements the behavior of the action available in gCoKe. 

gCoKe supports 2 kinds of actions: Atomic and Composite. Atomic actions 
directly enhance a given graph, and produce another graph as output. Composite 
actions reified macro-actions, which produces an action sequence as output. 
This sequence can contain both atomic and composite actions (refinement is 
executed on-the-fly).

The execution engine (see engine.pl) will automatically fill the 
execution-dependent parameters, that is, the last two parameters. For each 
action, a code snippets describes how to use it with the gCoKe engine. 
*/

%%%%
%% Atomic Actions: Add
%%%%

%% add_node(+Node_name, +Key_value_list, +Graph, -Result)
% Create a node named Node_name, bound to Key_value_list, push it in Graph and
% unify the enhanced graph with Result.
%
% Precondition: non_existing_node
% ==
%   actions:add_node(n, [[prop1, val1], [prop2, val2]])
% ==
add_node(Node_name, Key_value_list, Graph, Result) :- 
	graph:build_node(Node_name, Empty_node),
	graph:build_property_list(Key_value_list, Prop_list),
	graph:push_property_list(Empty_node, Prop_list, Node),
	graph:push_node(Graph, Node, Result).
:- Action = actions:add_node,
   constraints:register_predicate(pre, Action, actions:non_existing_node).

%% add_edge(+Source_node, +Target_node, +Key_value_list, +Graph, -Result)
% Create an edge from Source_node name to Target_node name, bound to 
% Key_value_list, push it in Graph and unify the enhanced graph with Result.
%
% Precondition: existing_boundaries
% ==
%   actions:add_edge(from, to, [[prop1, val1], [prop2, val2]])
% ==
add_edge(Source_node, Target_node, Key_value_list, Graph, Result) :- 
	graph:build_edge(Source_node, Target_node, Empty_edge),
	graph:build_property_list(Key_value_list, Prop_list),
	graph:push_property_list(Empty_edge, Prop_list, Edge),
	graph:push_edge(Graph, Edge, Result).
:- Action = actions:add_edge,
   constraints:register_predicate(pre, Action, actions:existing_boundaries).

%% add_property(+Key, +Value, +Graph, -Result)
% Build a Key-Value graph property, push it in graph and unify with Result.
% This action is idempotent.
% ==
%   actions:add_property(key, value)
% ==
add_property(Key, Value, Graph, Result) :- 
	(graph:read_properties(Graph, Key, [Value]) 
        -> Result = Graph % Idempotency
	;  graph:build_property(Key, Value, Prop), 
	   graph:push_property(Graph, Prop, Result)).
	
%%%%
%% Atomic Actions: Del
%%%%

%% del_node(+Node_name, +Graph, -Result)
% Delete the node named Node_name in Graph, unify with Result.
%
% Preconditions: existing_node, unused_node
% ==
%   del_node(n)
% ==
del_node(Node_name, Graph, Result) :- 
	queries:get_node_by_name(Graph, Node_name, Node), 
	graph:pull_node(Graph, Node, Result).
:- Action = actions:del_node,
   constraints:register_predicate(pre, Action, actions:existing_node), 
   constraints:register_predicate(pre, Action, actions:unused_node).	

%% del_edge(+Source_name, +Target_name, +Key_value_list, +Graph, -Result)
% Find in Graph an edge from a node named Source_name to a node named 
% Target_name, that hold a property set defined as a superset of Key_value_list.
% The resulting graph is unified with Result.
%
% Precondition: unique_selection
% ==
%  del_edge(from, to, [[p1,v1]]) 
% ==
del_edge(Source_name, Target_name, Key_value_list, Graph, Result) :- 
	queries:get_edge_by_properties(Graph, Key_value_list, Source_name, 
	                             Target_name, Edge), !, %% => is det.
	graph:pull_edge(Graph, Edge, Result).
:- Action = actions:del_edge,
   constraints:register_predicate(pre, Action, actions:unique_selection).

%%%%
%% Composite Action: replace
%%%%

%% replace_node(+Old_name, +New_name, +Graph, -Action_list)
% Replace a node named Old_name with a node named New_name in Graph. The 
% actions necessary to perform such a replace are unified with Action_list
%
% Precondition: existing_nodes
% ==
%   replace_node(old, new)
% ==
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
:- Action = actions:replace_node, engine:register_as_composite(Action),
  constraints:register_predicate(pre, Action, actions:existing_nodes).

%%%%	
%% Composite Action: unify
%%%%

%% unify_node(+Node_name_list, +New_name, +Graph, -Action_list)
% Unify all the node contained in Node_name_list (in Graph)  as a new node 
% named New_name. The relevant actions are unified with Action_list
%
% Precondition: non_existing_nodes
% ==
%   unify_node([n1, n2], n)
% ==
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
:- Action = actions:unify_node, engine:register_as_composite(Action),
  constraints:register_predicate(pre, Action, actions:non_existing_node).

%%%%
%% Composite Action: dump
%%%%

%% dump(+Graph_to_dump, +Graph, -Action_list)
% dump the content of Graph_to_dump into Graph. Necessary actions are unified
% with Action_list.
% ==
%   dump(gcoke_graph(...))
% ==
dump(Graph,_, Action_list) :- 
	% Properties
	graph:get_properties(Graph, Props), 
	graph:build_key_value_list(Props, Prop_lst), 
	findall(actions:add_property(K,V), member([K,V],Prop_lst), Prop_acts),
	% Nodes
	graph:get_nodes(Graph, Nodes), 
	findall(actions:add_node(Id,L), 
	        ( member(N,Nodes), graph:read_name(N,Id), 
		  graph:get_properties(N, Node_props), 
		  graph:build_key_value_list(Node_props, L) ), Node_acts),
	% Edges
	graph:get_edges(Graph, Edges), 
 	findall(actions:add_edge(From,To,L), 
 	        ( member(E,Edges), graph:read_edge_boundaries(E,From,To), 
 		  graph:get_properties(E, Edge_props), 
 		  graph:build_key_value_list(Edge_props, L) ), Edge_acts),
	% Final Result
	flatten([Prop_acts, Node_acts, Edge_acts], Action_list).
:- Action = actions:dump, engine:register_as_composite(Action).



%%%%
%% Constraints associated to these actions, part of the gCoKe kernel
%%%%

%% non_existing_node(+Graph, +Action)
% Fail if Action asks to add a node still existing in Graph
%
% Related action: add_node
non_existing_node(G, Action) :-
	(Action = actions:add_node(Id,_) ; Action = actions:unify_node(_,Id)),
	\+ queries:get_node_by_name(G,Id,_).

%% existing_node(+G, +actions:del_node(Id))
% Fail if there is no node named Id in G.
%
% Related Action: del_node
existing_node(G, actions:del_node(Id)) :- 
	queries:get_node_by_name(G,Id,_).

%% unused_node(+G, +actions:del_node(Id))
% Fail if an edge uses a node named Id in G (as both source or target).
%
% Related action: del_node
unused_node(G, actions:del_node(Id)) :- 
	\+ queries:get_edge_by_boundaries(G,Id,_,_),
	\+ queries:get_edge_by_boundaries(G,_,Id,_).

%% existing_boundaries(+Graph, +actions:add_edge(Source, Target, _))
% Fail if there is no node named Source and Target in Graph 
%
% Related action: add_edge
existing_boundaries(Graph, actions:add_edge(Source, Target, _)) :-
	queries:get_node_by_name(Graph,Source,_),
	queries:get_node_by_name(Graph,Target,_).
	
%% unique_selection(+Graph, +actions:del_edge(Source, Target, Key_value_list))
% Fail if the properties in Key_value_list match several edges between Source 
% and Target.
%
% Related action: del_edge
unique_selection(Graph, actions:del_edge(Source, Target, Key_value_list)) :- 
	findall(E, queries:get_edge_by_properties(Graph, Key_value_list,
	                                          Source, Target,E), [E]).

%% existing_nodes(+Graph, actions:replace_node(Old, New)).
% Fail if Old or New node do not exist in Graph. 
%
% Related action: replace_node
existing_nodes(Graph, actions:replace_node(Old, New)) :- 
	queries:get_node_by_name(Graph, Old, _), 
	queries:get_node_by_name(Graph, New, _). 