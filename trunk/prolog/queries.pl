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
:- module(queries, []).

%%%%
%% Scalar Getters (may be non-deterministic)
%%%%

%% get_node_by_name/3: get_node_by_name(+G, +Node_name, -Node)
% Unify Node with the actual node named Node_name in graph G.
get_node_by_name(Graph, Node_name, Node) :- 
	graph:has_for_node(Graph, Node), 
	graph:read_name(Node, Node_name), !. %% single answer by construction. 

%% [nondet] get_edge_by_boundaries/4: 
%    get_edge_by_boundaries(+Graph, ?Source_name, ?Target_name, -Edge)
% Unify Edge with an edge contained in Graph, from Source_name to Target_name.
get_edge_by_boundaries(Graph, Source_name, Target_name, Edge) :- 
	graph:has_for_edge(Graph, Edge),
	graph:read_edge_boundaries(Edge, Source_name, Target_name).

%%%%
%% Property-based searching (itrinsically non deterministic)
%%%%

%% [nondet] get_node_by_properties/3: get_node_...(+Graph, +Key_values, -Node)
% unify Node with a node contained by Graph, holding a superset of properties 
% described in Key_value_list.
get_node_by_properties(Graph, Key_value_list, Node) :- 
	graph:has_for_node(Graph, Node),
	contains_properties(Node, Key_value_list).

%% [nondet] get_edge_by_properties/5:
%   get_edge_by_properties(+Graph, +Key_value_list, ?Source_n, ?Target_n, -Edge)
% unify Edge with an edge contained by Graph, holding a superset of properties 
% described in Key_value_list. Source_n and Target_n can be used to restrict 
% the scope of the search (and may be partially instantiated).
get_edge_by_properties(Graph, Key_value_list, Source_n, Target_n, Edge) :- 
	get_edge_by_boundaries(Graph, Source_n, Target_n, Edge),
	contains_properties(Edge, Key_value_list).

%% contains_properties/2: contains_properties(+Element, +Key_value_list)
% succeed if Key_value_list defines a subset of Element' properties.
contains_properties(Element, Key_value_list) :-
	graph:get_properties(Element, All_properties),
	graph:build_property_list(Key_value_list, Prop_list),
	subset(Prop_list, All_properties).
