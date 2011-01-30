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

:- module(graph, []).
/** <module> Graph meta-model definition

  - Metamodel definition
    - A graph holds a name, and contains nodes and edges (as unordered sets).
    - A node holds a name. Edges are anonmyous
    - An edge is an entity referencing a source node and a target one.
    - Elements (i.e., graph, node & edge) holds properties.
    - A property if a key-value pair.

  - /!\ Implementation detail:
    - The current implementation always assume that the properties are stored
      in the last slot of an element. Do not change this position.
    - Same assumptions is made for the 'name': it must be the first term.
*/

%%%%
%% Constructors 
%%%%

%% build_graph(+Name, -Graph)
% create an empty graph, named 'Name'
build_graph(Name, gcoke_graph(Name, [], [], [])).

%% build_node(+Name, -Node)
% create an empty node, named 'Name'
build_node(Name,  gcoke_node(Name, [])).

%% build_edge(+Source_name, +Target_name, -Node)
% create an empty edge, from the node named 'Source_name' to the node 
% named 'target_name'.
build_edge(Source_name, Target_name, gcoke_edge(Source_name, Target_name, [])).

%% build_property(?Key, ?Value, ?Property)
% create a property, binding Key to Value.
build_property(Key, Value, gcoke_property(Key, Value)).

%%%%
%% Model Accessors
%%%%

%% read_name(+Elem, -Name)
% Unify Name with the actual name of Elem. Edges are anonmyous (-> fail).
read_name(Elem, Name) :- 
	Elem \= gcoke_edge(_,_,_), arg(1,Elem,Name).

%% read_properties(+Element, +Key, -Values)
% Unify with Values a list of values binded to Key in Element's properties.
read_properties(Elem, Key, Prop_values) :- 
	build_property(Key,_,Prototype),
	get_properties(Elem,All_props),
	findall(Val, ( member(P, All_props), P = Prototype, 
	               graph:get_property_value(P, Val)),
		Prop_values).

%% read_edge_boundaries(+Edge, +Source_n, +Target_n)
% Extract Source_n(ame) and Target_n(ame) from Edge.
read_edge_boundaries(Edge, Source_name, Target_name) :- 
	Edge = gcoke_edge(Source_name, Target_name,_).

%% read_property_content(+Property, -Key, -Value)
% read the content of a given property (uses build_property)
read_property_content(Property, Key, Value) :- 
	build_property(Key, Value, Property).

%%%%
%% Graph Getters (get_* are deterministic, has_for_* are non-deterministic).
%%%%

%% get_nodes(+Graph, -Nodes)
% Unify Nodes with actual nodes contained by Graph.
get_nodes(Graph,Nodes) :- Graph = gcoke_graph(_,Nodes,_,_).

%% get_edges(+Graph, -Edges)
% Unify Edges with the actual edges contained by Graph.
get_edges(Graph,Edges) :- Graph = gcoke_graph(_,_,Edges,_).

%% get_properties(+Element, -Properties)
% unify Properties with a list of property contained by Element.
get_properties(Element, Properties) :- 
	Element =.. List, last(List, Properties).

%% get_property_value(+Property, -Value)
% unify Value with the actual value contained in Property.
get_property_value(Property, Value) :- 
	Property = gcoke_property(_,Value).

%% has_for_node(+Graph, -Node) is nondet
% Node is unified with a node contained by Graph.
has_for_node(Graph, Node) :- 
	get_nodes(Graph, Nodes), member(Node, Nodes).

%% has_for_edge(+Graph, -Edge) is nondet
% Edge is unified with an edge contained by Graph.
has_for_edge(Graph, Edge) :- 
	get_edges(Graph, Edges), member(Edge, Edges).

%% has_for_property(+Element, -Property) is nondet
% Property is unified with a property contained by Element).
has_for_property(Elem, Prop) :- 
	get_properties(Elem, Props), member(Prop, Props).

%%%%
%% Pushers: push an element into another one (aka 'add')
%%%%

%% push_node(+Graph, +Node, -Result)
% Result is unified with Graph, enriched by Node.
push_node(Graph, Node, Result) :- 
	Graph = gcoke_graph(Name, Nodes, Edges, Properties),
	Result = gcoke_graph(Name, [Node|Nodes], Edges, Properties).

%% push_edge(+Graph, +Edge, -Result)
% Result is unified with Graph, enriched by Edge.
push_edge(Graph, Edge, Result) :- 
	Graph = gcoke_graph(Name, Nodes, Edges, Properties),
	Result = gcoke_graph(Name, Nodes, [Edge|Edges], Properties).

%% push_property(+Element, +Property, -Result)
% Result is unified with Element, enriched by Property.
push_property(Element, Property, Result) :- 
	Element =.. List, last(List, Properties),
	reverse(List,[_|Reversed_others]), reverse(Reversed_others, Others),
	append(Others, [[Property|Properties]], New_list),
	Result =.. New_list.

%% push_property_list(+Element, +Property_list, -Result)
% Iterate over Property_list to push all the contained properties into Element.
push_property_list(Element,[], Element).
push_property_list(Element,[Property|Tail], Result) :- 
	push_property(Element, Property, Temp),
	push_property_list(Temp, Tail, Result).

%%%%
%% Pullers: pull an element out of another one (aka 'del')
%% Remark: do nothing if the element to pull out is not contained by the other.
%%%%

%% pull_node(+Graph, +Node, -Result)
% Result is unified with Graph, deprived of Node.
pull_node(Graph, Node, Result) :- 
	Graph = gcoke_graph(Name, Nodes, Edges, Properties),
	delete(Nodes, Node, Removed_list),
	Result = gcoke_graph(Name, Removed_list, Edges, Properties).

%% pull_edge(+Graph, +Edge, -Result)
% Result is unified with Graph, deprived of Edge
pull_edge(Graph, Edge, Result) :- 
	Graph = gcoke_graph(Name, Nodes, Edges, Properties),
	delete(Edges, Edge, Removed_list),
	Result = gcoke_graph(Name, Nodes, Removed_list, Properties).

%% pull_property(+Element, +Property, -Result)
% Result is unified with Element, deprived of Property
pull_property(Element, Property, Result) :- 
	Element =.. List, last(List, Properties),
	delete(List, Properties, Others), 
	delete(Properties, Property, New_list),
	append(Others,[New_list], New_term_list), Result =.. New_term_list.

%%%%
%% Persistence API
%%%%
%% Internal representation:  stored(Name, Nodes, Edges, Properties).
%%  - Name:  the graph identifier
%%  - Nodes: a list of gcoke_node(Id,Properties) elements
%%  - Edges: a list of gcoke_edge(SourceNode,TargetNode,Properties) elements
%%  - Properties: a list of gcoke_property(Key,Value) elements
:- dynamic stored/4. 

%% pull_from_db(+Name, -Graph)
% Read a graph named Name in the fact database, and unify it with Graph
pull_from_db(Name, Graph) :- 
	stored(Name, Nodes, Edges, Properties),
	Graph = gcoke_graph(Name, Nodes, Edges, Properties).

%% push_into_db(+Graph)
% push Graph into the fact database (using Graph's name as key).
% /!\ Remark: a pre-existing graph will be silently deleted.
push_into_db(Graph) :- 
	Graph = gcoke_graph(Name,Nodes,Edges,Properties),
	del_from_db(Name), assert(stored(Name,Nodes,Edges,Properties)).

%% del_from_db(+Name)
% delete the graph stored under the key Name in the fact database. A 
% non-existing graph will be silently ignored.
del_from_db(Name) :- 
	stored(Name,_,_,_) -> retract(stored(Name,_,_,_)); true.

%%%%
%% Helpers
%%%%

%% rename(+Graph, +New_name, -New_graph)
% rename Graph with New_name, and unifiy the result with New_graph
rename(Graph, New_name, New_graph) :- 
	Graph = gcoke_graph(_, A, B, C), 
	New_graph = gcoke_graph(New_name, A, B, C).

%% build_property_list(+Key_value_list, -Property_list)
% unify Property_list with a list of properties, based on Key_value_list
build_property_list(Key_value_list, Property_list):-
	findall(P, (member([K,V],Key_value_list),graph:build_property(K,V,P)),
	        Property_list).

%% build_key_value_list(+Property_list, -Key_value_list)
% unify Key_value_list with a list of [K,V] elements, based on Property_list.
build_key_value_list(Property_list, Key_value_list) :- 
	findall([K,V],(member(P, Property_list), 
	               graph:read_property_content(P,K,V)), Key_value_list). 