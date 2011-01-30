%%%
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
%% @author   Main    Sebastien Mosser  [mosser@polytech.unice.fr]
%%%%
:- module(dot,[]).
:- channels:declare(dot(shell)).

/** <module> Dot transformation module

 Rationale: to automate the gCoke graph -> Graphviz artefact transformation.

 The 'structure' of the transformation is hard-coded in the gCoke framework.
 Users can then "configure" the transformation to call their own predicates
 when needed, and then specialize the transformation for their own needs.
 Their predicates will be automatically called by the framework, which 
 handles the 'technical' part of the transfromation process.

 User-Given Predicate (UGP) call are identified in the transformation with
 a 'UGP call' comment. 

 Remark: The code assume the 'dot' executable to be in the PATH. One can use
   a GCOKE_DOT shell environment variable to set an absolute path.
   It also assumes a picture visualizer, named 'open' (Mac OS X swiss-knife
   command). the GCOKE_OPEN shell variable can be set to oveeride this 
   default value.

 Builder: User-given Predicates to be used to build the dot source code
 
 A builder is a list of [Key, Value] pairs, where key is a predicate nickname
 and Value the concrete predicate to be used in this transformation. gCoke
 defines 'default' predicates to be used when nothing is given by the user.
 These predicates supports a 'free' transformation, but generates ugly 
 pictures. You should look at the default implementation before providing your
 own.

 List of nicknames to be used whilst transforming the graph:
  - partition: The associated predicate 'p' is used to extract the 'main' 
               graph, and the associated subgraphs to be printed as clusters.
     ->  p(+Graph, -Main, -Clusters). 
     -> It unify 'Main' with a list of 'nodes', and clusters with a list of 
        list of nodes (e.g., [[a,b],[c,d,e]]). Nodes cannot be part of 
        multiple lists.

  - graph_config: Declare global config information for the graphviz code,
                  such as fontname, node shape, ...
     -> p(-List)
     -> List is unified with strings to be put in the graphviz header 
        section (e.g., 'fontname = Courier', 'node [shape="record"]'). 

  - graph_handler: Build graph-specific top-level information.
     -> p(+Graph, -Key_value_list)
     -> Key_value_list must be unified with a list of [K,V] elements, based 
        on the content of Graph (e.g, [label, real_graph_name]).
  
  - node_handler: build node specific attributes (e.g., label, color)
    -> p(+Graph, +Node, +Location, +Key_value_list)
    -> Graph is unified with the whole graph, and Node with the node to 
       transform. Location is 'main' (the node is in the main graph) or
      'cluster' (the node is part of a cluster, according to 'partition').
       Key_value_list must be unified with a list of [K,V] elements.

  - edge_handler: build edge specific attributes  (e.g., label, color)
    -> p(+Graph, +Edge, +Key_value_list)
    -> See 'node_handler', and replace Node by Edge (and eject Location) ^_^.

  - cluster_handler: build cluster-specific top level information
    -> p(+Graph, +Cluster, -Prop_list).
    -> see graph_handler

  - custom_code: allows users to defines specific Graphviz code
    -> p(+Graph, -Code).
    -> the user-given code is added at the end of the generated one.

 Implementation Remarks:
   - Assumption (strong): User-given predicate are deterministic and cannnot 
     fail. A more robust version should ensure the determinism (with a !) and 
     catch predicate failure. TODO.
   - An understanding of Graphviz is necessary to perform customizations!
     [ http://www.graphviz.org/Documentation.php ]
   - Edges are always transformed in the main content, even if the involved
     nodes are part of a cluster. It should not interfere, however.
*/

%% get_value(+Builder, +Key, -Value)
% Unify Value with the predicated associated to Key. If one exists in Builder, 
% this one is used. In the other case, the default one is used.
get_value(Builder, Key, Value) :- member([Key, Value], Builder), !.
get_value(_, partition,       dot:default_partition) :- !.
get_value(_, graph_config,    dot:default_graph_config) :- !.
get_value(_, graph_handler,   dot:default_graph_handler) :- !.
get_value(_, node_handler,    dot:default_node_handler) :- !.
get_value(_, edge_handler,    dot:default_edge_handler) :- !.
get_value(_, cluster_handler, dot:default_cluster_handler) :- !.
get_value(_, custom_code,     dot:default_custom_code) :- !.

%%%%
%% Default Handler for graph' elements
%%%%

%% default_partition(+Graph, -Main, -Clusters)
% Consider all nodes as Main, and Unify Clusters with the empty list.
default_partition(Graph, Main, []) :- 
	graph:get_nodes(Graph, Main).

%% default_graph_config(+Config_list)
% Use Courier font and record-shaped nodes.
default_graph_config(Config_list) :- 
	Config_list = [].

%% default_graph_handler(+G, -Prop_list)
% The graph label is unified with its name.
default_graph_handler(Graph, [[label, Name]]) :- 
	graph:read_name(Graph, Name).

%% default_node_handler(+G, +N, +Loc, -List)
% The node label is unified with its name.
default_node_handler(_, Node, _, [[label, Name]]) :- 
	graph:read_name(Node, Name).

%% default_edge_handler(+G, +E, -List)
% No extra-information for edges.
default_edge_handler(_, _, []).

%% default_cluster_handler(+G, +Cluster, -List)
% No clusters in the default implementation
default_cluster_handler(_,_,_,_).

%% default_custom_code(+Graph,-Code).
% Allows user-defined custom code, expressed as plain Graphviz
default_custom_code(_,'').

%%%%
%% Graph -> dot-handled file format 
%%    See [http://www.graphviz.org/doc/info/output.html] for a list of output
%%    format supported by Graphviz
%%%%


%% show(+Graph, +Builder)
% Tranform Graph into a PNG picture, and open a picture visualizer tool.
show(Graph, Builder) :- 
	tmp_file('gcoke_to_dot_to_png',Tmp), draw(Graph, Builder, png, Tmp), 
	getenv_or_default('GCOKE_OPEN','open',E),
        swritef(Cmd,'%w %w.png', [E, Tmp]), 
        channels:push(dot(shell),Cmd,[]),
        shell(Cmd).

%% draw(+Graph, +Builder, +Format, +F)
%  The file F.Format (e.g., F='foo', Format='png' ~> 'foo.png') will contains
%  the result of the transformation of Graph, based on Builder predicates.
draw(Graph, Builder, Format, F) :- 
	tmp_file('gcoke_to_dot',Tmp), write_dot_source(Graph, Builder, Tmp),
        getenv_or_default('GCOKE_DOT','dot',E),
	expand_file_name(F,[File]),
        swritef(Cmd,'%w -T%w %w > %w.%w', [E, Format, Tmp, File, Format]), 
        channels:push(dot(shell),Cmd,[]),
        shell(Cmd).

%% write_dot_source(+Graph, +Builder, +File)
% File will contains the graphviz code associated to Graph, according to 
% Builder' predicates.
write_dot_source(Graph, Builder, File) :- 
	transform(Graph,Builder,Dot_code),
	open(File, write, Stream), write(Stream, Dot_code), close(Stream).

%%%%
%% Graph -> Dot source code (Transformation pattern)
%%%%

%% transform(+Graph, +Builder, -Dot_code)
% Dot_code is unified with the graphviz code associated to Graph, based on the
% predicates contained in Builder (or the default ones).
transform(Graph, Builder, Dot_code) :- 
	get_value(Builder, partition, Partition_predicate),
	graph:read_name(Graph, Name),
	call(Partition_predicate, Graph, Main, Clusters),
	build_graph_properties(Builder, Graph, Graph_prop_code),
	build_clusters(Builder, Graph, Clusters, Clusters_code),
	build_main_graph(Builder, Graph, Main, Main_graph_code),
	build_custom_code(Builder, Graph, Custom_code),
	swritef(Dot_code, 'digraph %w {\n%w%w\n%w\n%w\n}', 
                [Name, Graph_prop_code, Clusters_code, 
		 Main_graph_code, Custom_code]), !.

%%%%
%% Graph handling
%%%%

%% build_main_graph(+Builder, +G, +Main, -Dot_code)
% Main contains the nodes dispatched into the Main graph. G is the whole graph, 
% and Builder the current builder. Dot_code is unified with the graphviz code 
% associated to the main content of the graph.
build_main_graph(Builder, Graph, Main_content, Dot_code) :- 
	get_value(Builder, node_handler, Node_handler),
	build_nodes(Graph, Main_content, Node_handler, main, Nodes_code),
	get_value(Builder, edge_handler, Edge_handler),
	build_edges(Graph, Edge_handler, Edges_code),
	swritef(Dot_code, '%w\n%w', 
                [Nodes_code, Edges_code]).

%% build_graph_properties(+Builder, +Graph, -Dot_code)
% Builder is the current builder, and Graph the chole graph. Dot_code is unified
% with graph attributes code (i.e. the header of the Graphviz final source).
build_graph_properties(Builder, Graph, Dot_code) :- 
	get_value(Builder, graph_config, Config_handler),
	call(Config_handler,Config_attr_list),      %% UGP: graph_config
	get_value(Builder, graph_handler, Prop_handler),
	call(Prop_handler, Graph, Graph_prop_list), %% UGP: graph_handler
	prop_list_pattern(Graph_prop_list, '=', Graph_attr_list),
	append(Config_attr_list, Graph_attr_list, Attributes),
	swrite_list(Attributes, ';\n', '\t', Prop_code),
	swritef(Dot_code, '  /** PROPERTIES **/\n%w;', [Prop_code]).

%%%%
%% Node Handling
%%%%

%% build_nodes(+G, +Nodes, +Handler, +Loc, -Dot_code)
% G is the graph, Nodes the nodes to transform (main of cluster' content),
% Handler the predicated to be used to transform a given node, and Loc the 
% location of these nodes (i.e., 'main' or 'cluster'). Dot_code is unified with
% the graphviz sources associated to ALL of these nodes.
build_nodes(_,[],_,_,'').
build_nodes(Graph, Nodes, Handler, Location, Dot_code) :- 
	findall(C,(member(N,Nodes),
	           dot:build_a_node(Handler,Graph, N, Location,C)),
	        Code_list),
	swrite_list(Code_list,';\n','\t',Raw_code),
	swritef(Dot_code,'  /** NODES **/\n%w;',[Raw_code]).

%% build_a_node(+Handler, +G, +Node, +Loc, -Dot_code)
% Handler is the predicate to be used to perform the transformation. G is the
% graph, Node the node to transform, and Loc \in {'main,'cluster'}. Dot_code
% is unified with the graphviz code associated to THIS particular node.
build_a_node(Handler, Graph, Node, Location, Dot_code) :-
	graph:read_name(Node, Node_name),
	call(Handler, Graph, Node, Location, Prop_list), %% UGP: node_handler
	prop_list_to_graphviz(Prop_list, Attributes),
	swritef(Dot_code,'%w %w',[Node_name, Attributes]).

%%%%
%% Edge Handling
%%%%

%% build_edges(+G, +Handler, -Dot_code)
% G is the whole graph, Handler the predicate to be used to transform an edge,
% Dot_code is unified with the graphviz code for ALL edges defined in G.
build_edges(Graph, _, '') :- graph:get_edges(Graph, []), !.
build_edges(Graph, Handler, Dot_code) :-
	graph:get_edges(Graph, Edges),
	findall(C,(member(E,Edges),
	           dot:build_an_edge(Handler,Graph, E,C)),
	        Code_list),
	swrite_list(Code_list,';\n','\t',Raw_code),
	swritef(Dot_code,'  /** EDGES **/\n%w;',[Raw_code]).

%% build_an_edge(+Handler, +Graph, +Edge, -Dot_code)
% Handler is the predicate to be used to perform the transformation. G is the
% graph, Edge the edge to transform.
build_an_edge(Handler, Graph, Edge, Dot_code) :- 
	graph:read_edge_boundaries(Edge, Source_ref, Target_ref),
	call(Handler, Graph, Edge, Prop_list), %% UGP: edge_handler
	prop_list_to_graphviz(Prop_list, Attributes),
	swritef(Dot_code,'%w -> %w %w',[Source_ref, Target_ref, Attributes]).

%%%%
%% Cluster Handling
%%%%

%% build_clusters(+Builder, +Graph, +Cluster_list, -Dot_code)
% Builder is the current builder, and Graph the whole graph. Cluster_list is the
% list obtained with the 'partition' predicate. Dot_code is unified with the 
% graphviz code associated to ALL the identified clusters.
build_clusters(_,_,[],'') :- !.
build_clusters(Builder, Graph, Cluster_list, Dot_code) :- 
	findall(Code, (member(Cluster, Cluster_list), 
	               dot:build_clusters_inner(Graph, Cluster, Builder, Code)),
		Code_list),
	swrite_list(Code_list, '\n','',Raw_clusters),
	swritef(Dot_code,'  /** CLUSTERS */\n%w',[Raw_clusters]).

%% build_clusters_inner(+G, +Cluster, +Build, -Code)
% Code is unified with the graphviz code associated to Cluster in Graph, 
% according to Build predicates.
build_clusters_inner(Graph, Cluster, Builder, Dot_code) :- 
	get_value(Builder, cluster_handler, Handler),
	call(Handler, Graph, Cluster; Prop_list),
	swrite_list(Prop_list,';\n','\t',Prop_code),
	build_cluster_content(Graph, Cluster, Builder, Content_code),
	gensym(cluster_, Cluster_id),
	swritef(Dot_code, '\tsubgraph %w {\n%w;\n%w\n\t}',
                [Cluster_id, Prop_code, Content_code]).

%% build_cluster_content(+G, +Cluster, +Build, -Code)
% Code is unified with the graphviz code associated to the content of Cluster
% (i.e., its transformed nodes), according to Builder predicates.
build_cluster_content(Graph, Cluster, Builder, Dot_code) :- 
	get_value(Builder, node_handler, Node_handler),
	build_nodes(Graph, Cluster, Node_handler, cluster, Dot_code).

%%%%
%% Custom Code handling
%%%%

%% build_custom_code(+Builder, +Graph, -Dot_code)
% Dot_code is unified with custom GraphViz code associated to Graph and Builder
build_custom_code(Builder, Graph, Dot_code) :- 
	get_value(Builder, custom_code, Handler), 
	call(Handler, Graph, Dot_code).

%%%%
%% Helpers 
%%%%

%% prop_list_to_graphviz(+Prop_list, -Dot_code)
% Transform Prop_list (as [[K1, V1], ...]) into the associated graphviz 
% attributes string (i.e., '[K1="V1", ...]').
prop_list_to_graphviz([],'') :- !.
prop_list_to_graphviz(Prop_list, Dot_code) :- 
	prop_list_pattern(Prop_list, '=', Pair_list),
	swrite_list(Pair_list, ', ', '', Raw_code),
	swritef(Dot_code, '[%w]', [Raw_code]).

%% prop_list_pattern(+Prop_list, +Sep, -Pair_list)
% Each [K,V] element of Prop_list is transformed into a 'K Sep "V"' string,
% collected into Pair_list.
prop_list_pattern(Prop_list, Sep, Pair_list) :- 
	findall(Pair, 
	        ( member([K,V], Prop_list), 
	          swritef(Pair, '%w%w"%w"', [K,Sep,V])), Pair_list).