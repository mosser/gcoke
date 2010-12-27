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

:- module(composition, []).

%%%%
%% Pool Construction & Handling
%%%%

%% build_pool/4: build_pool(+Name, +Input_graphs, +Output_graphs, -Pool)
% unify Pool with the pool associated to the orther arguments, and an empty
% call list. Inputs and 
build_pool(Name, In_graph_names, Out_graph_names, P) :- 
	P = pool(Name, In_graph_names, Out_graph_names, []).

%% push_directive_set/3: push_directive_set(+In, +Dirs, -Out)
% adds all the directives stored in Dirs into In, and unify the result with Out
% Remarks: introduced to simplify the gck compiler implementation.
push_directive_set(Pool, [], Pool).
push_directive_set(Pool, [Dir|Others], Result) :- 
	Dir = dir(Algo_name, Input_raw_lst, Output_raw_lst),
	findall(Param,( member([N, V], Input_raw_lst),
	                composition:build_parameter_binding(N, V, Param)), 
		Input_param_lst),
	findall(Param,( member([N, V], Output_raw_lst),
	                composition:build_parameter_binding(N, V, Param)), 
		Output_param_lst),
	build_call(Algo_name, Input_param_lst, Output_param_lst, Call),
	push_directive_set(Pool, Others, Tmp),
	push_call(Tmp, Call, Result).


%% push_call/3: push_call(+Pool, +Call, -Result)
% unify Result with Pool enhanced with Call in its call list.
push_call(Pool, Call, Result) :- 
	Pool = pool(N, Ins, Outs, Calls),
	Result = pool(N, Ins, Outs, [Call|Calls]).

%% push_call_set/3: push_call_set(+Pool, +Call_set, -Result).
% Unify Result with  the pool that contains all the calls defined in Call_set, 
% added with the ones defined in the initial Pool.
push_call_set(Pool, [], Pool).
push_call_set(Pool, [Call | Others], Result) :- 
	push_call(Pool, Call, Tmp), push_call_set(Tmp, Others, Result).

%%%%
%% Call & Parameters: Construction & Handling
%%%%

%% build_call/4: build_call(+Algo, +Input, +Outputs, -Call)
% Unify Call with the call structure associated to the invocation of Algo, 
% using Inputs and Outputs as eponymous parameters.
build_call(Algorithm, Input_params, Output_params, Call) :- 
	Call = gcoke_call(Algorithm, Input_params, Output_params).

%% build_parameter_binding/3: build_parameter_binding(+Name,+Artefact,-Binding)
% Unify Binding with the gcoke construction that binds Artefact with the 
% algorithlm parameter called Name.
build_parameter_binding(Parameter_name, Artefact, Binding) :- 
	Binding = binding(Parameter_name, Artefact).

%%%%
%% Composition dependencies (based on the database content)
%%%%

get_required(Composition, Required) :- 
	Composition = pool(_, Required, _, _).

get_provided(Composition, Provided) :- 
	Composition = pool(_, _, Provided, _).

get_associated_graphs(Composition, Graphs) :- 
	get_required(Composition, Required), 
	get_provided(Composition, Provided),
	append(Required, Provided, All), sort(All, Graphs).

%% TODO

%%%%
%% Call Scheduling
%%%%

%% TODO

%%%%
%% Picture Export (FIXME: this code is definitively ugly)
%%
%% Remark: Always code as if the guy who ends up maintaining your code will be 
%%         a violent psychopath who knows where you live. (Rick Osborne)
%%%%

%% show_all/0: show_all
% Show ALL the described composition as a graph
show_all :- 
	findall(C, composition:pull_from_db(_,C), Composition_lst),
	show(Composition_lst).

%% show/1: show(+Composition_list):
% show a graphical representation of the artefact and algorithms involved in 
% the given Composition_list
show(Composition_list) :- 
	tmp_file('compo_to_png',Tmp), as_picture(Composition_list, png, Tmp), 
	getenv_or_default('GCOKE_OPEN','open',E),
	swritef(Cmd,'%w %w.png', [E, Tmp]), channels:push(trace(shell),Cmd,[]),
        shell(Cmd).

%% as_picture/3: as_picture(+Compo_lst, +Format, +F)
% Write a picture (using +Format, e.g., png, pdf) representing +Compo_lst in the
% file %F (automatically postfixed with '.Format').
as_picture(Composition_list, Format, F) :- %% TODO: refactor
	as_dot(Composition_list, Dot_code), tmp_file('compo_to_dot',Tmp), 
	open(Tmp, write, Stream), write(Stream, Dot_code), close(Stream),
        getenv_or_default('GCOKE_DOT','dot',E),	expand_file_name(F,[File]),
        swritef(Cmd,'%w -T%w %w > %w.%w', [E, Format, Tmp, File, Format]), 
        channels:push(trace(shell),Cmd,[]), shell(Cmd).

%% as_dot_file
as_dot_file(Composition_list, File) :- 
	as_dot(Composition_list, Dot_code), 
	open(File, write, Stream), write(Stream, Dot_code), close(Stream).

%% as_dot/2: as_dot(+Composition_list, -Dot_code)
% Build the Dot_code associated to Composition_list
as_dot(Composition_list, Dot_code) :- 
	external_graphs_as_dot(Composition_list, Ext_code),
	composition_list_as_dot(Composition_list, Int_code),
	swritef(Dot_code,'digraph compo {\n  fontname="Courier";\n  node [fontname="Courier", fontsize=12];\n  edge [fontname="Courier",fontsize=10];\n\n%w;\n\n%w\n}',[Ext_code, Int_code]).

%% external_graphs_as_dot/2: external_graphs_as_dot(+Compo_lst, -Code)
% Produce the graphviz Code associated to each as "external" artefacts (that 
% is, publicly available) inviled in Composition_lst.
external_graphs_as_dot(Composition_lst, Ext_code) :- 
	findall(G_lst,( member(C, Composition_lst), 
                	composition:get_associated_graphs(C,G_lst) ), Ext_lst),
	flatten(Ext_lst, Ext_flat), sort(Ext_flat, Externals),
	findall(C, (member(E, Externals), swritef(C,'%w [style=filled, fillcolor=lemonchiffon, label="%w"]',[E,E])), Code_lst),
	swrite_list(Code_lst, ';\n','  ', Ext_code).

composition_list_as_dot(Composition_lst, Dot_code) :- 
	findall(Code,(member(C, Composition_lst),
	              composition:as_dot_cluster(C,Code)),Clusters),
	swrite_list(Clusters, '\n', '', Dot_code).

%% as_dot_cluster/2: as_dot_cluster(+Composition, -Dot_code)
% Transform a given Composition in ots associated Dot_code
as_dot_cluster(Composition, Dot_code) :- 
	Composition = pool(Name, Ins, Outs, Calls),
	flatten([Ins, Outs], External),
	call_list_as_dot(Name, External, Calls, Nodes, Edges, Ext_edges), 
	dot_write_list(Nodes, Internal_nodes),
	dot_write_list(Edges, Edges_descr),
	dot_write_list(Ext_edges, Ext_edges_descr),
	swritef(Dot_code, '  subgraph cluster_%w {\n    label_%w [label="%w", shape="none"];\n%w\n%w\n  }\n%w', 
                [Name, Name, Name, Internal_nodes, Edges_descr, Ext_edges_descr]).

dot_write_list([],'') :- !.
dot_write_list(L,S) :- 
	swrite_list(L,';\n','    ', Tmp), swritef(S,'%w;\n',[Tmp]).

% call_list_as_dot/6: ( ??? o_O ??? ) Pretty ugly, isn'it?  
call_list_as_dot(_,_,[], [], [],[]).
call_list_as_dot(Name, Ext, [Call|Others], Nodes, Int_edges, Ext_edges) :- 
	call_as_dot(Name, Ext, Call, Curr_nodes,Curr_int_edges,Curr_ext_edges),
	call_list_as_dot(Name, Ext, Others, Other_ns, Other_int, Other_ext),
	append(Curr_nodes, Other_ns, Nodes_tmp), sort(Nodes_tmp, Nodes),
	append(Curr_int_edges, Other_int, Int_edges), 
	append(Curr_ext_edges, Other_ext, Ext_edges).

%% pretty ugly, isn'it?
call_as_dot(Name, Externals, Call, Nodes, Int_edges, Ext_edges) :- 
	Call =  gcoke_call(Algo, Ins, Outs), 
	%% Algorithm node,
	gensym(algo_,Id),
 	swritef(Algo_node,
 	  '%w [shape="record", style="filled",fillcolor="grey89",label="%w"]',
 	        [Id, Algo]),% trace,
	%% Inputs & Outputs
	gen_call_params(in,Externals,Name,Id,Ins,Ins_ns,Ins_int_es,Ins_ext_es),
	gen_call_params(out,Externals,Name,Id,Outs,Outs_ns,Outs_int_es,
	                Outs_ext_es),
	%% the all together
	flatten([Algo_node,  Ins_ns, Outs_ns], Nodes),
	append(Ins_int_es, Outs_int_es, Int_edges),
	append(Ins_ext_es, Outs_ext_es, Ext_edges).

	
gen_call_params(_,_,_,_,[],[],[],[]).
gen_call_params(Dir,Exts,Name,Algo_id,Params,Nodes,Int_edges,Ext_edges) :-
	Params = [P|Tail],
	handle_parameter(P, Name, Exts, Algo_id, Dir, N, Ie, Ee),
	gen_call_params(Dir,Exts,Name,Algo_id,Tail,Oth_n,Oth_ie,Oth_ee),
	append(N,Oth_n,Nodes), append(Ie, Oth_ie, Int_edges), 
	append(Ee, Oth_ee, Ext_edges).

handle_parameter(Param, _, Exts, Algo_id, Dir, Nodes, Int_edges, Ext_edges) :-
	%% External graph
	Param = binding(Param_name, graph(Graph_name)), member(Graph_name,Exts),
	build_edge(Dir, solid, normal, Graph_name, Algo_id, Param_name, Edge),
	Nodes = [], Int_edges = [], Ext_edges = [Edge].

handle_parameter(Param,Name,Exts,Algo_id,Dir,Nodes,Int_edges,Ext_edges) :- 
	%% Internal graph
	Param = binding(Param_name, graph(Graph_name)),
	\+ member(Graph_name, Exts),
	swritef(Node_id, '%w_%w', [Name, Graph_name]),
	swritef(Node,'%w [label="%w", style="dashed"]',
                [Node_id,Graph_name]),
	build_edge(Dir, dashed, open, Node_id, Algo_id, Param_name, Edge),
	Nodes = [Node], Int_edges = [Edge], Ext_edges = [].

handle_parameter(Param,_,_,Algo_id,Dir,Nodes,Int_edges,Ext_edges) :- 
	%% Scalar term
	Param =  binding(Param_name, term(Scalar)),
	gensym(scalar, Scalar_id),
	swritef(Scalar_node, '%w [shape="note", style="filled", fillcolor="lightblue",label="\'%w\'"]', [Scalar_id, Scalar]),
        build_edge(Dir, dashed, open, Scalar_id, Algo_id, Param_name, Edge),
	Nodes = [Scalar_node], Int_edges = [Edge], Ext_edges = [].
	
handle_parameter(Param,_,Exts,Algo_id,Dir,Nodes,Int_edges,Ext_edges) :- 
	%% Graph set
	Param = binding(Param_name, graph_set(Graph_lst)),
	gensym(set_node, Set_node_id),
	swritef(Set_node, '%w [shape="circle",label="",style="filled",fillcolor="black", width=0.1,fixedsize=true]', [Set_node_id]),
        build_edge(Dir, dashed, normal, Set_node_id, Algo_id, Param_name, Set_edge),
        findall(E, (member(G, Graph_lst), member(G, Exts), 
	            composition:build_edge(Dir, solid, none,G, Set_node_id, '', E)), 
		Ext_edges),
	findall(E, (member(G, Graph_lst), \+ member(G, Exts), 
	            composition:build_edge(Dir, dashed, none, G, Set_node_id, '', E)),
		Int_graph_edges),
	Nodes = [Set_node],
	Int_edges = [Set_edge|Int_graph_edges].
	

build_edge(in, Style, Head, Node, Algo, Label, Edge) :- 
	swritef(Edge, '%w -> %w [label="%w", style="%w",arrowhead="%w"]',[Node, Algo, Label,Style,Head]).
build_edge(out, Style, Head, Node, Algo, Label, Edge) :- 
	swritef(Edge, '%w -> %w [label="%w", style="%w",arrowhead="%w"]',[Algo, Node, Label,Style,Head]).


is_internal_node(N, Call, Externals) :- 
 	Call = gcoke_call(_, Ins, Outs),
 	(member(binding(_,graph(N)), Ins) | member(binding(_,graph(N)), Outs)),
 	\+ member(N, Externals).

%%%%
%% Persistence API (FIXME: inspired by Graph unit ... refactor redundancy?)
%%%%
:- dynamic stored/4.

%% push_into_db/1: push_into_db(+Composition)
% Store Composition in the fact database.
push_into_db(Composition) :- 
	Composition = pool(Name, Ins, Outs, Calls),
	del_from_db(Name), assert(stored(Name, Ins, Outs, Calls)).

%% pull_from_db/2: pull_from_db(+Name, -Composition)
% Retrieve the Composition declared with its associated Name.
pull_from_db(Name, Composition) :- 
	stored(Name, Ins, Outs, Calls),
	Composition = pool(Name, Ins, Outs, Calls).

%% del_from_db/1: del_from_db(+Name)
% delete the composition stored under the key Name in the fact database. A 
% non-existing composition will be silently ignored.
del_from_db(Name) :- 
	stored(Name,_,_,_) -> retract(stored(Name,_,_,_)); true.
