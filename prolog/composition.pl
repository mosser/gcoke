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
/** <module> Composition declaration and execution 

This module supports the implementation of the composition execution engine.
*/

%%%%
%% Pool Construction & Handling
%%%%

%% build_pool(+Name, +Input_graphs, +Output_graphs, -Pool)
% unify Pool with the pool associated to the orther arguments, and an empty
% call list. Inputs and 
build_pool(Name, In_graph_names, Out_graph_names, P) :- 
	P = pool(Name, In_graph_names, Out_graph_names, []).

%% push_directive_set(+In, +Dirs, -Out)
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


%% push_call(+Pool, +Call, -Result)
% unify Result with Pool enhanced with Call in its call list.
push_call(Pool, Call, Result) :- 
	Pool = pool(N, Ins, Outs, Calls),
	Result = pool(N, Ins, Outs, [Call|Calls]).

%% push_call_set(+Pool, +Call_set, -Result).
% Unify Result with  the pool that contains all the calls defined in Call_set, 
% added with the ones defined in the initial Pool.
push_call_set(Pool, [], Pool).
push_call_set(Pool, [Call | Others], Result) :- 
	push_call(Pool, Call, Tmp), push_call_set(Tmp, Others, Result).

%%%%
%% Call & Parameters: Construction & Handling
%%%%

%% build_call(+Algo, +Input, +Outputs, -Call)
% Unify Call with the call structure associated to the invocation of Algo, 
% using Inputs and Outputs as eponymous parameters.
build_call(Algorithm, Input_params, Output_params, Call) :- 
	Call = gcoke_call(Algorithm, Input_params, Output_params).

%% build_parameter_binding(+Name,+Artefact,-Binding)
% Unify Binding with the gcoke construction that binds Artefact with the 
% algorithlm parameter called Name.
build_parameter_binding(Parameter_name, Artefact, Binding) :- 
	Binding = binding(Parameter_name, Artefact).

%%%%
%% Composition dependencies
%%%%

%% get_required(?Composition, ?Required)
% Extract from Composition the Required graphs. 
get_required(Composition, Required) :- 
	Composition = pool(_, Required, _, _).

%% get_provided(?Composition, ?Provided)
% Extract from Composition the Provided graphs 
get_provided(Composition, Provided) :- 
	Composition = pool(_, _, Provided, _).

%% get_associated_graphs(+Composition, -Graphs)
% Extract from composition ALL the handled (public) graph
get_associated_graphs(Composition, Graphs) :- 
	get_required(Composition, Required), 
	get_provided(Composition, Provided),
	append(Required, Provided, All), sort(All, Graphs).

%%%%
%% Composition execution
%%%%

%% play(+Composition)
% Play the given composition. Output artefacts are pushed in the database after
% the execution.
play(Composition) :-
	build_context(Composition, Context_in),
	execute_contents(Composition, Context_in, Context_out),
	push_public_output(Composition, Context_out),!.

%% execute_contents(+Composition, +Ctx_in, -Ctx_out)
% Execute the content of Composition, using Ctx_in as execution context. 
% Ctx_out is unified with the execution context obtained AFTER the execution.
execute_contents(Composition, Ctx_in, Ctx_out) :- 
	Composition = pool(_,_,_,Calls),
	exec_scheduled_calls(Calls, Ctx_in, Ctx_out).

%% exec_scheduled_calls(+Call_list, +Ctx_in, -Ctx_out)
% Execute the contents of Call_list, using Ctx_in as input. Calls are scheduled
% according to the =|findCandidates/3|= predicate.
exec_scheduled_calls([], Ctx, Ctx) :- !.
exec_scheduled_calls(L, Ctx_in, Ctx_out) :- 
	find_candidates(L, Ctx_in, Candidates), 
	subtract(L, Candidates, L_prime),
	execute_calls(Candidates, Ctx_in, Outputs), 
	handle_execution_outputs(Outputs, Graph_list),
	synchronize_context(Ctx_in, Graph_list, Tmp),
	exec_scheduled_calls(L_prime, Tmp, Ctx_out). % Recursive call

%% find_candidates(+Call_list, +Context, -Elements)
% Extract from Call_list the candidates ready to be executed in a given Context.
% These candidates are unified with Elements.
find_candidates([], _, []) :- !.
find_candidates(Call_lst, Context, Elements) :-
	findall(Call, 
	        composition:is_candidate(Call_lst, Context, Call), Elements).

%% is_candidate(+Call_list, +Context, -Call)
% Call is unified with a member of Call_list that relies on graphs available in
% Context. In other word, a Call is a good candidate for execution since all 
% its required graphs are computed and available in Context.
is_candidate(Call_list, Context, Call) :- 
	member(Call, Call_list), Call = gcoke_call(_, Input_bindings, _), 
	findall(I,(  member(binding(_,graph(I)), Input_bindings)
                   | ( member(binding(_,graph_set(L)), Input_bindings),
		       member(I, L))), Raw_ins),
	sort(Raw_ins, Input_graph_names),
	findall(G, (member(I, Input_graph_names), 
	            composition:pull_from_context(Context, I, G)), Raw_graphs),
	sort(Raw_graphs, Available_graphs), 
	length(Input_graph_names, Length), length(Available_graphs, Length).

%% execute_calls(+Call_list, +Context, -Outputs)
% Execute all the calls contained in Call_list, in the given Context. Outputs
% is unified with the output (see algorithm.pl) of each call contained in 
% Call_list. The execution order is not ensured.
%
% @tbd use threads to (really) parallelize computations.
execute_calls([],_,[]) :- !. 
execute_calls([Call|Others], Context, Outputs) :- 
	algorithm:execute(Call, Context, Call_outputs),
	execute_calls(Others, Context, Others_outputs),
	append(Call_outputs, Others_outputs, Outputs).

%% handle_execution_outputs(+Outputs, -Graphs)
% Based on the content of Outputs, unify Graphs with the graphs associated to
% the execution of the asked actions, on the asked graphs.
%
% @tbd check that concurrent action sequence will not interact.
handle_execution_outputs([], []) :- !.
handle_execution_outputs([H|T], [Graph|Others]) :-
	algorithm:handle_output(H, Graph_init, Actions_init),
	% findall other actions to be done on Graph_init
	findall(Actions, (member(O, T), 
	        algorithm:handle_output(O, Graph_init, Actions)),
	        Actions_others),
	append([Actions_init|Actions_others], Action_sequence),
	% Execute the combined action set
	% FIXME: Should be checked before executed ...
	engine:do_sequence(Graph_init, Action_sequence, Graph, _), 
	% Recursive call
	findall(O, ( member(O, T), 
	             algorithm:handle_output(O, Graph_init,_)), Remove),
	subtract(T, Remove, Purged),
	handle_execution_outputs(Purged, Others).

%% synchronize_context(+Context_in, +Graph_list, -Context_out)
% Push the contents of Graph_list in Context_in, unified in Context_out.
synchronize_context(Context, [], Context) :- !.
synchronize_context(Context_in, [H|T], Context_out) :- 
	push_into_context(H, Context_in, Tmp),
	synchronize_context(Tmp, T, Context_out).

%%%%
%% Composition execution context
%%%%

%% build_context(+Composition, -Context)
% Initialize an execution Context (basically a lost of graphs) for a given 
% Composition. Context contains all the graphs required by Composition.
build_context(Composition, Context) :- 
	Composition = pool(_,Inputs,_,_),
	findall(G, (member(I, Inputs), graph:pull_from_db(I, G)), Context).

%% pull_from_context(+Context, +Graph_name, -Graph)
% Extract a Graph named Graph_name in Context.
pull_from_context(Context, Graph_name, Graph) :- 
	member(Graph, Context), graph:read_name(Graph, Graph_name).

%% push_into_context(+Graph, +Context_in, -Context_out)
% Push Graph into Context_in, and unify the result in Context_out. If Context_in
% contains a graph G' that uses the same name as Graph's one, G' is silenty 
% deleted before the insertion of Graph.
push_into_context(Graph, Context_in, Context_out) :- 
	graph:read_name(Graph, Name),
	(member(G, Context_in), graph:read_name(G, Name) ->
	    delete(Context_in, G, Context) ; Context = Context_in),
	Context_out = [Graph |Context].

%% push_public_output(+Composition, +Context)
% Push into the global database all the "provided" graphs defined in 
% Composition, based on Context contents.
push_public_output(Composition, Context) :- 
	Composition = pool(_,_,Outputs,_),
	findall(O, ( member(O, Outputs), 
	             composition:pull_from_context(Context,O, Graph), 
		     graph:push_into_db(Graph)), Outputs).

%%%%
%% Picture Export (FIXME: this code is definitively ugly)
%%
%% Remark: Always code as if the guy who ends up maintaining your code will be 
%%         a violent psychopath who knows where you live. (Rick Osborne)
%%%%

%% show_all
% Show ALL the described composition as a graph
show_all :- 
	findall(C, composition:pull_from_db(_,C), Composition_lst),
	show(Composition_lst).

%% show(+Composition_list)
% show a graphical representation of the artefact and algorithms involved in 
% the given Composition_list
show(Composition_list) :- 
	tmp_file('compo_to_png',Tmp), as_picture(Composition_list, png, Tmp), 
	getenv_or_default('GCOKE_OPEN','open',E),
	swritef(Cmd,'%w %w.png', [E, Tmp]), channels:push(trace(shell),Cmd,[]),
        shell(Cmd).

% as_picture(+Compo_lst, +Format, +F)
% Write a picture (using +Format, e.g., png, pdf) representing +Compo_lst in the
% file %F (automatically postfixed with '.Format').
as_picture(Composition_list, Format, F) :- %% TODO: refactor
	as_dot(Composition_list, Dot_code), tmp_file('compo_to_dot',Tmp), 
	open(Tmp, write, Stream), write(Stream, Dot_code), close(Stream),
        getenv_or_default('GCOKE_DOT','dot',E),	expand_file_name(F,[File]),
        swritef(Cmd,'%w -T%w %w > %w.%w', [E, Format, Tmp, File, Format]), 
        channels:push(trace(shell),Cmd,[]), shell(Cmd).

% as_dot_file(+List, +File)
% Transform all the composition contained in List as dot code, and write the
% result of such a transformation in File.
as_dot_file(Composition_list, File) :- 
	as_dot(Composition_list, Dot_code), 
	open(File, write, Stream), write(Stream, Dot_code), close(Stream).

% as_dot(+Composition_list, -Dot_code)
% Build the Dot_code associated to Composition_list
as_dot(Composition_list, Dot_code) :- 
	external_graphs_as_dot(Composition_list, Ext_code),
	composition_list_as_dot(Composition_list, Int_code),
	swritef(Dot_code,'digraph compo {\n  fontname="Courier";\n  node [fontname="Courier", fontsize=12];\n  edge [fontname="Courier",fontsize=10];\n\n%w;\n\n%w\n}',[Ext_code, Int_code]).

% external_graphs_as_dot(+Compo_lst, -Code)
% Produce the graphviz Code associated to each as "external" artefacts (that 
% is, publicly available) inviled in Composition_lst.
external_graphs_as_dot(Composition_lst, Ext_code) :- 
	findall(G_lst,( member(C, Composition_lst), 
                	composition:get_associated_graphs(C,G_lst) ), Ext_lst),
	flatten(Ext_lst, Ext_flat), sort(Ext_flat, Externals),
	findall(C, (member(E, Externals), swritef(C,'%w [style=filled, fillcolor=lemonchiffon, label="%w"]',[E,E])), Code_lst),
	swrite_list(Code_lst, ';\n','  ', Ext_code).

% composition_list_as_dot(+Composition_list, -Dot_code)
% Unify with Dot_code the graphviz code associated to the content of 
% Composition_list.
composition_list_as_dot(Composition_lst, Dot_code) :- 
	findall(Code,(member(C, Composition_lst),
	              composition:as_dot_cluster(C,Code)),Clusters),
	swrite_list(Clusters, '\n', '', Dot_code).

% as_dot_cluster(+Composition, -Dot_code)
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

% dot_write_list(+List, -String)
% Write all elements in List into String. Each element is printed on a line 
% ended by a semi-colon symbol.
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

% pretty ugly, isn'it?
call_as_dot(Name, Externals, Call, Nodes, Int_edges, Ext_edges) :- 
	Call =  gcoke_call(Algo, Ins, Outs), 
	% Algorithm node,
	gensym(algo_,Id),
 	swritef(Algo_node,
 	  '%w [shape="record", style="filled",fillcolor="grey89",label="%w"]',
 	        [Id, Algo]),% trace,
	% Inputs & Outputs
	gen_call_params(in,Externals,Name,Id,Ins,Ins_ns,Ins_int_es,Ins_ext_es),
	gen_call_params(out,Externals,Name,Id,Outs,Outs_ns,Outs_int_es,
	                Outs_ext_es),
	% the all together
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
	% External graph
	Param = binding(Param_name, graph(Graph_name)), member(Graph_name,Exts),
	build_edge(Dir, solid, normal, Graph_name, Algo_id, Param_name, Edge),
	Nodes = [], Int_edges = [], Ext_edges = [Edge].

handle_parameter(Param,Name,Exts,Algo_id,Dir,Nodes,Int_edges,Ext_edges) :- 
	% Internal graph
	Param = binding(Param_name, graph(Graph_name)),
	\+ member(Graph_name, Exts),
	swritef(Node_id, '%w_%w', [Name, Graph_name]),
	swritef(Node,'%w [label="%w", style="dashed"]',
                [Node_id,Graph_name]),
	build_edge(Dir, dashed, open, Node_id, Algo_id, Param_name, Edge),
	Nodes = [Node], Int_edges = [Edge], Ext_edges = [].

handle_parameter(Param,_,_,Algo_id,Dir,Nodes,Int_edges,Ext_edges) :- 
	% Scalar term
	Param =  binding(Param_name, term(Scalar)),
	gensym(scalar, Scalar_id),
	swritef(Scalar_node, '%w [shape="note", style="filled", fillcolor="lightblue",label="\'%w\'"]', [Scalar_id, Scalar]),
        build_edge(Dir, dashed, open, Scalar_id, Algo_id, Param_name, Edge),
	Nodes = [Scalar_node], Int_edges = [Edge], Ext_edges = [].
	
handle_parameter(Param,_,Exts,Algo_id,Dir,Nodes,Int_edges,Ext_edges) :- 
	% Graph set
	Param = binding(Param_name, graph_set(Graph_lst)),
	gensym(set_node, Set_node_id),
	swritef(Set_node, '%w [shape="circle",label="",style="filled",fillcolor="black", width=0.1,fixedsize=true]', [Set_node_id]),
        build_edge(Dir, dashed, open, Set_node_id, Algo_id, Param_name, Set_edge),
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

%% push_into_db(+Composition)
% Store Composition in the fact database.
push_into_db(Composition) :- 
	Composition = pool(Name, Ins, Outs, Calls),
	del_from_db(Name), assert(stored(Name, Ins, Outs, Calls)).

%% pull_from_db(+Name, -Composition)
% Retrieve the Composition declared with its associated Name.
pull_from_db(Name, Composition) :- 
	stored(Name, Ins, Outs, Calls),
	Composition = pool(Name, Ins, Outs, Calls).

%%  del_from_db(+Name)
% delete the composition stored under the key Name in the fact database. A 
% non-existing composition will be silently ignored.
del_from_db(Name) :- 
	stored(Name,_,_,_) -> retract(stored(Name,_,_,_)); true.
