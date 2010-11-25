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
	P = gcoke_composition_pool(Name, In_graph_names, Out_graph_names, []).

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
	Pool = gcoke_composition_pool(N, Ins, Outs, Calls),
	Result = gcoke_composition_pool(N, Ins, Outs, [Call|Calls]).

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
	Call = gcoke_algorithm_call(Algorithm, Input_params, Output_params).

%% build_parameter_binding/3: build_parameter_binding(+Name,+Artefact,-Binding)
% Unify Binding with the gcoke construction that binds Artefact with the 
% algorithlm parameter called Name.
build_parameter_binding(Parameter_name, Artefact, Binding) :- 
	Binding = gcoke_parameter_binding(Parameter_name, Artefact).

%%%%
%% Composition dependencies (based on the database content)
%%%%


%%%%
%% Call Scheduling
%%%%

%%%%
%% Picture Export (TODO: refactor with trace and/or dot custom export)
%%%%

show(Composition_list) :- %% TODO: refactor
	tmp_file('compo_to_png',Tmp), as_picture(Composition_list, png, Tmp), 
	getenv_or_default('GCOKE_OPEN','open',E),
        swritef(Cmd,'%w %w.png', [E, Tmp]), channels:push(trace(shell),Cmd,[]),
        shell(Cmd).

as_picture(Composition_list, Format, F) :- %% TODO: refactor
	as_dot(Composition_list, Dot_code), tmp_file('compo_to_dot',Tmp), 
	open(Tmp, write, Stream), write(Stream, Dot_code), close(Stream),
        getenv_or_default('GCOKE_DOT','dot',E),
	expand_file_name(F,[File]),
        swritef(Cmd,'%w -T%w %w > %w.%w', [E, Format, Tmp, File, Format]), 
        channels:push(trace(shell),Cmd,[]),
        shell(Cmd).

as_dot(Composition_list, Dot_code) :- 
	findall(C,( member(Compo, Composition_list),
	            composition:as_dot_cluster(Compo, C) ), Raw),
	swrite_list(Raw,'\n', '', Internal_code),
	swritef(Dot_code,'digraph compo {\n fontname="Courier"; node [fontname="Courier"]; edge [fontname="Courier"];\n%w\n}',[Internal_code]).

as_dot_cluster(Composition, Dot_code) :- 
	Composition = gcoke_composition_pool(Name, Ins, Outs, Calls),
	flatten([Ins, Outs], External),
	call_list_as_dot(External, Calls, Nodes, Edges), 
	swrite_list(Nodes, ';\n', '', Internal_nodes),
	swrite_list(Edges, ';\n', '', Edges_descr),
	swritef(Dot_code, 'subgraph cluster_%w {\n label="%w";\n%w\n}\n%w', 
                [Name, Name, Internal_nodes, Edges_descr]).

call_list_as_dot(_,[], [], []).
call_list_as_dot(Ext, [Call|Others], Final_nodes, Final_edges) :- 
	call_as_dot(Ext, Call, Nodes, Edges),
	call_list_as_dot(Ext, Others, Other_ns, Other_es),
	flatten([Nodes, Other_ns], Final_nodes),
	flatten([Edges, Other_es], Final_edges).

call_as_dot(_, gcoke_algorithm_call(Algo, Ins, Outs), Nodes, Edges) :- 
	%% Algorithm call box
	gensym(algo_,Id),
	swritef(Algo_box,
	  '%w [shape="record",style="filled",fillcolor="lightgrey",label="%w"]',
	        [Id, Algo]),
	Nodes = [Algo_box],
	%% related 
	findall(Req,( member(gcoke_parameter_binding(Param, graph(G)), Ins),
	              swritef(Req, '%w -> %w [label="%w"]', [G,Id,Param])), 
		Req_scalar_edges),
	findall(Prov,( member(gcoke_parameter_binding(Param, graph(G)), Outs),
	              swritef(Prov, '%w -> %w [label="%w"]', [Id,G,Param])), 
		Prov_scalar_edges),
	flatten([Req_scalar_edges, Prov_scalar_edges],Edges).

%%%%
%% Persistence API (FIXME: inspired by Graph unit ... refactor redundancy?)
%%%%
:- dynamic stored/4.

%% FIXME
push_into_db(Composition) :- 
	Composition = gcoke_composition_pool(Name, Ins, Outs, Calls),
	del_from_db(Name), assert(stored(Name, Ins, Outs, Calls)).
%% FIXME
pull_from_db(Name, Composition) :- 
	stored(Name, Ins, Outs, Calls),
	Composition = gcoke_composition_pool(Name, Ins, Outs, Calls).

%% del_from_db/1: del_from_db(+Name)
% delete the composition stored under the key Name in the fact database. A 
% non-existing composition will be silently ignored.
del_from_db(Name) :- 
	stored(Name,_,_,_) -> retract(stored(Name,_,_,_)); true.
