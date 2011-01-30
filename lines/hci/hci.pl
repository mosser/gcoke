%%%%                                                            %%%%
%%        HCI Composition specialization using gCoKe              %%
%%                                                                %%
%% @author   main    Sebastien Mosser  [ sm@gcoke.org ]           %%
%% @author   contrib Christian Brel    [ brel@polytech.unice.fr ] %%
%%%%                                                            %%%%
:- module(hci,[]).

%%%%               %%%%
%% Graph Constraints %%
%%%%               %%%%

%% Semantics: a button node is always linked to an action node.
% Implementation: precondition on an add_edge action
:- constraints:register_predicate(pre,'actions:add_edge',hci:button_to_action).
button_to_action(Frame, actions:add_edge(From, To, _)) :- 
	queries:get_node_by_name(Frame, From, Node_from),
	graph:read_properties(Node_from, kind, [button]) -> 
	  ( queries:get_node_by_name(Frame, To, Node_to),
	    graph:read_properties(Node_to, kind, [action]) )
          ; true.

%% Semantics: For all buttons, a connection with an action exists.
% Implementation: a graph invariant
:- constraints:register_invariant(hci:connected_buttons).
connected_buttons(Frame) :- 
	get_by_kind(Frame, button, Buttons), 
	forall(member(B, Buttons),
	       ( queries:get_edge_by_boundaries(Frame, B, A, _),
	         queries:get_node_by_name(Frame, A, Act_node),
	         graph:read_properties(Act_node, kind, [action]))).

get_by_kind(Frame, Kind, Element_names) :- 
	findall(N, (graph:has_for_node(Frame, X), 
	            graph:read_properties(X, kind, [Kind]),
		    graph:read_name(X, N)), Element_names).

%%%%                    %%%%
%% Composition Algorithms %%
%%%%                    %%%%

%%
%%%% Example #2: parallel composition
%%

:- algorithm:declare(hci:parallel, [in(frame, graph), out(result)], 
	             [[frame, result]]).

parallel(Frame, Out) :- 
	%% Find all the defined buttons
	get_by_kind(Frame, button, Tmp),
	%% build the unified button ID
	symbol:build(Frame, b, Symbol),
	%% Ask for unification
	Out = [actions:unify_node(Tmp,Symbol)].



%%
%%%% Example #3: field unification (name-based)
%%

:- algorithm:declare(hci:unify_fields, [in(frame, graph), out(result)], 
	             [[frame, result]]).
unify_fields(Frame, Out) :- 
	findall(C, hci:field_equivalence_class(Frame,C), Raw_classes),
	sort(Raw_classes, Classes), %% remove duplicate equivalence classes
	findall(actions:unify_node(Eq,Symb), 
	        ( member(Eq,Classes), symbol:build(Frame, f,Symb) ), Out).

field_equivalence_class(G, Equivalence_class) :- 
	% Let N a node in G, tagged as a 'field', and named 'Name'
	graph:has_for_node(G, N), graph:read_properties(N, kind, [field]), 
	graph:read_properties(N, name, [Name]), 
	% L contains the identifier of all nodes equivalent to N (name-based)
	findall(I, (queries:get_node_by_properties(G, [[name, Name]], Np),
	            graph:read_name(Np,I)), L), 
	% Equivalence classes singleton are automatically rejected
	length(L, Length), Length > 1, sort(L, Equivalence_class).

%%
%%%% Example #4: sequential composition
%%
:- algorithm:declare(hci:sequence, 
	             [in(first, graph), in(second, graph), out(result)], 
	             [[first, result]]).
sequence(First, Second, Out) :- 
	% Retrieving Output (single) element and the Action of the First frame
	findall([O,A], hci:get_output(First, O,A), Output_lst),
	(Output_lst = [[Output,Act_first]] -> true; throw(error)),
	% Retrieving Input (single) element and the Action of the Second frame
	findall([I,A], hci:get_input(Second, I,A), Input_lst),
	(Input_lst = [[Input, Act_second]] -> true ; throw(error)),
	% Retrieving the bindings
	get_binding(First, Act_first, Output, Bind_out),
	get_binding(Second, Input, Act_second, Bind_in),
	% Retrieving the repudiated button
	get_by_kind(Second, button, [Button]),
	% Creating the actions to be executed
	Out=[ actions:del_edge(Act_first, Output,[]), actions:del_node(Output),
	      actions:dump(Second),
	      actions:del_edge(Input, Act_second,[]), actions:del_node(Input),
	      actions:add_edge(Act_first, Act_second, 
	                       [[from,Bind_out],[to,Bind_in]]),
	      actions:del_edge(Button,Act_second,[]), actions:del_node(Button)].

get_input(Frame, Input_name, Action_name) :- 
	graph:has_for_node(Frame, Action), 
	graph:read_properties(Action, kind, [action]), 
	graph:read_name(Action, Action_name),
	queries:get_edge_by_boundaries(Frame, Input_name, Action_name, _),
	queries:get_node_by_name(Frame, Input_name, Input),
	graph:read_properties(Input, kind, [field]).

get_output(Frame, Output_name, Action_name) :- 
	graph:has_for_node(Frame, Action), 
	graph:read_properties(Action, kind, [action]), 
	graph:read_name(Action, Action_name),
	queries:get_edge_by_boundaries(Frame, Action_name, Output_name, _),
	queries:get_node_by_name(Frame, Output_name, Output),
	graph:read_properties(Output, kind, [label]).

get_binding(Frame, Source, Target, Binding_value) :- 
	queries:get_edge_by_boundaries(Frame, Source, Target, Edge),
	graph:read_properties(Edge, binding, [Binding_value]).


%%%%                     %%%%
%% Graphical Customization %%
%%%%                     %%%%

%%
%%%% Graph display implementation
%%

settings(L) :- 
        L = ['fontname = "Courier"', 'edge [fontname="Courier"]',
	     'node [shape="record", fontname="Courier", style = "filled", fillcolor = "white"]', 
	     'rankdir=LR', 'bgcolor = "grey95"'].

graph_settings(Graph, [[label, Label]]) :-
	graph:read_name(Graph,Name), graph:read_properties(Graph,kind,[Kind]),
	swritef(Label, '%w <%w>', [Kind, Name]).

draw_node(_, Node, _, L) :- 
	graph:read_properties(Node, kind, [action]),
	graph:read_properties(Node, target, [Target]),
	L = [[label, Target], [shape, ellipse], [fillcolor, lightgrey]].
draw_node(_, Node, _, [[shape, note], [label, Node_id]]) :- 
	graph:read_properties(Node, kind, [button]),
	graph:read_name(Node, Node_id).
draw_node(_, Node, _, [[label, Label]]) :- 
	\+ graph:read_properties(Node, kind, [action]),
	graph:read_properties(Node, name, [Name]),
	graph:read_name(Node, Node_id),
	swritef(Label, '%w | %w', [Node_id, Name]).

draw_edge(_, Edge, [[arrowhead, dot], [style, dashed]]) :- 
	graph:get_properties(Edge, []).
draw_edge(_, Edge, [[label, Binding]]) :- 
	graph:read_properties(Edge, binding,[Binding]).


draw_edge(_, Edge, [[style, dashed], [label, Label]]) :- 
	graph:read_properties(Edge, from, [From]),
	graph:read_properties(Edge, to, [To]),
	swritef(Label, '%w -> %w', [From, To]).

raw(Frame, Code) :- 
	get_by_kind(Frame, button, Buttons), 
	findall(R, ( member(B, Buttons), 
	             queries:get_edge_by_boundaries(Frame, B, A, _),
		     swritef(R,'{rank="same"; %w; %w;}',[B,A])), Directives),
	swrite_list(Directives, '\n', '\t', Code).

%%
%%%% Interface with the Emacs gCoKe Mode
%%

custom(B) :- 
	B = [[graph_config,hci:settings], [graph_handler,hci:graph_settings],
	     [node_handler,hci:draw_node], [edge_handler,hci:draw_edge], 
	     [custom_code, hci:raw] ].

show(F) :- custom(B), dot:show(F, B).

to_dot(F, Dot) :- custom(B), dot:write_dot_source(F, B, Dot).

:- emacs_mode:register(graph_show, hci:show), 
   emacs_mode:register(graph_to_dot, hci:to_dot).


