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
:- module(automata_graphics, []).

%%%%
%% Graphical customisation for Automata composition
%%%%

graph_config(L) :- 
        L = ['fontname = Courier', 'node [fontname="Courier", shape="circle"]', 
	     'edge [fontname="Courier"]', 'rankdir=LR'].

graph_handler(Graph, [[label, Label]]) :-
	graph:read_properties(Graph, regexp, [RegExp]),
	swritef(Label, '%w recognizer', [RegExp]).

custom_code(Graph, Custom) :- 
	queries:get_node_by_properties(Graph, [[kind, start]], Start), !,
	graph:read_name(Start, Name),
	swritef(Custom, 'start_%w [shape="none",label="",height=0,width=0];\n start_%w -> %w;', 
                [Name, Name, Name]).

node_handler(_, Node, _, Params) :-
	graph:read_name(Node,Name), graph:read_properties(Node, kind, Kinds),
 	(member(K,Kinds), K = final -> O = [[shape, doublecircle]],!; O = []),
	append([[label, Name],[style, filled], [fillcolor, lightgrey]], 
	       O, Params).

edge_handler(_, Edge, [[label, Label]]) :- 
	graph:read_properties(Edge, symbol, Symbols), 
	swrite_list(Symbols, ',', '', Label).

dot_builder(B) :- 
	B = [[graph_config,  automata_graphics:graph_config],
	     [graph_handler, automata_graphics:graph_handler],
	     [custom_code,   automata_graphics:custom_code],
	     [node_handler,  automata_graphics:node_handler],
	     [edge_handler,  automata_graphics:edge_handler]].

draw(Automata, Format, File) :- 
	dot_builder(B), dot:draw(Automata, B, Format, File).

%% Interface with the Emacs gCoKe Mode (visualization mechanisms refinement)
graph_show(Automata) :- 
	dot_builder(B), dot:show(Automata, B).
graph_to_dot(Automata, DotFile) :- 
	dot_builder(B), dot:write_dot_source(Automata, B, DotFile).

:- emacs_mode:register(graph_show, automata_graphics:graph_show), 
   emacs_mode:register(graph_to_dot, automata_graphics:graph_to_dot).