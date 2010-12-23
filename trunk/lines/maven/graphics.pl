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
:- module(maven_graphics, []).

%%%%
%% Graphical customisation for Maven composition
%%%%

graph_config(L) :- 
        L = ['fontname = Courier', 
	     'node [fontname="Courier", shape="record", style="filled", fillcolor="white"]', 
	     'edge [fontname="Courier", dir="both", arrowtail="none"]'].

graph_handler(Graph, [[label, Label]]) :-
	graph:read_properties(Graph, name, [Label]).


node_handler(_, Node, _, Params) :- 
	graph:read_properties(Node, kind, [Kind]),
	graph:read_properties(Node, name, [Name]),
 	(Kind = 'pom' -> O = [[fillcolor, lemonchiffon]], ! ; O = []),
	append([[label, Name]], O, Params).

edge_handler(_, Edge, Params) :- 
	graph:read_properties(Edge, scope, [Scope]), 
	( Scope = test -> Params = [[style, dashed]], !; 
	    Scope = compile -> Params = [[arrowhead, dot]],!;
	    Params = [] ).

dot_builder(B) :- 
	B = [[graph_config,  maven_graphics:graph_config],
	     [graph_handler, maven_graphics:graph_handler],
	     [node_handler,  maven_graphics:node_handler],
	     [edge_handler,  maven_graphics:edge_handler]].

draw(Graph, Format, File) :- 
	dot_builder(B), dot:draw(Graph, B, Format, File).

%% Interface with the Emacs gCoKe Mode (visualization mechanisms refinement)
graph_show(Graph) :- 
	dot_builder(B), dot:show(Graph, B).
graph_to_dot(Graph, DotFile) :- 
	dot_builder(B), dot:write_dot_source(Graph, B, DotFile).

:- emacs_mode:register(graph_show, maven_graphics:graph_show), 
   emacs_mode:register(graph_to_dot, maven_graphics:graph_to_dot).