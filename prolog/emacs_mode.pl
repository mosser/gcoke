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

:- module(emacs_mode, [graph_show/1, graph_to_dot/2]).

:- dynamic gcoke_emacs_registered_predicate/2.


%%%%
%% Predicate registration
%%%%
register(Label, Predicate) :- 
	assert(gcoke_emacs_registered_predicate(Label, Predicate)).

%%%%
%% Graphviz interaction: ->Graph and ->Dot transformations
%%%%

%% Display a given graph to the user screen.
graph_show(Graph_name) :- 
	\+ gcoke_emacs_registered_predicate(graph_show, _),
	dot:show(Graph_name, []). %% default visualization
graph_show(Graph_name) :- 
	gcoke_emacs_registered_predicate(graph_show, Pred), !,
	call(Pred, Graph_name).   %% custom visualization

%% Transform a given graph into dot code, sotred in a given file.
graph_to_dot(Graph_name, Dot_file) :- 
	\+ gcoke_emacs_registered_predicate(graph_to_dot, _),
	dot:write_dot_source(Graph_name, [], Dot_file).
graph_to_dot(Graph_name, Dot_file) :- 
	gcoke_emacs_registered_predicate(graph_to_dot, Pred), !,
	call(Pred, Graph_name, Dot_file).