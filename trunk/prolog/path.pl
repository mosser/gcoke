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

:- module(path,[]).

%%%%
%% Path existence
%%%%
% Warning: exists/3 is more efficient than exists/4.

%% exists/3: exists(+Graph, +Start, +End)
% Evaluated as true if a path exists between a node named Start and a node 
% named End in Graph. Cyclic graphs are handled (see exists_internal).
exists(Graph, Start_name, End_name) :- 	
	exists_internal(Graph, Start_name, End_name, [Start_name]).

%% exists_internal/4: exists_internal(+G, +Start, +End, +Visited)
% Evaluated as true if a path exists between a node named Start and a node 
% named End in Graph (nodes are visited only once).
exists_internal(Graph, Start, End, _) :- 
	queries:get_edge_by_boundaries(Graph, Start, End, _), !.
exists_internal(Graph, Start, End, Visited) :- 
	queries:get_edge_by_boundaries(Graph, Start, Intermediate,_),
	\+ member(Intermediate, Visited), 
	Path = [Intermediate | Visited],
	path:exists_internal(Graph, Intermediate, End, Path).

%% exists/4: exists(+Graph, +Start, +End, +Key_values)
% Same as exists/3, but restricted to a given Key_values list expected in the 
% choosen path. 
exists(Graph, Start_name, End_name, Key_values_list) :- 	
	exists_internal(Graph, Start_name, End_name, 
	                Key_values_list, [Start_name]).

%% exists_internal/5: exists_internal(+G, +Start, +End, +Key_values, +Visited)
% same as exists_internal/4, but restricted to expected props (described as 
% Key_values list) in the edges.
exists_internal(Graph, Start, End, Props ,_) :- 
	queries:get_edge_by_properties(Graph, Props, Start, End, _), !.
exists_internal(Graph, Start, End, Props, Visited) :- 
	queries:get_edge_by_properties(Graph, Props, Start, Intermediate,_),
	\+ member(Intermediate, Visited), 
	Path = [Intermediate | Visited],
	path:exists_internal(Graph, Intermediate, End, Props, Path).


	