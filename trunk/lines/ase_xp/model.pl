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
:- module(mde,[]).

%% Illustrative example associated to ASE 11 paper

%%%%
%% Adaptations
%%%%

:- algorithm:declare(mde:s, [in(model, graph), out(output)], [[model, output]]).
s(Model, Output) :- 
	findall(actions:add_edge(R,sc,[[kind,inheritance]]), 
	        ( mde:is_root(Model,Root),graph:read_name(Root,R) ), 
		RootLinks), 
	Output = [actions:add_node(sc, [[kind,class]]) | RootLinks].

:- algorithm:declare(mde:a, [in(model, graph), out(output)], [[model, output]]).
a(Model, Output) :- 
	findall(actions:add_edge(R,ac, [[kind, aggregate], [refname, S]]), 
	        ( mde:is_root(Model,Root),graph:read_name(Root,R),gensym(a,S) ),
		RootLinks), 
	Output = [actions:add_node(ac, [[kind,class]]) | RootLinks].

%%%%
%% Helper predicates
%%%%

%a root class is a class that does not have any superclass.
is_root(Model, Class) :- 
	graph:has_for_node(Model, Class),
	graph:read_name(Class, Name),
	findall(Sub, queries:get_edge_by_properties(Model,[[kind,inheritance]],
	                                            Sub,_,_), 
		SubClasses), \+ member(Name, SubClasses).

%%%%
%% Graphical Representation
%%%%

graph_config(L) :- 
        L = ['fontname = Courier', 'node [fontname="Courier", shape="box"]', 
             'edge [fontname="Courier",dir="both"]', 'rankdir=BT'].

edge_handler(_, Edge, [[arrowtail, 'none'],[arrowhead, 'empty']]) :- 
        graph:read_properties(Edge, kind, ['inheritance']).
edge_handler(_, Edge, [[arrowtail, 'odiamond'], [arrowhead, 'open'], 
	               [label, N] ]) :- 
        graph:read_properties(Edge, kind, ['aggregate']),
	graph:read_properties(Edge, refname, [N]).

dot_builder(B) :- 
        B = [[graph_config,mde:graph_config], [edge_handler,mde:edge_handler]].

draw(Model, Format, File) :- 
        dot_builder(B), dot:draw(Model, B, Format, File).

%% Interface with the Emacs gCoKe Mode (visualization mechanisms refinement)
graph_show(Model) :- 
        dot_builder(B), dot:show(Model, B).
graph_to_dot(Model, DotFile) :- 
        dot_builder(B), dot:write_dot_source(Model, B, DotFile).

:- emacs_mode:register(graph_show, mde:graph_show), 
   emacs_mode:register(graph_to_dot, mde:graph_to_dot).
