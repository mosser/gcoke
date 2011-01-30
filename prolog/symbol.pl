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
:- module(symbol, []).
/** <module> Unique Symbols creation 
*/

%% build(+Graph, +Prefix, -Symbol)
% Symbol is unified with an atom starting with Prefix, UNUSED as node name 
% in Graph (the 'simple' gensym does not ensure such an unicity).
build(Graph, Prefix, Symbol) :- 
	gensym(Prefix, Tmp), 
	(queries:get_node_by_name(Graph, Tmp, _) -> 
	    build(Graph, Prefix, Symbol) ; Symbol = Tmp). 


