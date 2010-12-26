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
:- module(trace,[]).
:- channels:declare(trace(shell)).

%%%%
%% Model instantiation
%%%%

%% build_empty/1: build_empty(-T):
% unify T with an empty trace, unique in the system (We hope, but it should).
build_empty(gcoke_trace(Id,[])) :- 
	gensym(gtrace_,Functor), get_time(Timestamp), 
	Id =.. [Functor, Timestamp].

%% read_identifier/2: read_identifier(+T, -Id):
% Unify Id with the (assumed) unique identifier associated to T.
read_identifier(gcoke_trace(Id,_), Id).

%% merge/2: merge(+Trace_set, -Result)
% Unify Result with a trace model that contains all the traces given in the 
% given Trace_set.
merge(Trace_set, Result) :- 
	build_empty(Empty_trace), read_identifier(Empty_trace, Id),
	findall(L, (member(T,Trace_set),T = gcoke_trace(_,L)), Contents),
	flatten(Contents, Flattened), Result = gcoke_trace(Id, Flattened).

%%%%
%% Pushing data into a Trace model.
%%%%

%% push/5: push(+Trace, +Origin, +Destination, +Message, -Trace')
% Unify Trace' with Trace, enhanced by a new peeble build from Origin to
% Destination, associated to Message. (1 -> 1 trace)
push(Trace, Origin, Destination, Message, Trace_prime) :- 
	Trace = gcoke_trace(Id,L),
	Trace_prime = gcoke_trace(Id,[pebble(Origin, Destination, Message)|L]).

%% multi_push/5: multi_push(+Trace, +Origins, +Destination, +Message, -Trace')
% Same as 'push', but works on a list of Origins (n -> 1 trace).
multi_push(Trace, [], _, _, Trace) :- !.
multi_push(Trace, [H|T], Destination, Message, Trace_prime) :- 
	push(Trace, H, Destination, Message, Tmp),
	multi_push(Tmp, T, Destination, Message, Trace_prime).

%%%%
%% Trace exploration
%%%%

%% has_for_ancestor/3: has_for_ancestor(+Trace, +Element, -Ancestor)
% Extract an Ancestor of an Element in Trace.
has_for_ancestor(Trace, Element, Ancestor) :- 
	Trace = gcoke_trace(_, Pebbles),
	member(pebble(Ancestor, Element, _), Pebbles).
has_for_ancestor(Trace, Element, Ancestor) :- 
	Trace = gcoke_trace(_, Pebbles),
	member(pebble(X, Element, _), Pebbles), 
	has_for_ancestor(Trace, X, Ancestor).

%% has_for_descendant/3: has_for_descendant(+Trace, +Element, -Descendant)
% Extract a Descendant of an Element in Trace.
has_for_descendant(Trace, Element, Descendant) :-
	Trace = gcoke_trace(_, Pebbles),
	member(pebble(Element, Descendant, _), Pebbles).
has_for_descendant(Trace, Element, Descendant) :-
	Trace = gcoke_trace(_,Pebbles), member(pebble(Element, X, _), Pebbles), 
	has_for_descendant(Trace, X, Descendant).

%%%%
%% Representation of Trace model (textual & graphical)
%%%%

%% as_text/2: as_text(+Trace, -Text):
% Unify Text with a textual representation of Trace.
as_text(gcoke_trace(_,L), Text) :- 
	findall(S,( member(pebble(X,Y,Z),L), 
	            swritef(S,'%w -> %w (%s)',[X,Y,Z])), Raw),
	sort(Raw, To_print), swrite_list(To_print, ',\n', '  - ', Text).

%% as_dot/2: as_dot(+Trace, -Dot_code):
% Unify Dot_code with a Graphviz representation of Trace
as_dot(gcoke_trace(_,L), Dot_code) :- 
	%% Nodes
	findall(X,(member(pebble(X,_,_),L)|member(pebble(X,_,_),L)), Raw_nodes),
	sort(Raw_nodes, Node_list),
	findall(S,( member(N,Node_list), trace:elem_to_dot(N,Id),
	            swritef(S,'%w [label="%w"]', [Id,N]) ),
	        Node_elems),
	swrite_list(Node_elems, '\n', '  ', Nodes),
	%% Edges
	findall(S,( member(pebble(X,Y,Z),L), trace:elem_to_dot(X,Id_x),
	            trace:elem_to_dot(Y, Id_y),
	            swritef(S,'%w -> %w [label="%s"];',[Id_x,Id_y,Z]) ), 
		Raw_edges),
	swrite_list(Raw_edges, '\n', '  ', Edges),
	%% Final graph code
	swrite_list(['fontname = Courier;', 'node [fontname = "Courrier", shape="record"];',
		     'edge [fontname = "Courrier"];', 'rankdir=LR;'],
		    '\n','  ',Head),
	swritef(Dot_code, 'digraph trace_model {\n%w\n%w\n%w\n}',
                [Head, Nodes, Edges]).

%% elem_to_dot/2: elem_to_dot(Elem,Dot_id)  
% transfom a named element Elem (e.g., g(e)) into a valid dot id (e.g., g_e)
elem_to_dot(Elem, Dot_id) :- 
	Elem =.. List, swrite_list(List,'_','',Dot_id).

%% as_picture/3: as_picture(+Trace, +Format, +File_name)
% Create a file File_name.Format, containing a graph. representation of Trace.
as_picture(Trace, Format, F) :- 
	as_dot(Trace, Dot_code), tmp_file('trace_to_dot',Tmp), 
	open(Tmp, write, Stream), write(Stream, Dot_code), close(Stream),
        getenv_or_default('GCOKE_DOT','dot',E),
	expand_file_name(F,[File]),
        swritef(Cmd,'%w -T%w %w > %w.%w', [E, Format, Tmp, File, Format]), 
        channels:push(trace(shell),Cmd,[]),
        shell(Cmd).

%% show/1: show(+Trace)
% Open a graphical visualizer which depicts the content of Trace.
show(Trace) :- 
	tmp_file('trace_to_png',Tmp), as_picture(Trace, png, Tmp), 
	getenv_or_default('GCOKE_OPEN','open',E),
        swritef(Cmd,'%w %w.png', [E, Tmp]), channels:push(trace(shell),Cmd,[]),
        shell(Cmd).

%%%%
%% Test
%%%%

test(Trace) :- 
 	build_empty(T1), 
 	push(T1, graph(a), b, "a to b", T2), push(T2, b, c, "b to c", T3),
 	multi_push(T3, [d,e], h, "(d,e) to h", T4), 
 	multi_push(T4, [f,g], i, "(f,g) to i", T5), 
 	multi_push(T5, [h,i], j, "(h,i) to j", T6), 
 	push(T6, j, k, "j to k", Trace).