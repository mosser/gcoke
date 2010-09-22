%%%%
%% This file is part of gCoke [ http://www.gcoke.org ]
%%
%% Copyright (C) 2010-  Sebastien Mosser
%%
%% gCoke is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as 
%% published by the Free Software Foundation; either version 2 of 
%% the License, or (at your option) any later version.
%%
%% gCoke is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public 
%% License along with gCoke; if not, write to the Free Software Foundation,
%% Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% @author   Main    Sebastien Mosser  [ sm@gcoke.org ]
%%%%
:- module(trace,[]).


%%%%
%% Pebble-based system.
%%%%

%% build_empty/1: build_empty(-T):
% unify T with an empty trace, unique in the system (We hope, but it should).
buid_empty(gcoke_trace(Id,[])) :- 
	gensym(gtrace_,Functor), get_time(Timestamp), 
	Id =.. [Functor, Timestamp].

%% read_identifier/2: read_identifier(+T, -Id):
% Unify Id with the (assumed) unique identifier associated to T.
read_identifier(gcoke_trace(Id,_),Id).

%% push_pebble/5: push_pebble(+Trace, +Origin, +Destination, +Message, -Trace')
% Unify Trace' with Trace, enhanced by a new peeble build from Origin to
% Destination, associated to Message.
push_peeble(Trace, Origin, Destination, Message, Trace_prime) :- 
	Trace = gcoke_trace(Id,L),
	Trace_prime = gtrace(Id,[pebble(Origin, Destination, Message)|L]).
	


explore(Trace, Origin, Destination, Message) :- 
	member(pebble(Origin, Destination, Message), Trace).
