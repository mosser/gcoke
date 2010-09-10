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

:- module(channels, []).

%%%%
%% What is a channel?
%%   - Channel is a term defined as channel_name(level_name).
%%
%% How does it work?
%%   - Prolog code push messages on different channels
%%   - One can subscribe to a channel and then receive the associated messages
%%   - Received messages are printed on stdout 
%%   - Others messages are simply lost (reduced overhead).
%%
%% How to subscribe/unsubscribe to a channel?
%%   - Simply use a 'channels:(un)subscribe(channel_name(level_name))' rule. 
%%
%% How to know which channel to subscribe to?
%%   - gCoke artefacts may (its not mandatory) declare their channels using a 
%%     channels:declare(Channel) rules. However, it's simply informational.
%%   - Call channels:list_declared_channels to print athe list of declared
%%     channels.
%%
%% /!\ Remark: Implementation detail
%%   - gCoke channels mechanisms intensively relies on SWI-PL debug framework.
%%     However, one can use simple 'write' mechanisms to implement it.
%%%%

%% subscribe/1: subscribe(+Channel)
%  One can use '_' as level_name to subscribe to ALL the message 
%%  sent on channel_name.
subscribe(Channel) :- 
	gwf('%% Subscribing to channel %d\n',[Channel]),
	debug(Channel).

%% unsubscribe/1: unsubscribe(Channel)
% exact contrary of subscribe.
unsubscribe(Channel) :- 
	gwf('%% Unsubscribing to channel %d',[Channel]),
	nodebug(Channel).

%% push/4: push(+Channel, +Format, +Args)
% Push a message on a given Channel. The message is built based on a Format 
% string and the asosciated Args.
% /!\ Remark: this implementation use 'format' to build the message. That is, 
%             the Format string uses ~-based data (instead of %-based for 
%             writef)
push(Channel, Format, Args) :- 
	catch((swritef(Str, '[%w] %w', [Channel, Format]), 
               string_to_atom(Str, Atom), debug(Channel, Atom, Args)),_,true).

%%%%
%% Channel Declarations
%%%%
:- dynamic gcoke_declared_channel/1.

%% declare/1: declare(+Channel)
% declare Channel as an available channel in gcoke.
declare(Channel) :- 
	assert(gcoke_declared_channel(Channel)).

%% list_declared_channels/0: list_declared_channels
% print all the declared channels on stdout.
list_declared_channels :- 
	findall(C,gcoke_declared_channel(C), Raw_channels),
	sort(Raw_channels, Channels),
	write('###'), nl, write_list(Channels,'# ',',\n'), nl, write('###'), nl.