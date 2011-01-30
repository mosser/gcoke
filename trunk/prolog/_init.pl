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


%%%%
%% Message Handling
%%%%

% Should the engine be silent? (false by default)
:- dynamic gcoke_silence/1.
gcoke_silence(false).

%% set_gcoke_silence(+Value)
% Assign Value (a boolean) to the gcoke silence level
set_gcoke_silence(Value) :- 
	retractall(gcoke_silence(_)), assert(gcoke_silence(Value)).

%% gw(+Str)
% means gcoke_write. Write Str if gcoke_silence is set to false.
gw(Str) :- 
	gcoke_silence(false) -> (write(Str),nl); true.

% gwf(+Format, +Args)
% like gw/1, but use writef instead of write.
gwf(Format, Args) :- 
	gcoke_silence(false) -> writef(Format, Args) ; true.

%%%%
%% Artefact load mechanisms
%%%%

%% load_pops(+Path)
% Load Path, as a Plain Old Prolog Source (POPS) file.
load_pops(Path) :-
	set_prolog_flag(verbose_load, false),
	gwf('%% Loading gCoKe source file  [%w] ...\n', [Path]), [Path].

%% load_source(+Path)
% load the gcoke module written in the file Path.
load_module(Path) :- 
	gwf('%% Loading gCoKe module file  [%w] ...\n', [Path]),
        use_module(Path).

%% load_lib(+Lib)
% load a swi-pl third party library, avoiding autoload. 
% /!\ Remark: use the 'check.' rule to identify autoloaded libraries
load_lib(Lib) :- 
	gwf('%% Loading third-part library [%w] ...\n', [Lib]),
	use_module(library(Lib)).

%%%%
%% Pretty printed header informations
%%%%

%% header
% Print header informations, according to adore_silence setting.
header :- 
	gw('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
        gw('%%           gCoKe Copyright (C) 2010 - ...          %%'),
        gw('%%          a Graph-based COmposition KErnel         %%'),
        gw('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
        gw('%%  Author:  Sebastien Mosser < sm@gcoke.org >       %%'),
        gw('%%  Website: http://www.gcoke.org                    %%'),
        gw('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
        gw('%%  This program comes with ABSOLUTELY NO WARRANTY.  %%'),
        gw('%%  This is free software, and you are welcome to    %%'),
        gw('%%  redistribute it under certain conditions.        %%'),
        gw('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%').
   
%%%%
%% Concrete instantiation of the gCoke engine
%%%%

%% load_libraries
% load all the needed swi-pl libraries
load_libraries :- 
	load_lib(error), load_lib(lists), load_lib(debug), load_lib(gensym).

%%  load_core
% load gCoke core, as modules or plain old prolog source code
load_core :-  
	load_pops(helpers), load_module(channels), load_pops(sniffs), 
	load_module(graph), load_module(queries), load_module(path),
	load_module(constraints), load_module(engine), load_module(errors),
	load_module(actions), load_module(dot), load_module(trace), 
	load_module(algorithm), load_module(common), load_module(composition),
	load_module(symbol), load_module(emacs_mode), true.
    

%% load_local_config
% load a local config file (~/.gcoke.pl), if exists.
load_local_config :- 
	getenv('HOME',Home), swritef(F,'%w/.gcoke.pl',[Home]), 
        ( exists_file(F) 
          -> (gw('%% Loading user\'s config file [~/.gcoke.pl] ...'),[F])
          ; true).

%%%%
%% Loading the complete gCoke engine
%%%%

%% load_gcoke
% Load the librairies, THEN the gcoke sources, and FINALLY the local config.
load_gcoke :- 
	set_prolog_flag(verbose_load, false),
	header, load_libraries, load_core, load_local_config.

:- load_gcoke.

