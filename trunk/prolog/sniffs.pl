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
%% License along with gCokKe; if not, write to the Free Software Foundation,
%% Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% @author   Main    Sebastien Mosser  [ sm@gcoke.org ]
%%%%
:- module(sniffs,[gcoke_declare_repository/1, gcoke_sniff/1]).
:- channels:declare(sniff(load)).
/** <module> "Sniffing lines of gCoKe"
 Rationale: this file allows one to sniff 'lines' in gCoKe.

 Line?
  - A line is a gCoKe 'specialization'. It consists of composition 
    mechanisms dedicated to a given domain.
  - A line is built upon gCoKe, and use the framework to ease the definition
    of the intended composition mechanisms
 
 How do I define my own lines?
  - A 'line' is a simple directory, which defines a file named '_init.pl'
  - Starting from this '_init.pl' file, you can do virtually what you want,
    you are in a Plain Old Prolog Source File. Load modules, others files, ...
  - Used paths are computed locally, that is, from your '_init.pl' file.

 How do I 'sniff' something?
  - You will have to declare your personal lines repository (i.e., the directory
    which contains your 'lines'). You can declare multiple repositories.
  - the gcoke_sniff(line) rules will load the a_repository/line/_init.pl file.

*/
:- dynamic gcoke_sniff_repository/1.

%% gcoke_declare_repository(+Directory)
% declare Directory as an available sniff repository
gcoke_declare_repository(Directory) :- 
	assert(gcoke_sniff_repository(Directory)).

%% gcoke_get_available_sniffs(-List)
% Unify list with a list of ALL available lines (from all repositories).
gcoke_get_available_lines(List) :- 
	findall(S, gcoke_is_available_line(S),List).

%% gcoke_is_available_line(?Line_name) is nondet
% Line_name is unified with an available line. An instantiated Line_name
% check its existence. A free call find an available one.
gcoke_is_available_line(Line_name) :- 
	gcoke_sniff_repository(Dir), swritef(Pattern,'%w/*/_init.pl',[Dir]),
        expand_file_name(Pattern, Match_list),
	member(Path, Match_list), file_directory_name(Path, Sniff_dir), 
	file_base_name(Sniff_dir, Line_name).

%% gcoke_can_sniff(+Line_name, -Path)
% Unify Path with the concrete path of the '_init.pl' file associated to 
% Line_name, and checks its existence. Fail elsewhere.
gcoke_can_sniff(Line_name, Path) :- 
	gcoke_sniff_repository(Dir), 
	swritef(Path,'%w/%w/_init.pl',[Dir,Line_name]), exists_file(Path).

%% gcoke_sniff(Line_name)
% Load a sniff in the engine. Throw an error if unable to do so.
gcoke_sniff(Line_name) :- 
	\+ gcoke_can_sniff(Line_name,_), !,
	channels:push(sniff(load),'Unable to load [~w]',[Line_name]),
	throw(error(gcoke_sniff_error(Line_name),
	            context(gcoke_sniff/1,'Unknown Line'))).
gcoke_sniff(Line_name) :- 
	gcoke_can_sniff(Line_name, Path), 
	channels:push(sniff(load),'Loading [~w] from [~w]',[Line_name,Path]),
	module(user), [Path].
