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
:- module(errors,[]).


%%%%
%% Error handling:
%%
%%   - Exceptions are assumed as : error(Details, context(Artifact, Message).
%%    - Details is an error-dependent term (see 'handle_details').
%%    - Artifact is the faulty artifact (graph(Name)|composition(Name)).
%%    - Message is a human-readable message (i.e., the "reason"). 
%%  
%%  - handle_details: by default, Detail term is simply printed, with few 
%%      enhancement. One can define his/her own handling through the definition
%%      of a new 'handle_details' predicate. This predicate must end with a cut.
%%%%

%% catch_and_ignore/1: catch_and_ignore(+Goal). 
% Call Goal. If case of an exception catch, display the exception, but silently
% succeed (so it does not impact other calls).
% Remark: Mainly used by the gCoKe compiler (see compiled code).
catch_and_ignore(Goal) :- 
	catch(Goal, Exception, display_error(Exception)).

%% display_error/1: display_error(+Exception).
% Display Exception on stderr.
display_error(error(Details_term, context(Artifact_term, Msg))) :- 
	handle_details(Details_term, Details),
	handle_artifact(Artifact_term, Artifact),
	swritef(S, '\ngCoKe exception: %w\n  Artefact: %w\n  Details:\n%w\n', 
                [Msg, Artifact, Details]), write(user_error,S).

%% handle_artifact/2: handle_artifact(+Term, -String)
% Transform an artifact Term into a String.
handle_artifact(Term, String) :- 
	Term =.. L, swrite_list(L,' ','',String).

%% handle_details/2, handle_details(+Details, -String).
% Transform the Details exception term into a human readable String.
handle_details(gcoke_constraint_error(Constraint, Action_term), Result) :- 
	Action_term =.. [Kind_code, Action],
	(Kind_code = pre -> Kind = precondition; 
	    Kind_code = post -> Kind = postcondition; Kind_code = Kind),
	swritef(Result, '    - Kind: %w\n    - Constraint: %w\n    - Action: %w\n', 
                 [Kind, Constraint, Action]),!.

handle_details(Generic_term, Result) :-  %% Generic implementation (default)
	Generic_term =.. L, swrite_list(L, '\n','    - ',Result).

