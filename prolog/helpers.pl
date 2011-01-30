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
:- module(helpers, [ gcoke_functor/3, gcoke_term_to_list/2, id_assert/1, 
	             write_list/3, swrite_list/4, getenv_or_default/3]).
/** <module> Helpers predicate
*/

%% gcoke_functor(+Term, ?Head, ?Arity) 
% Redefinition of the 'functor' predicate, to properly handle modules.
gcoke_functor(Term, Head, Arity) :- 
 	strip_module(Term,user,Term), !, % not a module term, raw pred.
 	functor(Term,Head,Arity).
gcoke_functor(Term, Head, Arity) :-
	strip_module(Term, Module, Real_term),
	functor(Real_term,Raw_head, Arity),
	swritef(Final_head,'%w:%w',[Module,Raw_head]),
        string_to_atom(Final_head,Head).

%% gcoke_term_to_list(+Term, -List)
% Redefinition of the '=..' predicate to properly handle modules (not bidir).
gcoke_term_to_list(Term, List) :- 
	strip_module(Term,user,Term), !, Term =.. List.
gcoke_term_to_list(Term, [Head|Other_terms]) :- 
	strip_module(Term, Module, Real_term),
	Real_term =.. [Raw_head|Other_terms],
	swritef(Final_head,'%w:%w',[Module,Raw_head]),
        string_to_atom(Final_head,Head).	

%% id_assert(+Fact)
% Realize an idempotent assertion (existing fact will not be re-asserted)
% also works on dynamic predicate (which do not pre-exists)
% generates weird behavior for 'module:term' unification. FIX?
id_assert(Fact) :- catch(Fact,error(existence_error(_,_),_),fail),!.
id_assert(Fact) :- assert(Fact).

%% write_list(+List, +Separator,+Pre)
% Write each element of List, separated by Separator and preceded by Pre.
write_list(List, Pre, Sep) :- swrite_list(List, Sep, Pre, R), write(R).

%% swrite_list(+List, +Separator, +Pre, -Result)
% Unify Result with a string containing all elements of List, preceded by Pre 
% and separated by Separator.
swrite_list([],_,_,'').
swrite_list([H],_,Pre,R) :- !, swritef(R,'%w%w',[Pre,H]).
swrite_list([H|T],Sep,Pre,Res) :- 
	swrite_list(T,Sep,Pre,Others), 
	swritef(Res,'%w%w%w%w',[Pre,H,Sep,Others]).

%% getenv_or_default(+Env_var_name, +Default, -Value)
% unify Value with the value of Env_var_name, if defined. If not, Value is
% unified with Default.
getenv_or_default(Env_var_name, Default, Value) :- 
 	getenv(Env_var_name, Value) -> true; Value = Default.