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


%%%%
%% Rejected Automata (business constraints violation)
%%%%

automata_multiple_starts(Automaton,_) :- 
	automata:build(a,A), 
	Actions = [automata:add_start(s1), automata:add_start(s2)],
	engine:do_sequence(A, Actions, Automaton,_).

automata_multiple_finals(Automaton,_) :- 
	automata:build(a,A), 
	Actions = [automata:add_final(s1), automata:add_final(s2)],
	engine:do_sequence(A, Actions, Automaton,_).

%%%%
%% Good examples
%%%%

automata_A(Automaton, Cpt) :- 
	automata:build(a,A), 
	Actions = [
          automata:add_start(s1), 
	  automata:add_state(s2), 
	  automata:add_final(s3),
	  automata:add_trans(s1, s2, [0]),
	  automata:add_trans(s2, s3, [1,0])
        ],
	engine:do_sequence(A, Actions, Automaton, Cpt).




%% automaton_A(Automaton) :- 
%% 	graph:build_graph(automata_A,G), 
%% 	Actions = [
%%           actions:add_property('family', 'moore'),
%% 	  actions:add_node(e1,[['kind','start']]),
%% 	  actions:add_node(e2,[['kind', 'end']]),
%% 	  actions:add_edge(e1,e2,[['letter', 'a']])
%%         ],
%% 	engine:do_sequence(G,Actions,Automaton,_).

%% automaton_B(Automaton) :- 
%% 	graph:build_graph(automata_B,G), 
%% 	Actions = [
%%           actions:add_property('family', 'moore'),
%% 	  actions:add_node(e1,[['kind','start']]),
%% 	  actions:add_node(e2,[['kind', 'end']]),
%% 	  actions:add_edge(e1, e2,[['letter', 'b']])
%%         ],
%% 	engine:do_sequence(G,Actions,Automaton,_).


%% automaton_Unify(Automaton, Cpt) :- 
%% 	graph:build_graph(automata_A,G), 
%% 	Actions = [
%%           actions:add_property('family', 'moore'),
%% 	  actions:add_node(a,[['kind','start']]),
%% 	  actions:add_node(b,[['kind', 'normal']]),
%% 	  actions:add_node(c,[['kind','end']]),
%% 	  actions:add_edge(a,b,[['letter', '0']]),
%% 	  actions:add_edge(b,c,[['letter', '1']]),
%% 	  actions:add_node(bP,[['kind', 'normal']]),
%% 	  actions:add_edge(a,bP,[['letter', '1']]),
%% 	  actions:add_edge(bP,c,[['letter', '0']]),
%% 	  actions:unify_node([b,bP],bPP)
%%         ],
%% 	engine:do_sequence(G, Actions, Automaton, Cpt).

