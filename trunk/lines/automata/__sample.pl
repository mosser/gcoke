

automaton_A(Automaton) :- 
	graph:build_graph(automata_A,G), 
	Actions = [
          actions:add_property('family', 'moore'),
	  actions:add_node(e1,[['kind','start']]),
	  actions:add_node(e2,[['kind', 'end']]),
	  actions:add_edge(e1,e2,[['letter', 'a']])
        ],
	engine:do_sequence(G,Actions,Automaton,_).

automaton_B(Automaton) :- 
	graph:build_graph(automata_B,G), 
	Actions = [
          actions:add_property('family', 'moore'),
	  actions:add_node(e1,[['kind','start']]),
	  actions:add_node(e2,[['kind', 'end']]),
	  actions:add_edge(e1, e2,[['letter', 'b']])
        ],
	engine:do_sequence(G,Actions,Automaton,_).


automaton_Unify(Automaton, Cpt) :- 
	graph:build_graph(automata_A,G), 
	Actions = [
          actions:add_property('family', 'moore'),
	  actions:add_node(a,[['kind','start']]),
	  actions:add_node(b,[['kind', 'normal']]),
	  actions:add_node(c,[['kind','end']]),
	  actions:add_edge(a,b,[['letter', '0']]),
	  actions:add_edge(b,c,[['letter', '1']]),
	  actions:add_node(bP,[['kind', 'normal']]),
	  actions:add_edge(a,bP,[['letter', '1']]),
	  actions:add_edge(bP,c,[['letter', '0']]),
	  actions:unify_node([b,bP],bPP)
        ],
	engine:do_sequence(G, Actions, Automaton, Cpt).

