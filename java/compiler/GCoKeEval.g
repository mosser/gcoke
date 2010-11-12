/**
 * This file is part of gCoKe [ http://www.gcoke.org ]
 *
 * Copyright (C) 2010-  Sebastien Mosser
 *
 * gCoKe is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation; either version 2 of 
 * the License, or (at your option) any later version.
 *
 * gCoKe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with gCoKe; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * @author   Main    Sebastien Mosser  [ sm@gcoke.org ]
 **/
tree grammar GCoKeEval;

options {
  tokenVocab=GCoke;
  ASTLabelType=CommonTree;
  output=template;
}


// Source handling
source	:	^(SOURCE artefacts+=artefact+) 
	-> 	source(artefacts={$artefacts});

// Artefact handling
artefact:	^(RAW file=STRING)
		-> 	raw(file_name={$file.text})
	|	^(SNIFF line=ID)
		-> 	sniff(line_name={$line.text})
	|	^(GRAPH id=ID nodes=node_lst 
		        edges=edge_lst? props=graph_prop_lst?)
		-> 	graph(id={$id.text}, nodes={nodes}, 
			 	  edges={edges}, props={props});
	

/** Graph-specific rules **/

// graph properties handling
graph_prop_lst
	:	^(PROP_LST props+=graph_prop+)
		-> 	graph_prop_lst(props={$props});
graph_prop	
	:	^(PROP key=ID value=STRING)
		-> 	graph_prop(key={$key.text}, value={$value.text}); 
// nodes handling (graph internal)
node_lst:	^(NODE_LST nodes+=node+) -> node_lst(nodes={$nodes});
node	:	^(NODE id=ID props=prop_lst?)
		-> 	node(id={$id.text}, props={props})
	;	

// edges handling (graph internal)
edge_lst:	^(EDGE_LST edges+=edge+)
		-> 	edge_lst(edges={edges});
edge	:	^(EDGE left=ID right=ID props=prop_lst?)
		-> 	edge(left={$left.text},right={$right.text},
			   	 props={props});

// properties handling (nodes & edges)
prop_lst:	^(PROP_LST props+=prop+)
		-> 	prop_lst(props={$props});
prop	:	^(PROP key=ID value=STRING)
		-> 	prop(key={$key.text}, value={$value.text});
		
