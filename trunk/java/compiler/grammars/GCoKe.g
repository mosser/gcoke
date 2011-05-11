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
grammar GCoKe;

options {
  output 	= AST;
  ASTLabelType 	= CommonTree;
}

/* Imaginary tokens used to structure the AST */ 
tokens { 
  SOURCE; GRAPH; REQUIRE; SNIFF; RAW; COMPOSITION; TRIGGER;
  EDGE; EDGE_LST; NODE; NODE_LST; PROP; PROP_LST;
  COMPO_INPUTS; COMPO_OUTPUTS; COMPO_DIR_LST; COMPO_DIR; 
  ALGO_INPUTS; ALGO_OUTPUTS; ALGO_BIND; ALGO_GRAPH; ALGO_TERM; ALGO_GRAPH_SET;
  VAL_LST;
}

/* Lexer-specific code */
@lexer::header { package org.gcoke.dsl.compiler; }

/* parser specific code */
@parser::header {
package org.gcoke.dsl.compiler;
import java.util.Map;
import java.util.HashMap;
import java.io.*;
}

@parser::members {  
  // Mandatory to handle requirement cycle properly (and avoid infinite recursion)
  private Map<String, Boolean> visited = new HashMap<String, Boolean>();
  public void setVisited(Map<String,Boolean> visited) { this.visited = visited; }
  public Map<String, Boolean> getVisited() { return this.visited; }
  private void remember(String fileName) { this.visited.put(fileName, new Boolean(true)); }
  private boolean isVisited(String fileName) { return this.visited.containsKey(fileName); }
}

/* Lexer token recognizer */
ID  	:		'a'..'z' ('a'..'z'|'A'..'Z'|'0'..'9'|'_')* ;
STRING	:  		'\'' ( ESC_SEQ | ~('\\'|'\'') )* '\'' ;
fragment ESC_SEQ : 	'\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\') ;
COMMENT
    :   '//' ~('\n'|'\r')* '\r'? '\n' 			{$channel=HIDDEN;}
    |   '/*' ( options {greedy=false;} : . )* '*/' 	{$channel=HIDDEN;} ;
WS  :   ( ' ' | '\t' | '\r' | '\n') 			{$channel=HIDDEN;} ;

/* Parser rules */
source	:	artefact*			-> ^(SOURCE artefact*);
artefact:	sniff | raw | graph | compo  | trigger | require ;

sniff	:	'sniff' line=ID ';'		-> ^(SNIFF $line);
raw	:	'require_raw' file=STRING ';'	-> ^(RAW $file);
trigger :	'trigger' composition=ID ';'	-> ^(TRIGGER $composition);

/* Graph-specific (internal) rules */
graph	:	'graph' id=ID props=prop_lst? '{' nodes=node_lst edges=edge_lst? '}' 
						-> ^(GRAPH $id $nodes $edges? $props?);
prop_lst:	'[' prop (',' prop)* ']'	-> ^(PROP_LST prop+);
prop	:	key=ID '=' value=STRING 	-> ^(PROP $key ^(VAL_LST $value))
	|	key=ID'=' '{' STRING (',' STRING)* '}' 
						-> ^(PROP $key ^(VAL_LST STRING+));

node_lst:	node+ 				-> ^(NODE_LST node+);
node	:	id=ID props=prop_lst? ';' 	-> ^(NODE $id $props?);

edge_lst:	edge+				-> ^(EDGE_LST edge+);
edge	:	left=ID '->' right=ID props=prop_lst? ';' 
						-> ^(EDGE $left $right $props?);

/* Composition specific (internal) rules */

compo	:	 'composition' id=ID ins=graph_lst content=compo_content '=>' outs=graph_lst ';'
						-> ^(COMPOSITION $id 
								 ^(COMPO_INPUTS $ins*) 
								 ^(COMPO_OUTPUTS $outs*)
								 $content);
graph_lst
	:	'(' ID? ')'			-> ID?
	|	'(' ID (',' ID)+ ')'		-> ID+;
compo_content
	:	('[' prefix=ID ']')? '{' directive+=compo_dir[$prefix.text]+ '}'	
						-> ^(COMPO_DIR_LST $directive+);
compo_dir [String prefix] // used to distributed a given prefix to all the given directives
	:	id=ID ins=param_list '=>' outs=param_list ';'
		{ if (null != prefix) { id.setText(prefix + ":" + $id.text); } }
						-> ^(COMPO_DIR $id ^(ALGO_INPUTS $ins*)
							       ^(ALGO_OUTPUTS $outs*))
	|	mod=ID ':' id=ID ins=param_list '=>' outs=param_list ';'
		{ id.setText($mod.text + ":" + $id.text); }
						-> ^(COMPO_DIR $id ^(ALGO_INPUTS $ins*)
							       ^(ALGO_OUTPUTS $outs*))
	;
param_list
	:	'(' compo_bind? ')'		-> compo_bind?
	|	'(' compo_bind (',' compo_bind)+ ')' 
						-> compo_bind+ ;
compo_bind
	:	id=ID ':' value=ID 		-> ^(ALGO_BIND $id ^(ALGO_GRAPH  $value))
	|	id=ID ':' value=STRING		-> ^(ALGO_BIND $id ^(ALGO_TERM $value))
	|	id=ID ':' bind_graph_set  	-> ^(ALGO_BIND $id bind_graph_set)
	;
bind_graph_set
	:	'{' ID '}'			-> ^(ALGO_GRAPH_SET ID)
	|	'{' ID (',' ID)+ '}'
						-> ^(ALGO_GRAPH_SET ID+);
														
/* Require mechanisms (include the AST of another source file into this one) */
// code insipired by [http://www.antlr.org/wiki/pages/viewpage.action?pageId=557057]
require	
@init { CommonTree includeTree = null; boolean produce = false; }
	:	'require' file=STRING ';' 
	 	{ try {
	 	    if (! isVisited($file.text) ) {
	 	      this.remember($file.text); produce = true;
		      String fileName = ($file.text).substring(1,($file.text).length()-1);
      		      GCoKeParser parser = ASTBuilder.buildParser(new FileInputStream(fileName));
      		      parser.setVisited(this.getVisited());
      		      includeTree = ASTBuilder.buildAST(parser);
      		      this.setVisited(parser.getVisited());
      		    }
    		  } catch (Exception fnf) { throw new RuntimeException(fnf); }
	  	}
	  	->  {produce}? 	^(REQUIRE $file ^({includeTree})) // must require the file
	  	-> ; // simply ignored if still produced
