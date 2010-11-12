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
  output=AST;
  ASTLabelType=CommonTree;
}

tokens {
  SOURCE; GRAPH; REQUIRE; SNIFF; RAW;
  EDGE; EDGE_LST; NODE; NODE_LST; PROP; PROP_LST;
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

source	:	artefact+			-> ^(SOURCE artefact+);
artefact:	require | sniff | graph | raw;

require	:	'require' file=STRING ';'	-> ^(REQUIRE $file); // FIXME
sniff	:	'sniff' line=ID ';'		-> ^(SNIFF $line);
graph	:	'graph' id=ID props=prop_lst? '{' nodes=node_lst edges=edge_lst? '}' 
						-> ^(GRAPH $id $nodes $edges? $props?);
raw	:	'require_raw' file=STRING ';'	-> ^(RAW $file);

/* Graph-specific (internal) rules */
prop_lst:	'[' f=prop (',' o=prop)* ']'	-> ^(PROP_LST $f $o*);
prop	:	key=ID '=' value=STRING 	-> ^(PROP $key $value);

node_lst:	node+ 				-> ^(NODE_LST node+);
node	:	id=ID props=prop_lst? ';' 	-> ^(NODE $id $props?);

edge_lst:	edge+				-> ^(EDGE_LST edge+);
edge	:	left=ID '->' right=ID props=prop_lst? ';' 
						-> ^(EDGE $left $right $props?);
