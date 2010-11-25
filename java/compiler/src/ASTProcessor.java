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
 * License along with gCoke; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * @author   Main    Sebastien Mosser  [ sm@gcoke.org ]
 **/
package org.gcoke.dsl.compiler;
import java.io.*;
import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;
import org.antlr.stringtemplate.*;

public class ASTProcessor {
    
    public static StringTemplate run(CommonTree tree, String tpl)
	throws Exception {
	StringTemplateGroup templates = initializeTemplate(tpl);
	CommonTreeNodeStream nodes = new CommonTreeNodeStream(tree);
	GCoKeEval walker = new GCoKeEval(nodes);
	walker.setTemplateLib(templates);
	GCoKeEval.source_return wr = walker.source();
	return (StringTemplate) wr.getTemplate();
    }

    private static StringTemplateGroup initializeTemplate(String tplFile) 
	throws Exception {
	String res = tplFile+".stg"; 
	InputStream stgStream = ASTProcessor.class.getResourceAsStream(res);
	Reader stgReader = new InputStreamReader(stgStream);
	StringTemplateGroup templates = new StringTemplateGroup(stgReader);
	stgReader.close(); 
	return templates;
    }
}