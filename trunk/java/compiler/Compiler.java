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

import java.io.*;
import org.antlr.runtime.tree.*;
import org.antlr.stringtemplate.*;

public class Compiler {

    public static final String DEFAULT_TPL = "gck2pl"; // gck -> prolog
    public static final InputStream DEFAULT_IN = System.in;
    public static final PrintStream DEFAULT_OUT = System.out;

    public static void run(String tpl, InputStream in, PrintStream out) 
	throws Exception {
	CommonTree ast = ASTBuilder.run(in);
	StringTemplate result = ASTProcessor.run(ast, tpl);
	out.println(result.toString()); 
    }
}