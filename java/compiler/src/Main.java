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

public class Main {

    /** Main method, entry point of the compiler **/
    public static void main(String[] args) throws Exception {
	String tpl = extractTemplateName(args);
	InputStream in = extractInputStream(args);
	PrintStream out = extractOutputStream(args);
	Compiler.run(tpl, in, out);
    }

    private static String extractTemplateName(String[] args) {
	try {
	    String data = extract("tpl",args);
	    return (null == data ? Compiler.DEFAULT_TPL: data);
	} catch (Exception e) {
	    System.err.println("Template: command-line arguments error.");
	    System.err.println("Exception is: " + e);
	    return Compiler.DEFAULT_TPL;
	}
    }

    private static InputStream extractInputStream(String[] args) {
	try {
	    String data = extract("in",args);
	    return (null == data ? Compiler.DEFAULT_IN: 
		    new FileInputStream(data));
	} catch (Exception e) {
	    System.err.println("Input: command-line arguments error.");
	    System.err.println("Exception is: " + e);
	    return Compiler.DEFAULT_IN;
	}
    }

    private static PrintStream extractOutputStream(String[] args) {
	try {
	    String data = extract("out",args);
	    return (null == data? Compiler.DEFAULT_OUT: 
		    new PrintStream(new FileOutputStream(data)));
	} catch (Exception e) {
	    System.err.println("Output: command-line arguments error.");
	    System.err.println("Exception is: " + e);
	    return Compiler.DEFAULT_OUT;
	}
    }

    private static String extract(String key, String[] args) {
	for(int i = 0; i < args.length; i++) {
	    if (args[i].equals("-" + key)){ return args[i+1]; }
	}
	return null;
    }
}