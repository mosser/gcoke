%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           gCoKe Copyright (C) 2010 - ...          %%
%%          Graph-based COmposition KErnel           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Author:  Sebastien Mosser < sm@gcoke.org >       %%
%%  Website: http://www.gcoke.org                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  This program comes with ABSOLUTELY NO WARRANTY.  %%
%%  This is free software, and you are welcome to    %%
%%  redistribute it under certain conditions.        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 O. Repository fetch & Third part software

See http://code.google.com/p/gcoke/source/checkout for checkout instructions.

gCoKe assumes that the following applications are available on your system:

  - swipl
  - graphviz (especially the 'dot' tool)
  - Java (to compile the gCoKe compiler)

 I. Shell setup

Enhance your environment variables, by adding in your ~/.bash_profile file the 
following code snippet:

GCOKE_HOME=/Users/mosser/work/gcoke
PATH=$PATH:$GCOKE_HOME/bin
GCOKE_DOT='/usr/local/bin/dot -Gfontpath=/System/Library/fonts'
GCOKE_OPEN='open'
export GCOKE_HOME GCOKE_OPEN GCOKE_DOT PATH

Replace:
  - '/Users/mosser/work/gcoke' by the checkout repository
  - '/usr/local/bin/dot ...' by a graphviz DOT command line
  - 'open' by a picture visualizer, if you're not using OS X

 II. Compiler setup

The gCoKe system comes with a compiler associated to its domain-specific 
language. Run the following commands to compile the compiler.

azrael:~ mosser$ cd /Users/mosser/work/gcoke/java/compiler/
azrael:compiler mosser$ ./compile.sh 
## Generating Java file from ANTLR description
## Compiling Java source file
Note: Some input files use unchecked or unsafe operations.
Note: Recompile with -Xlint:unchecked for details.
## Building executable JAR file
azrael:compiler mosser$ cd

  III. Local configuration file

The local gCoKe configuration is stored in a ~/.gocke.pl file. 

Use the following configuration in this file to load the existing examples:

:- gcoke_declare_repository('/Users/mosser/work/gcoke/lines').

  IV. Starting the engine (from command line)

Run the following command to start the gcoke interpreter from the command line:

azrael:~ mosser$ gcoke_start.sh 
% library(swi_hooks) compiled into pce_swi_hooks 0.00 sec, 3,688 bytes
% ...
% /Users/mosser/work/gcoke/prolog/_init.pl compiled 0.01 sec, 162,136 bytes
?- halt.
azrael:~ mosser$

  V. Optional: Emacs setup 

Add the following code snippet in your ~/.emacs file to load the gCoKe mode:

(setenv "GCOKE_HOME" "/Users/mosser/work/gcoke")
(load-file (concat (getenv "GCOKE_HOME") "/gcoke.el"))
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "GCOKE_HOME") "/bin"))

