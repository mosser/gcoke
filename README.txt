%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           gCoke Copyright (C) 2010 - ...          %%
%%          Graph-based COmposition KErnel           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Author:  Sebastien Mosser < sm@gcoke.org >       %%
%%  Website: http://www.gcoke.org                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  This program comes with ABSOLUTELY NO WARRANTY.  %%
%%  This is free software, and you are welcome to    %%
%%  redistribute it under certain conditions.        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

I. Setup instructions

Enhance your environment variables, in your ~/.bash_profile file

GCOKE_HOME=/Users/mosser/work/gcoke
PATH=$PATH:$GCOKE_HOME/bin
export GCOKE_HOME PATH

Optional: Create a ~/.gcoke.pl file, to store your personal settings.

II. Graphviz setup

gCoke assumes a running installation of Graphviz to produce graphical files. 
The framework will look for a 'dot' executable, in the PATH. One can use a
GCOKE_DOT environment variable to set an absolute path to the executable (e.g.,
'/opt/local/bin/dot')

III. Starting the engine:

Simply run the 'gcoke_start.sh' shell script:

mosser@necronomicon:~/work/gcoke$ gcoke_start.sh 
% library(swi_hooks) compiled into pce_swi_hooks 0.00 sec, 3,688 bytes
% ...
% /Users/mosser/work/gcoke/prolog/_init.pl compiled 0.01 sec, 162,136 bytes
?- 
