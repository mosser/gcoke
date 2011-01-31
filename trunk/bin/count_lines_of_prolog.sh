#! /bin/bash

USE_M2H="true"
#USE_M2H="false"

# m2h is a tool written by Richard O'Keefe
#   ==>> http://www.cs.otago.ac.nz/staffpriv/ok/software.htm
# Without m2h, the number of lines of code is only approximated (as the
# regular expression does not handle properly multi-line comments.

# Approximate the number of lines of code (including multiline comments)
# $1 is the name of the file to be analysed
function count_approx() 
{
    echo `cat $1 | grep -v '^[[:space:]]*%' | sed '/^[[:space:]]*$/d' | wc -l`
}

# Count exactly the number of line of codes
# $1 is the name of the file to be analysed
function count_m2h() 
{
    echo `cat $1 | m2h -ex -l pr | sed '/^[[:space:]]*$/d' | wc -l`
}

# Count the number of LoC for all *.pl file in a given directory
# $# is the name of the diectory to ba analysed
function count_all_files() 
{
    SUM=0;
    for f in $1/*.pl
    do
	if [ "true" = $USE_M2H ]
	then
	    CURR=$(count_m2h $f)
	else
	    CURR=$(count_approx $f)
	fi
	SUM=$(( $SUM + $CURR))
	NAME=`basename $f`
	echo "\t$NAME\t$CURR"
    done
    echo "$SUM"
}

# Run the engine, and pretty print the results
function main()
{
    echo "Directory: $1"
    echo -e "$(count_all_files $1)"
}

# Start the counting engine
main $@