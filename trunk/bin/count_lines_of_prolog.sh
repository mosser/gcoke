#! /bin/bash


function count_lines() 
{
    echo `cat $1 | grep -v '^[[:space:]]*%' | sed '/^[[:space:]]*$/d' | wc -l`
}


function count_all_files() 
{
    SUM=0;
    for f in $1/*.pl
    do
	CURR=$(count_lines $f)
	SUM=$(( $SUM + $CURR))
	NAME=`basename $f`
	echo "\t$NAME\t$CURR"
    done
    echo "$SUM"
}


function main()
{
    echo "Directory: $1"
    echo -e "$(count_all_files $1)"
}

main $@