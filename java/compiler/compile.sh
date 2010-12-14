#!/bin/sh

CP=.:$GCOKE_HOME/java/compiler/libs/antlr-3.3-complete.jar

function generate() 
{
    echo "## Generating Java file from ANTLR description"
    java -classpath $CP org.antlr.Tool -o gen grammars/*.g
}

function compile()
{
    echo "## Compiling Java source file"
    javac -classpath $CP -d . src/*.java gen/grammars/*.java
}

function build_jar()
{
    echo "## Building executable JAR file"
    cp -r templates/*.stg  org/gcoke/dsl/compiler/.
    jar cmf MANIFEST.MF gcoke_compiler.jar org
    rm -rf gen org 
}

function main()
{
    generate
    compile
    build_jar
}

main