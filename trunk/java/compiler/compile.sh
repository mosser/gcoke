#!/bin/sh

function generate() 
{
    echo "## Generating Java file from ANTLR description"
    java org.antlr.Tool *.g
}

function compile()
{
    echo "## Compiling Java source file"
    javac -d . *.java
}

function build_jar()
{
    echo "## Building executable JAR file"
    jar cmf MANIFEST.MF gcoke_compiler.TMP *.class templates/*.stg
    ./clean.sh
    mv gcoke_compiler.TMP gcoke_compiler.jar
}

function main()
{
    generate
    compile
    build_jar
}

main