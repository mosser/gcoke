#!/bin/sh

function generate() 
{
    echo "## Generating Java file from ANTLR description"
    java org.antlr.Tool -o gen grammars/*.g
}

function compile()
{
    echo "## Compiling Java source file"
    javac -d . src/*.java gen/grammars/*.java
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