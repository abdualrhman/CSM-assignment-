# Getting started with Java and ANTLR4

1. [Installing Java](#1-installing-java)
2. [Installing ATLR4](#2-installing-antlr4)
3. [Using ANTLR4](#3-using-antlr4)
4. [Further information](#4-further-information)

## 1. Installing Java

Your computer may already have Java installed. If not, see https://www.java.com/en/download/help/download_options.xml for instructions on how to install Java in your computer.

There are plenty of editors and IDEs for Java. In the following guidelines we use the terminal to compile Java programs and to run them.

## 2. Installing ANTLR4

[ANTLR](http://www.antlr.org/) has a pretty nice guide to [getting started](https://github.com/antlr/antlr4/blob/master/doc/getting-started.md). Follow it and try the simple examples.

## 3. Using ANTLR4

The guidelines to [getting started with ANTLR](https://github.com/antlr/antlr4/blob/master/doc/getting-started.md) contain a pretty simple example. We provide you here another simple one.

The following instructions assume that:
- Your grammar file is [Hello.g](hello/Hello.g) and it is in the current folder where you are running the terminal.
- Command `antlr4` can be found by the terminal


### Generating the parser

Execute this command in the terminal to generate the parser for your grammar:

```
antlr4 -visitor -no-listener Hello.g
```

This will generate a couple of Java files (`HelloParser.java`, `HelloLexer.java`, etc.)

### Importing and invoking the parser

See file [`Hello.java`](hello/Hello.java) for an example.

### Compiling your program

Execute the following command in the terminal to compile your Java program:

```
javac *.java
```

This will generate some class files.

### Running your program

You can now run your Java program with 

```
java Hello
```

The program will try to read and parse the answer to a simple question from the console. It will keep giving you chances to reply with the expected format (`I'm <name>`):

```
Who are you?
Alice
Who are you?
I am Alice
Who are you?
I'm Alice
Hello Alice!
```

## 4. Further information

On parsing in Java and ANTLR4:
* http://www.antlr.org/
