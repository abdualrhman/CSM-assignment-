# Getting started

1. [Installing F#](#1-installing-f)
2. [Installing FSLexYacc](#2-installing-fslexyacc)
3. [Using FSLexYacc](#3-using-the-parser-generator)
4. [Further information](#4-further-information)

# 1. Installing F# 

## Installing F# and mono on Mac OS/X

Go to http://fsharp.org/use/mac/ and select the installation method that better suits you.

A simple method is to install mono via [Homebrew](https://brew.sh/) by entering the following command in the terminal:

```
brew install mono
```

Now the commands `mono` (to run .NET exe files), `fsharpi` (F# interpreter) and `fsharpc` (F# compiler) will be available in your path so that you can run them using a terminal. 

There are several editors for F# but you may want to go for [Visual Studio Code](https://code.visualstudio.com/), which you can also install via Homebrew with:

```
brew cask install visual-studio-code 
```

To add F# extension to Visual Studio Code:
* Press Cmd+P and install the Ionide F# package: ext install Ionide-fsharp
* Follow the rest of the instructions.

# 2. Installing FsLexYacc

Again, there are several options for installing the parser generator FsLexYacc (see https://www.nuget.org/packages/FsLexYacc/). A simple option is to use Homebrew by typing the following commands in the terminal:

```    
brew install nuget
```

This will install the package manager `nuget`

And then you can install the `fxlex` and `fxyacc` and libraries to current folder by entering the following command in the terminal:

```
nuget install FsLexYacc
```

## 3. Using the parser generator

The following instructions assume that:
- fslex.exe and fsyacc.exe are available under the folder "FsLexYacc.7.0.6/build/" where you have the lexer and parser files
- mono is needed to execute ".exe" executables (if under Windows, then remove "mono")
- the lexer file is Hello.fsl
- the parser file is Hello.fsp

### Generating the Lexer:
Execute this command in the shell:

```
mono FsLexYacc.7.0.6/build/fslex.exe HelloLexer.fsl --unicode
```

This will generate the file `HelloLexer.fs`

### Generating the parser
Execute this command in the shell:

```
mono FsLexYacc.7.0.6/build/fsyacc.exe HelloParser.fsp --module HelloParser
```

This will generate the file `GCParser.fs`

(2) Importing and invoking the parser

fsharpi hello.fsx


# 4. Further information

On lexing and parsing in F#
* https://en.wikibooks.org/wiki/F_Sharp_Programming/Lexing_and_Parsing
* https://gist.github.com/AndreasHassing/16567f299b77b0090d94441115a5d031/ae1db7572fd877df733213120800084fbafe9858
* https://gist.github.com/klaeufer/2285720
* http://realfiction.net/2014/10/20/Lexing-and-parsing-in-F/