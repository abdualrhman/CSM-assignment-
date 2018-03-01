# Getting started with F# #

1. [Installing F#](#1-installing-f)
2. [Installing FSLexYacc](#2-installing-fslexyacc)
3. [Using FSLexYacc](#3-using-the-parser-generator)
4. [Further information](#4-further-information)

## 1. Installing F# 

### Installing F# on Windows

Go to http://fsharp.org/use/windows/ and select the installation method that better suits you. If using Visual Studio Community you might have to make sure that the F# individual components are installed.

### Installing F# and mono on Mac OS/X and Linux

The following instructions focus on Mac OS/X but they can be easily adapted to Linux (e.g. by replacing `brew` by some Linux package manager).

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
* Press `Cmd+P` and install the Ionide F# package by typing `ext install Ionide-fsharp`
* Follow the rest of the instructions.

## 2. Installing FsLexYacc

There are several options for installing the lexer and parser generator [FsLexYacc](http://fsprojects.github.io/FsLexYacc/). See https://www.nuget.org/packages/FsLexYacc/ for a list of such options. You can also use the terminal by first installing the package manager `nuget`.

In Mac OS/X you `nuget` can be installed via Homebrew:

```    
brew install nuget
```

In Linux you can use some other package manager to achieve the same thing.

Once `nuget` is installed you can used it to install `FsLexYacc` in your current folder by entering the following command in the terminal:

```
nuget install FsLexYacc
```

Another option is to use the package manager console of Visual Studio Community (under Tools -> NuGet Package Manager -> Package Manager Console) to run:

```
Install-Package FsLexYacc -Version 7.0.6
```

or add the package via the "Manage NuGet Packages..." option in the project solution and installing the `FsLexYacc` package. Either method builds the package under the project `packages` folder.


## 3. Using the parser generator

The following instructions assume that:
- `fslex.exe` and `fsyacc.exe` are available under the folder `FsLexYacc.7.0.6/build/` where you have the lexer and parser files. You can also simplify this by making `fslex` and `fsyacc` available in your path (see e.g. [this guide](https://gist.github.com/AndreasHassing/16567f299b77b0090d94441115a5d031/ae1db7572fd877df733213120800084fbafe9858#4-create-links-to-fslex-and-fsyacc-binaries)). In Windows they can be made available  `fslex.exe` and `fsyacc.exe` by adding the path to the command prompt `PATH` variable (`$env:Path += "C:\...\FsLexYacc.7.0.6\build"` in powershell).
- The `FsLexYacc` library is available under `FsLexYacc.Runtime.7.0.6` in the folder you are working. Also this can be simplified as explained in [this guide](https://gist.github.com/AndreasHassing/16567f299b77b0090d94441115a5d031/ae1db7572fd877df733213120800084fbafe9858#5-link-the-runtime-dll-to-your-fsharp-folder)
- `mono` is needed to execute ".exe" executables (if under Windows, then ignore `mono` in the below instructions)
- the lexer file is [`Hello.fsl`](https://gitlab.gbar.dtu.dk/02141/mandatory-assignment/blob/master/hello/HelloLexer.fsl) and it is in the current folder
- the parser file is [`Hello.fsp`](https://gitlab.gbar.dtu.dk/02141/mandatory-assignment/blob/master/hello/HelloParser.fsp) and it is in the current folder

### Generating the Lexer:
Execute this command in the terminal:

```
mono FsLexYacc.7.0.6/build/fslex.exe HelloLexer.fsl --unicode
```

This will generate the file `HelloLexer.fs`

### Generating the parser
Execute this command in the terminal:

```
mono FsLexYacc.7.0.6/build/fsyacc.exe HelloParser.fsp --module HelloParser
```

This will generate the file `HelloParser.fs`

### Importing and invoking the parser

See file [hello.fsx](hello/Hello.fsx) for an example.

### Running your program

Run the F# script `hello.fsx` with the F# interpreter by entering the following command in the terminal

```
fsharpi Hello.fsx
```

In Windows the F# interactive executable may be called `fsi.exe`, and be located in the `C:\Program Files (x86)\Microsoft SDKs\F#\version\Framework\version\` folder.

The program will try to read and parse the answer to a simple question from the console. It will give you three chances to reply with the expected format (`I'm <name>`):

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

* Lexing and Parsing in F#: https://en.wikibooks.org/wiki/F_Sharp_Programming/Lexing_and_Parsing
* Lexing and Parsing in F#: http://realfiction.net/2014/10/20/Lexing-and-parsing-in-F/
* Installing FsLexYacc on a Mac: https://gist.github.com/AndreasHassing/16567f299b77b0090d94441115a5d031/ae1db7572fd877df733213120800084fbafe9858
* Creating a Makefile: https://gist.github.com/klaeufer/2285720