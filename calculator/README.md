## Getting started 
### Installation 
Make sure to have `FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll` installed and change the path `FM4FUN.fsx` to where it's installed.

Then go inside `FM4FUN.fsx` and run the following commands:
``` 
mono PATH_TO_LEXER/FsLexYacc.10.0.0/build/fslex/net46/fslex.exe FM4FUNLexer.fsl --unicode
```

``` 
mono PATH_TO_PARSER/FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe FM4FUNParser.fsp --module FM4FUNParser
```

```
fsharpi FM4FUN.fsx
```

You should be able to write an arithmetic expression, such as:
```
i:=0; x:=0; y:=0; do i<10 -> if A[i]>=0 ->x:=x+A[i];i:=i+1 [] A[i]<0 -> i:=i+1 fi od
```