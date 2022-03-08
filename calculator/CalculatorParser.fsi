// Signature file for parser generated by fsyacc
module CalculatorParser
type token = 
  | ASSIGN
  | SKIP
  | SEMICOL
  | IF
  | FI
  | DO
  | OD
  | TRUE
  | FALSE
  | AND
  | ANDAND
  | OR
  | OROR
  | NOT
  | EQUAL
  | NOTEQUAL
  | GREATER
  | GREATEREQUAL
  | LESS
  | LESSEQUAL
  | TIMES
  | DIV
  | PLUS
  | MINUS
  | POW
  | LPAR
  | RPAR
  | EOF
  | LBRAC
  | RBRAC
  | VAR of (string)
  | NUM of (float)
type tokenId = 
    | TOKEN_ASSIGN
    | TOKEN_SKIP
    | TOKEN_SEMICOL
    | TOKEN_IF
    | TOKEN_FI
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_AND
    | TOKEN_ANDAND
    | TOKEN_OR
    | TOKEN_OROR
    | TOKEN_NOT
    | TOKEN_EQUAL
    | TOKEN_NOTEQUAL
    | TOKEN_GREATER
    | TOKEN_GREATEREQUAL
    | TOKEN_LESS
    | TOKEN_LESSEQUAL
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_POW
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_EOF
    | TOKEN_LBRAC
    | TOKEN_RBRAC
    | TOKEN_VAR
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_expression
    | NONTERM_Var
    | NONTERM_Boolean
    | NONTERM_Command
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (Command) 
