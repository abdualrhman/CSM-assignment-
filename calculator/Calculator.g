// This is how we specify the name of the grammar
grammar Calculator;

// Our start non-terminal symbol is "start"
// NOTE: it is conceptually the start symbol for our purpose
//       but we can start parsing with any symbol
// It has only one production, in "formal" notation:
// start -> expr
start : expr EOF ;

// These are the productions for non-terminal "expr"
expr  :               lhs = expr '*' rhs = expr  #TimesExpr
      |               lhs = expr '/' rhs = expr  #DivExpr
      |               lhs = expr '+' rhs = expr  #PlusExpr
      |               lhs = expr '-' rhs = expr  #MinusExpr
      | <assoc=right> lhs = expr '^' rhs = expr  #PowExpr
      | '(' expr ')'                             #NestedExpr
      | '+' e = expr                             #UPlusExpr
      | '-' e = expr                             #UMinusExpr
      | n = NUM                                  #NumExpr
      ;

// It has a lot of annotations, if we would remove them the "naked" grammar would look like this:
// expr  : expr '*' expr
//       | expr '/' expr
//       | expr '+' expr
//       | expr '-' expr
//       | expr '^' expr
//       | '(' expr ')'
//       | NUM
//       ;

// What is the role of the annotations?

// First, annotations like
//   lhs = expr
// allows us to easily access the sub-expression in the AST with name "lhs"

// Second, annotations like
// #TimesExpr
// tell ANTLR how to name class of the the AST nodes. In the example,
// a multiplication will be stored as an object of a class with the name "TimesExpr"

// The annotation <assoc=right> allows us to specify that
// exponentiation is right-associative.
// Left-associativity is the default so we don't need annotations for it

// This is how we specify numerical tokens, i.e. numbers in scientific annotation
// by using a regular expression
//NUM : ('+'|'-')? ('0'..'9')+ ( '.' ('0'..'9')+)?  ('E' ('+'|'-')? ('0'..'9')+ )? ;
NUM : ('0'..'9')+ ( '.' ('0'..'9')+)?  ('E' ('+'|'-')? ('0'..'9')+ )? ;

// Note that, contrary, to the "formal" convention:
// - non-terminals start with lower case characters
// - terminals/tokens start with upper case characters

// Another commonly used token is for blank spaces.
// The part "-> skip" tells ANTLR to throw them away.
WS    : [ \t\r\n]+ -> skip ;
