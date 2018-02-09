// This grammar is annotated in a way that it will produce
// a rather "pure" abstract syntax tree where each node
// will correspond to a production, thus reducting the need
// to inspect it in our calculator
grammar Calculator;

start : expr EOF ;

// Note that the annotation tells ANTLR how to name the parse tree nodes
// and their children. For example, in the first production we specify
// that the parse tree's class will use the name "TimesExpr" and the operands
// will be children called "lhs" and "rhs"
expr  :               lhs = expr '*' rhs = expr  #TimesExpr
      |               lhs = expr '/' rhs = expr  #DivExpr
      |               lhs = expr '+' rhs = expr  #PlusExpr
      |               lhs = expr '-' rhs = expr  #MinusExpr
      | <assoc=right> lhs = expr '^' rhs = expr  #PowExpr
      | '(' expr ')'                             #NestedExpr
      | NUM                                      #NumExpr
      ;

NUM : ('+'|'-')? ('0'..'9')+ ( '.' ('0'..'9')+)?  ('E' ('+'|'-')? ('0'..'9')+ )? ;

// ignore blank spaces
WS    : [ \t\r\n]+ -> skip ;
