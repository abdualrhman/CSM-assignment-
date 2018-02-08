grammar Hello;

start : 'I\'m' name = NAME EOF ;

NAME : ('A'..'Z'|'a'..'z')+;

// ignore blank spaces
WS    : [ \t\r\n]+ -> skip ;
