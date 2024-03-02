grammar Kotlisp;
/*------------------------------------------------------------------
 * A very basic implementation of a Lisp grammar.
 *------------------------------------------------------------------*/

/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/

kotlisp : fun*;
fun : OP 'fun' NAME s_expression CP;

s_expression: OP (variable | loop | conditional | fun_call | print_ | math_expression | read_line)? s_expression* CP;

math_expression : ((STRING | list_ | NUMBER | NAME | BOOLEAN  | index)
                        |
                      ( (PLUS | MINUS | MULT | DIV | XOR | AND | OR | EQUALS | NOT_EQUALS | GREATER | GREATER_OR_EQUALS | LESS | LESS_OR_EQUALS | MOD)
                      (list_ | NUMBER | NAME | BOOLEAN | index)+));


list_ : 'list' OP (NUMBER+ | STRING+ | BOOLEAN+) CP;

fun_call : NAME OP CP;

variable : 'setq' NAME s_expression;

loop: 'dotimes' OP NAME (NUMBER | NAME) CP s_expression;

conditional: 'if' s_expression s_expression s_expression;

index : NAME '[' (NUMBER | NAME) ']';

print_: 'print' s_expression;

read_line: 'read-line' OP ('int' | 'str' | 'bool') CP;

/*------------------------------------------------------------------
 * LEXER RULES
 *------------------------------------------------------------------*/
// OPERATORS
PLUS : '+';
MINUS : '-';
MULT : '*';
DIV : '/';
XOR : '^';
AND: '&';
MOD: '%';
OR: '|';
EQUALS : '=';
NOT_EQUALS : '!=';
GREATER : '>';
GREATER_OR_EQUALS : '>=';
LESS : '<';
LESS_OR_EQUALS : '<=';
OP : '(';
CP : ')';


NUMBER : '-'? DIGIT+ ;

BOOLEAN : ('true' | 'false');

STRING : '"'(LETTER | DIGIT | WHITESPACE | '?' | '!' | '>' | '<' | ',' | '.'| '\\n')+'"';

NAME: LETTER+;

WHITESPACE : [\r\n\t ,]+ -> skip;

COMMENT: ('#' (LETTER | DIGIT | WHITESPACE)*) -> skip;

fragment DIGIT : '0'..'9';

fragment LETTER : LOWER | UPPER ;

fragment LOWER : ('a'..'z') ;
fragment UPPER : ('A'..'Z') ;
