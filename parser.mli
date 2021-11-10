type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | INCREMENT
  | DECREMENT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | VOID
  | STRING
  | LBRAC
  | RBRAC
  | COLON
  | CHAR
  | BAR
  | EXP
  | PIXEL
  | DOT
  | ROWS
  | COLS
  | GRAY
  | LITERAL of (int)
  | ID of (string)
  | STR_LIT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
