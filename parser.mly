%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT INCREMENT DECREMENT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOLEAN VOID STRING
%token LBRAC RBRAC COLON
%token PIXEL
%token <int> LITERAL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start expr
%type <Ast.expr> expr 

%%

type:
    INT                             { Int }
  | BOOLEAN                         { Boolean }
  | VOID                            { Void }
  | STRING                          { String }
  | PIXEL                           { Pixel }

expr:
    LITERAL                                                { Literal($1) }
  | TRUE                                                   { BoolLit(true) }
  | FALSE                                                  { BoolLit(false) }
  | expr PLUS   expr                                       { Binop($1, Add,   $3) }
  | expr MINUS  expr                                       { Binop($1, Sub,   $3) }
  | expr TIMES  expr                                       { Binop($1, Mult,  $3) }
  | expr DIVIDE expr                                       { Binop($1, Div,   $3) }
  | expr EQ     expr                                       { Binop($1, Equal, $3) }
  | expr NEQ    expr                                       { Binop($1, Neq,   $3) }
  | expr LT     expr                                       { Binop($1, Less,  $3) }
  | expr LEQ    expr                                       { Binop($1, Leq,   $3) }
  | expr GT     expr                                       { Binop($1, Greater, $3) }
  | expr GEQ    expr                                       { Binop($1, Geq,   $3) }
  | expr AND    expr                                       { Binop($1, And,   $3) }
  | expr OR     expr                                       { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG                                   { Unop(Neg, $2) }
  | NOT expr                                               { Unop(Not, $2) }
  | LPAREN expr RPAREN                                     { $2 }
  | pixel_lit                                              { $1 }

pixel_lit:
    LPAREN expr RPAREN { PixelLit($2) }
