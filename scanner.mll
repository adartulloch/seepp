{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }                   
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRAC }
| ']'      { RBRAC }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| "<"      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "boolean"   { BOOL }
| "void"   { VOID }
| "string" { STRING }
| "true"   { TRUE }
| "false"  { FALSE }
| "char"   { CHAR }
| "pixel"  { PIXEL }
| "/*"     { Scomment }   
| "*/"     { Ecomment } 
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| ['a'-'z']+ as var { VARIABLE(var) } (* //TODO: more complex regex to capture variable name *)
| eof { EOF }

