%token <string> WORD 
%token LPAREN RPAREN
%token EOF

%start <Ast.expression> expression
%%

expression:
  | e = dependencies; EOF { e }

dependencies:
  | { [  ] }
  | d = dependency; ds = dependencies { d :: ds }

dependency:
  | p = package; LPAREN; ts = targets; RPAREN { (p, ts) }

target:
  | n = WORD; LPAREN; vs = versions; RPAREN   { (n, vs) }

targets:
  | { [] }
  | t = target; ts = targets   { t :: ts }

versions:
  | v = WORD { [ v ] }
  | v = WORD; vs = versions { v :: vs }

package:
  | name = WORD; version = WORD { (name, version) } 

