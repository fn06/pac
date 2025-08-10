%token <string> WORD
%token LPAREN RPAREN
%token ARROW
%token SEMICOLON
%token COMMA
%token EOF

%start <Ast.instance> instance
%start <Ast.query> query
%start <Ast.packages> packages
%%

instance:
  | i = entries; EOF { i }

query:
  | q = targets; EOF { q }

packages:
  | EOF; { [ ] }
  | p = package; EOF; { [ p ] }
  | p = package; COMMA; ps = packages; { p :: ps }

targets:
  | { [] }
  | t = target; ts = targets { t :: ts }

entries:
  | { [ ] }
  | d = entry; SEMICOLON; ds = entries { d :: ds }

entry:
  | p = package; rs = relations; { (p, rs) }

relations:
  | { [ ] }
  | r = relation; { [ r ] }
  | r = relation; COMMA; rs = relations { r :: rs }

relation:
  | ARROW; d = target { let (n, vs) = d in (n, vs) }

target:
  | n = WORD; LPAREN; vs = versions; RPAREN { (n, vs) }

/* TODO version formula */

versions:
  | v = WORD { [ v ] }
  | v = WORD; vs = versions { v :: vs }

package:
  | name = WORD; version = WORD { (name, version) }
