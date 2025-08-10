{
open Parser

exception SyntaxError of string
}

let word = ['a'-'z' 'A'-'Z' '0'-'9' '_' '.']+

rule read =
  parse
  | [' ' '\t' '\n' '\r'] + { read lexbuf }
  | "->"                   { ARROW }
  | ';'                    { SEMICOLON }
  | ','                    { COMMA }
  | word as s              { WORD s }
  | ')'                    { RPAREN }
  | '('                    { LPAREN }
  | eof                    { EOF }
  | _                      { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
