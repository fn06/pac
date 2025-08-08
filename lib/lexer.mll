{
open Parser

exception SyntaxError of string
}

let white = [' ' '\t' '\n']+

let word = ['a'-'z' 'A'-'Z' '0'-'9' '_' '.']+

rule read =
  parse
  | white     { read lexbuf }
  | "->"      { ARROW }
  | ';'       { SEMICOLON }
  | ','       { COMMA }
  | word as s { WORD s }
  | ')'       { RPAREN }
  | '('       { LPAREN }
  | eof       { EOF }
  | _         { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
