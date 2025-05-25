let () =
  let v =
    Pac.Parser.expression Pac.Lexer.read (Lexing.from_channel In_channel.stdin)
  in
  Pac.Ast.pp Format.std_formatter v
