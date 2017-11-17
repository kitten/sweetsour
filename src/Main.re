open Common;

let tokenise = (strings: array(string), interpolations: array(interpolation)) : array(Lexer.token) =>
  Input.input(strings, interpolations)
    |> Lexer.lexer
    |> LazyStream.toArray;

let parse = (strings: array(string), interpolations: array(interpolation)) : array(Parser.node) =>
  Input.input(strings, interpolations)
    |> Lexer.lexer
    |> Parser.parser
    |> LazyStream.toArray;
