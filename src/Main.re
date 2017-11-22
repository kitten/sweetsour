open Common;

let tokenise = (strings: array(string), interpolations: array(interpolation)) : array(Lexer.token) =>
  Input.input(strings, interpolations)
    |> Lexer.lexer
    |> LazyStream.toArray;

let parse = (strings: array(string), interpolations: array(interpolation)) : array(Output.outputNode) =>
  Input.input(strings, interpolations)
    |> Lexer.lexer
    |> Parser.parser
    |> Output.output
    |> LazyStream.toArray;
