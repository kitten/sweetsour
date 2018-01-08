open Common;

let tokenise = (strings: array(string), interpolations: array(interpolation)) : array(Lexer.token) =>
  Input.input(strings, interpolations)
    |> Lexer.lexer
    |> LazyStream.toArray;

let parse = (strings: array(string), interpolations: array(interpolation)) : array((int, IstfNode.rawNodePayload)) =>
  Input.input(strings, interpolations)
    |> Lexer.lexer
    |> Parser.parser
    |> Prefixer.prefixer
    |> Output.output
    |> LazyStream.toArray;

let stringOfRef = [@bs] (x: interpolation) => "";

let stringify = (strings: array(string), interpolations: array(interpolation)) : string =>
  Input.input(strings, interpolations)
    |> Lexer.lexer
    |> Parser.parser
    |> Prefixer.prefixer
    |> Stringifier.stringifier(~stringOfRef=stringOfRef);
