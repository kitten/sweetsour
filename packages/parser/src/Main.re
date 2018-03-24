open Common;

let tokeniseTemplate = (strings: array(string), interpolations: array(interpolation)) : Lexer.lexerStream =>
  Input.input(strings, interpolations)
    |> Lexer.lexer;

let parseTemplate = (strings: array(string), interpolations: array(interpolation)) : IstfNode.nodeStream =>
  Input.input(strings, interpolations)
    |> Lexer.lexer
    |> Parser.parser;

let nodeStreamToOutput = (stream: IstfNode.nodeStream) : array((int, IstfNode.rawNodePayload)) =>
  stream
    |> Output.output
    |> LazyStream.toArray;
