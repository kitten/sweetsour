open Common;

let tokenise = (strings: array(string), interpolations: array(interpolation)) : array(Lexer.token) => {
  let lexerStream = Lexer.lexer(Input.input(strings, interpolations));
  LazyStream.toArray(lexerStream);
};

let parse = (strings: array(string), interpolations: array(interpolation)) : array(Parser.node) => {
  let lexerStream = Lexer.lexer(Input.input(strings, interpolations));
  let parserStream = Parser.parser(lexerStream);
  LazyStream.toArray(parserStream);
};
