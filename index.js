var Stream = require('./lib/es6/src/LazyStream');
var input = require('./lib/es6/src/Input').input;
var lexer = require('./lib/es6/src/Lexer').lexer;

function next (lexerStream) {
  try {
    return Stream.next(lexerStream);
  } catch (err) {
    return undefined;
  }
}

function tokenise (strings, interpolations) {
  var inputStream = input(strings, interpolations);
  var lexerStream = lexer(inputStream);
  var tokens = [];
  var token;

  while ((token = next(lexerStream)) !== undefined) {
    tokens.push(token);
  }

  return tokens;
}

module.exports = tokenise;
