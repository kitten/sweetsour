import { next as Stream_next } from './lib/es6/src/LazyStream';
import { input } from './lib/es6/src/Input';
import { lexer } from './lib/es6/src/Lexer';

function next (lexerStream) {
  try {
    return Stream_next(lexerStream);
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

export { tokenise };
