type t = {
  buffer: option(Token.t),
  tokens: list(Token.t)
};

exception UnexpectedLexEof(t);

let fromLexEnv = (lexEnv: LexEnv.t) => {
  let rec explode = (env: LexEnv.t, ls: list(Token.t)) => {
    let (env, token) = Lexer.lex(env);

    switch (token) {
    | Token(T_EOF, _) as x => List.rev([x, ...ls])
    | x => explode(env, [x, ...ls])
    }
  };

  let tokens = explode(lexEnv, []);
  { buffer: None, tokens }
};

let make = (tokens: list(Token.t)) => {
  buffer: None,
  tokens
};

let buffer = (x: Token.t, env: t) => {
  ...env,
  buffer: Some(x)
};

let source = (env: t) =>
  switch (env.buffer, env.tokens) {
  | (None, [token, ...tokens]) => ({ ...env, tokens }, token)
  | (None, []) => raise(UnexpectedLexEof(env))
  | (Some(token), _) => ({ ...env, buffer: None }, token)
  };
