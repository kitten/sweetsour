type argMode =
  | StringArg
  | ContentArg;

type mode =
  | Main
  | Str(char)
  | StrEnd(Token.quote);

type t = {
  source: unit => Source.input,
  buffer: option(Source.input),
  prev: Token.t,
  mode: mode,
  pos: Loc.position
};

let prev = (env: t) => env.prev;
let mode = (env: t) => env.mode;

let make = (source: unit => Source.input) => {
  buffer: None,
  mode: Main,
  prev: T_EOF,
  pos: Loc.makePos(),
  source
};

let buffer = (x: Source.input, env: t) => {
  ...env,
  buffer: Some(x)
};

let newline = (env: t): t => {
  ...env,
  pos: Loc.advanceRow(env.pos)
};

let newchar = (env: t): t => {
  ...env,
  pos: Loc.advanceOffset(env.pos)
};

let source = (env: t) => {
  switch (env.buffer) {
  | None => {
    let x = env.source();
    let env = switch (x) {
    | S_CHAR('\n') => newline(env)
    | S_CHAR(_) => newchar(env)
    | _ => env
    };

    (env, x)
  }
  | Some(x) => ({ ...env, buffer: None }, x)
  }
};

let switchMode = (mode: mode, env: t): t => {
  ...env,
  mode
};

let emitToken = (env: t, token: Token.t): (t, Token.t) => {
  let env = { ...env, prev: token };
  (env, token)
};
