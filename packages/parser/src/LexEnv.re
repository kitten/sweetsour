type argMode =
  | StringArg
  | ContentArg;

type mode =
  | Main
  | Str(char)
  | StrEnd(Token.quote);

type t = {
  buffer: option(Source.input),
  mode: mode,
  prev: Token.value,
  prevPos: Loc.position,
  pos: Loc.position,
  source: unit => Source.input
};

let prev = (env: t) => env.prev;
let mode = (env: t) => env.mode;

let pos = (env: t) =>
  switch(env.buffer) {
  | None => env.pos
  | Some(_) => env.prevPos
  };

let make = (source: unit => Source.input) => {
  buffer: None,
  mode: Main,
  prev: T_EOF,
  prevPos: Loc.makePos(),
  pos: Loc.makePos(),
  source
};

let buffer = (x: Source.input, env: t) => {
  ...env,
  buffer: Some(x)
};

let newline = (env: t): t => {
  ...env,
  prevPos: env.pos,
  pos: Loc.advanceRow(env.pos)
};

let newchar = (env: t): t => {
  ...env,
  prevPos: env.pos,
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

let emitToken = (
  env: t,
  start: Loc.position,
  token: Token.value
): (t, Token.t) => {
  let loc = Loc.make(start, pos(env));
  let env = { ...env, prev: token };
  (env, Token(token, loc))
};
