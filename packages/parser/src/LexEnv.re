type mode =
  | Main
  | UrlFn
  | CalcFn(int)
  | Str(char)
  | StrEnd(Token.quote);

type t = {
  mutable cursor: int,
  sourceSize: int,
  source: array(Source.input),
  buffer: Source.input,
  mode: mode,
  prev: Token.value,
  prevPos: Loc.position,
  pos: Loc.position
};

let prev = (env: t) => env.prev;
let mode = (env: t) => env.mode;

let pos = (env: t) =>
  switch(env.buffer) {
  | S_EOF => env.pos
  | _ => env.prevPos
  };

let make = (source: array(Source.input)) => {
  cursor: 0,
  sourceSize: Array.length(source),
  source,
  buffer: S_EOF,
  mode: Main,
  prev: T_EOF,
  prevPos: Loc.makePos(),
  pos: Loc.makePos()
};

let buffer = (x: Source.input, env: t) => {
  ...env,
  buffer: x
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
  | S_EOF when (env.cursor >= env.sourceSize) =>
    (env, Source.S_EOF)
  | S_EOF => {
    let x = Array.unsafe_get(env.source, env.cursor);
    env.cursor = env.cursor + 1;

    switch (x) {
    | S_CHAR('\n') => (newline(env), x)
    | S_CHAR(_) => (newchar(env), x)
    | _ => (env, x)
    }
  }
  | x => ({ ...env, buffer: S_EOF }, x)
  }
};

let peek = (env: t) => {
  let (env, input) = source(env);
  (buffer(input, env), input)
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
