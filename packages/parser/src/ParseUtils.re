/* Skips over semis and returns the next token after */
let rec sourceAfterSemis = (env: ParseEnv.t) => {
  let (env, token) = ParseEnv.source(env);

  switch (token) {
  | Token(T_SYMBOL_SEMI, _) => sourceAfterSemis(env)
  | _ => (env, token)
  }
};

/* Checks whether two Loc.t are separated */
let isDistantOffset = (a: Loc.t, b: Loc.t) =>
  a._end.row !== b.start.row
    || a._end.offset + 1 < b.start.offset;

/* Checks whether two Loc.t are on separate lines */
let isDistantRow = (a: Loc.t, b: Loc.t) =>
  a._end.row !== b.start.row;
