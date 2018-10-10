open Lexer;

let lex = (strs, interpolations) => {
  let source = Source.make(strs, interpolations);
  let env = LexEnv.make(source);

  let rec explode = (env: LexEnv.t) => {
    let (env, token) = lex(env);

    switch (token) {
    | Token(T_EOF, _) => ()
    | _ => explode(env)
    }
  };

  explode(env)
};
