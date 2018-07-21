open Common;
open Token;

type result =
  | Emit(LexEnv.t, Token.value)
  | Continue(LexEnv.t);

exception TODO;
exception UnknownChar(LexEnv.t, char);
exception UnexpectedChar(LexEnv.t, char);
exception UnexpectedInput(LexEnv.t, Source.input);
exception UnexpectedEof(LexEnv.t);

let rec lexWhitespaces = ((env, input): (LexEnv.t, Source.input)): LexEnv.t => {
  switch (input) {
  | S_CHAR('\n' | '\t' | '\r' | ' ') =>
    lexWhitespaces(LexEnv.source(env))
  | _ => LexEnv.buffer(input, env)
  }
};

let rec lexComment = ((env, input): (LexEnv.t, Source.input)): LexEnv.t => {
  switch (input) {
  | S_CHAR('*') => {
    let (env, input) = LexEnv.source(env);
    switch (input) {
    | S_CHAR('/') => env
    | _ => lexComment(LexEnv.source(env))
    }
  }
  | S_EOF => raise(UnexpectedEof(env))
  | _ => lexComment(LexEnv.source(env))
  }
};

let rec lexHex = (
  (env, input): (LexEnv.t, Source.input),
  str: string
): (LexEnv.t, string) => {
  switch (input) {
  | S_CHAR(c) when LexUtils.isHex(c) =>
    lexHex(LexEnv.source(env), str ++ charToStr(c))
  | S_CHAR(' ') => (env, str ++ " ")
  | S_REF(_) => raise(UnexpectedInput(env, input))
  | _ => (LexEnv.buffer(input, env), str)
  }
};

let lexEscaped = (
  (env, input): (LexEnv.t, Source.input)
): (LexEnv.t, string) => {
  switch (input) {
  | S_CHAR(c) when LexUtils.isHex(c) =>
    lexHex(LexEnv.source(env), charToStr(c))
  | S_CHAR(c) => (env, charToStr(c))
  | S_REF(_) => raise(UnexpectedInput(env, input))
  | S_EOF => raise(UnexpectedEof(env))
  }
};

let rec lexWord = (
  (env, input): (LexEnv.t, Source.input),
  str: string
): (LexEnv.t, string) => {
  switch (input) {
  | S_CHAR('\\') => {
    let (env, escaped) = lexEscaped(LexEnv.source(env));
    lexWord(LexEnv.source(env), str ++ "\\" ++ escaped)
  }

  | S_CHAR(('%' | '-' | '_') as c) =>
    lexWord(LexEnv.source(env), str ++ charToStr(c))

  | S_CHAR(c) when LexUtils.isWordStart(c) =>
    lexWord(LexEnv.source(env), str ++ charToStr(c))

  | S_REF(_) => raise(UnexpectedInput(env, input))
  | _ => (LexEnv.buffer(input, env), str)
  }
};

let rec lexString = (
  (env, input): (LexEnv.t, Source.input),
  quote: char,
  str: string
): result => {
  switch (input) {
  | S_CHAR('\n') => raise(UnexpectedChar(env, '\n'))
  | S_CHAR('\\') => {
    let (env, escaped) = lexEscaped(LexEnv.source(env));
    let str = str ++ "\\" ++ escaped;
    lexString(LexEnv.source(env), quote, str)
  }

  | S_CHAR(('"' | '\'') as c) when quote === c => {
    let quote = switch (c) {
    | '"' => T_QUOTE_DOUBLE
    | '\'' => T_QUOTE_SINGLE
    | _ => raise(UnexpectedChar(env, c))
    };

    if (str === "") {
      Emit(LexEnv.switchMode(Main, env), T_SYMBOL_QUOTE(quote))
    } else {
      let env = LexEnv.switchMode(StrEnd(quote), env);
      Emit(env, T_LITERAL_STRING(str))
    }
  }

  | S_CHAR(c) =>
    let str = str ++ charToStr(c);
    lexString(LexEnv.source(env), quote, str)
  | S_REF(r) when str === "" =>
    Emit(env, T_REF(r))
  | S_REF(_) =>
    Emit(LexEnv.buffer(input, env), T_LITERAL_STRING(str))
  | S_EOF => raise(UnexpectedEof(env))
  };
};

let lexMainChar = (env: LexEnv.t, x: char): result => {
  switch (x) {
  | '{' => Emit(env, T_BRACKET_CURLY(T_PAIR_OPENING))
  | '}' => Emit(env, T_BRACKET_CURLY(T_PAIR_CLOSING))
  | '[' => Emit(env, T_BRACKET_SQUARE(T_PAIR_OPENING))
  | ']' => Emit(env, T_BRACKET_SQUARE(T_PAIR_CLOSING))
  | ')' => Emit(env, T_BRACKET_ROUND(T_PAIR_CLOSING))
  | '!' => Emit(env, T_SYMBOL_EXCLAMATION)
  | '=' => Emit(env, T_SYMBOL_EQUAL)
  | ':' => Emit(env, T_SYMBOL_COLON)
  | ';' => Emit(env, T_SYMBOL_SEMI)
  | '+' => Emit(env, T_SYMBOL_PLUS)
  | '>' => Emit(env, T_SYMBOL_GREATER)
  | '~' => Emit(env, T_SYMBOL_TILDE)
  | ',' => Emit(env, T_SYMBOL_COMMA)
  | '|' => Emit(env, T_SYMBOL_PIPE)
  | '$' => Emit(env, T_SYMBOL_DOLLAR)
  | '*' => Emit(env, T_SYMBOL_ASTERISK)
  | '&' => Emit(env, T_SYMBOL_AMPERSAND)
  | '^' => Emit(env, T_SYMBOL_CARET)

  | '(' => {
    /*
    let env = switch (LexEnv.prev(env)) {
    | T_LITERAL_WORD("calc") => LexEnv.switchMode(Str(')'), env)
    | _ => env
    };
    */

    Emit(env, T_BRACKET_ROUND(T_PAIR_OPENING))
  }

  | '/' => {
    let (env, input) = LexEnv.source(env);
    switch (input) {
    | S_CHAR('*') => Continue(lexComment(LexEnv.source(env)))
    | _ => raise(UnexpectedInput(env, input))
    }
  }

  | '"' => {
    let env = LexEnv.switchMode(Str('"'), env);
    Emit(env, T_SYMBOL_QUOTE(T_QUOTE_DOUBLE))
  }

  | '\'' => {
    let env = LexEnv.switchMode(Str('\''), env);
    Emit(env, T_SYMBOL_QUOTE(T_QUOTE_SINGLE))
  }

  | '\n'
  | '\r'
  | '\t'
  | ' ' => Continue(env)

  | '@' => {
    let (env, res) = lexWord(LexEnv.source(env), "");
    Emit(env, T_LITERAL_ATWORD(res))
  }

  | '\\' => {
    let (env, escaped) = lexEscaped(LexEnv.source(env));
    let (env, res) = lexWord(LexEnv.source(env), "\\" ++ escaped);
    Emit(env, T_LITERAL_WORD(res))
  }

  | c when LexUtils.isWordStart(c) => {
    let (env, res) = lexWord(LexEnv.source(env), charToStr(c));
    Emit(env, T_LITERAL_WORD(res))
  }

  | _ => raise(UnknownChar(env, x))
  }
};

let lexMain = (env: LexEnv.t, x: Source.input) => {
  switch (x) {
  | S_CHAR(c) => lexMainChar(env, c)
  | S_REF(r) => Emit(env, T_REF(r))
  | S_EOF => Emit(env, T_EOF)
  }
};

let lexStringEnd = (env: LexEnv.t, input: Source.input, quote: Token.quote) => {
  let env = LexEnv.buffer(input, env);
  Emit(LexEnv.switchMode(Main, env), T_SYMBOL_QUOTE(quote));
};

let lex = (env: LexEnv.t): (LexEnv.t, Token.t) => {
  let rec explode = (env) => {
    let mode = LexEnv.mode(env);
    let (env, input) = LexEnv.source(env);
    let start = LexEnv.pos(env);

    let output = switch (mode) {
    | Main => lexMain(env, input)
    | Str(c) => lexString((env, input), c, "")
    | StrEnd(q) => lexStringEnd(env, input, q)
    };

    switch (output) {
    | Emit(env, token) => LexEnv.emitToken(env, start, token)
    | Continue(env) => explode(env)
    }
  };

  explode(env)
};
