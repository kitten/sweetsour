open Common;
open Token;

type result =
  | Emit(LexEnv.t, Token.value)
  | Continue(LexEnv.t);

exception UnknownChar(LexEnv.t, char);
exception UnexpectedChar(LexEnv.t, char);
exception UnexpectedInput(LexEnv.t, Source.input);
exception UnexpectedWhitespace(LexEnv.t);
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
  | S_CHAR('\n') => raise(UnexpectedWhitespace(env))
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
    lexString(LexEnv.source(env), quote, str ++ charToStr(c))
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
    let (env, input) = LexEnv.peek(env);

    let env = switch (LexEnv.prev(env), input) {
    | (T_LITERAL_WORD("calc"), _) => LexEnv.switchMode(CalcFn(0), env)
    | (T_LITERAL_WORD("url"), S_CHAR('"' | '\'')) => env
    | (T_LITERAL_WORD("url"), S_CHAR('\n' | '\t' | '\r' | ' ')) =>
      LexEnv.switchMode(UrlFn, lexWhitespaces((env, input)))
    | (T_LITERAL_WORD("url"), _) => LexEnv.switchMode(UrlFn, env)
    | _ => env
    };

    Emit(env, T_BRACKET_ROUND(T_PAIR_OPENING))
  }

  | ')' => Emit(env, T_BRACKET_ROUND(T_PAIR_CLOSING))

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

let rec lexUrl = (
  (env, input): (LexEnv.t, Source.input),
  str: string
): result => {
  switch (input) {
  | S_CHAR('\\') => {
    let (env, escaped) = lexEscaped(LexEnv.source(env));
    let str = str ++ "\\" ++ escaped;
    lexUrl(LexEnv.source(env), str)
  }

  | S_CHAR(' ' | '\t' | '\r' | '\n') => {
    let env = lexWhitespaces(LexEnv.source(env));
    let (env, input) = LexEnv.source(env);

    switch (input) {
    | S_CHAR(')') => lexUrl((env, input), str)
    | _ => raise(UnexpectedWhitespace(env))
    }
  }

  | S_CHAR(')') when str === "" => {
    let env = LexEnv.switchMode(Main, env);
    Emit(env, T_BRACKET_ROUND(T_PAIR_CLOSING))
  }
  | S_CHAR(')') => {
    let env = LexEnv.buffer(input, LexEnv.switchMode(Main, env));
    Emit(env, T_LITERAL_STRING(str))
  }

  | S_CHAR(('"' | '\'') as c) =>
    raise(UnexpectedChar(env, c))

  | S_CHAR(c) =>
    lexUrl(LexEnv.source(env), str ++ charToStr(c))
  | S_REF(r) when str === "" =>
    Emit(env, T_REF(r))
  | S_REF(_) =>
    Emit(LexEnv.buffer(input, env), T_LITERAL_STRING(str))
  | S_EOF => raise(UnexpectedEof(env))
  }
};

let rec lexCalc = (
  (env, input): (LexEnv.t, Source.input),
  depth: int,
  str: string
): result => {
  switch (input) {
  | S_CHAR('\\') => {
    let (env, escaped) = lexEscaped(LexEnv.source(env));
    let str = str ++ "\\" ++ escaped;
    lexCalc(LexEnv.source(env), depth, str)
  }

  | S_CHAR('(') => {
    let depth = depth + 1;
    let env = LexEnv.switchMode(CalcFn(depth), env);
    lexCalc(LexEnv.source(env), depth, str ++ "(")
  }
  | S_CHAR(')') when depth > 0 => {
    let depth = depth - 1;
    let env = LexEnv.switchMode(CalcFn(depth), env);
    lexCalc(LexEnv.source(env), depth, str ++ ")")
  }
  | S_CHAR(')') when str === "" => {
    let env = LexEnv.switchMode(Main, env);
    Emit(env, T_BRACKET_ROUND(T_PAIR_CLOSING))
  }
  | S_CHAR(')') => {
    let env = LexEnv.buffer(input, LexEnv.switchMode(Main, env));
    Emit(env, T_LITERAL_STRING(str))
  }

  | S_CHAR(('"' | '\'') as c) =>
    raise(UnexpectedChar(env, c))

  | S_CHAR(c) =>
    lexCalc(LexEnv.source(env), depth, str ++ charToStr(c))
  | S_REF(r) when str === "" =>
    Emit(env, T_REF(r))
  | S_REF(_) =>
    Emit(LexEnv.buffer(input, env), T_LITERAL_STRING(str))
  | S_EOF => raise(UnexpectedEof(env))
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
    | UrlFn => lexUrl((env, input), "")
    | CalcFn(depth) => lexCalc((env, input), depth, "")
    | Str(quote) => lexString((env, input), quote, "")
    | StrEnd(quote) => lexStringEnd(env, input, quote)
    };

    switch (output) {
    | Emit(env, token) => LexEnv.emitToken(env, start, token)
    | Continue(env) => explode(env)
    }
  };

  explode(env)
};
