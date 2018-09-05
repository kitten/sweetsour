open Jest;
open Token;
open Lexer;

let it = test;

let test_lex = (str: string) => {
  let source = Source.make([| str |], [||]);
  let env = LexEnv.make(source);
  let rec explode = (env: LexEnv.t, ls: list(Token.value)) => {
    let (env, token) = lex(env);
    let Token(value, _) = token;

    switch (value) {
    | T_EOF => List.rev(ls)
    | _ => explode(env, [value, ...ls])
    }
  };

  explode(env, [])
};

let test_lex_pos = (str: string) => {
  let source = Source.make([| str |], [||]);
  let env = LexEnv.make(source);
  let rec explode = (env: LexEnv.t, ls: list(Token.t)) => {
    let (env, token) = lex(env);

    switch (token) {
    | Token(T_EOF, _) => List.rev(ls)
    | _ => explode(env, [token, ...ls])
    }
  };

  explode(env, [])
};

let quickLoc = ((ya, xa): (int, int), (yb, xb): (int, int)): Loc.t => {
  start: { row: ya, offset: xa },
  _end: { row: yb, offset: xb }
};

describe("Lexer", () => {
  describe("lex with string source", () => {
    open Expect;
    open! Expect.Operators;

    it("tokenises individual chars in main correctly", () => {
      let str = "{} [] () ! = : ; + > ~ , | $ * & ^";

      expect(test_lex(str)) == [
        T_BRACKET_CURLY(T_PAIR_OPENING),
        T_BRACKET_CURLY(T_PAIR_CLOSING),
        T_BRACKET_SQUARE(T_PAIR_OPENING),
        T_BRACKET_SQUARE(T_PAIR_CLOSING),
        T_BRACKET_ROUND(T_PAIR_OPENING),
        T_BRACKET_ROUND(T_PAIR_CLOSING),
        T_SYMBOL_EXCLAMATION,
        T_SYMBOL_EQUAL,
        T_SYMBOL_COLON,
        T_SYMBOL_SEMI,
        T_SYMBOL_PLUS,
        T_SYMBOL_GREATER,
        T_SYMBOL_TILDE,
        T_SYMBOL_COMMA,
        T_SYMBOL_PIPE,
        T_SYMBOL_DOLLAR,
        T_SYMBOL_ASTERISK,
        T_SYMBOL_AMPERSAND,
        T_SYMBOL_CARET,
      ]
    });

    it("skips over comments", () => {
      let str = "{/* ... */}/* ... */[]";

      expect(test_lex(str)) == [
        T_BRACKET_CURLY(T_PAIR_OPENING),
        T_BRACKET_CURLY(T_PAIR_CLOSING),
        T_BRACKET_SQUARE(T_PAIR_OPENING),
        T_BRACKET_SQUARE(T_PAIR_CLOSING),
      ]
    });

    it("tokenises strings", () => {
      let str = "'test'\"test\"";

      expect(test_lex(str)) == [
        T_SYMBOL_QUOTE(T_QUOTE_SINGLE),
        T_LITERAL_STRING("test"),
        T_SYMBOL_QUOTE(T_QUOTE_SINGLE),
        T_SYMBOL_QUOTE(T_QUOTE_DOUBLE),
        T_LITERAL_STRING("test"),
        T_SYMBOL_QUOTE(T_QUOTE_DOUBLE),
      ]
    });

    it("tokenises unquoted url() arguments", () => {
      let str = "url(http://example.com)";

      expect(test_lex(str)) == [
        T_LITERAL_WORD("url"),
        T_BRACKET_ROUND(T_PAIR_OPENING),
        T_LITERAL_STRING("http://example.com"),
        T_BRACKET_ROUND(T_PAIR_CLOSING)
      ]
    });

    it("tokenises unquoted url() arguments skipping whitespaces", () => {
      let str = "url(\r\n\t  http://example.com\r\n\t  )";

      expect(test_lex(str)) == [
        T_LITERAL_WORD("url"),
        T_BRACKET_ROUND(T_PAIR_OPENING),
        T_LITERAL_STRING("http://example.com"),
        T_BRACKET_ROUND(T_PAIR_CLOSING)
      ]
    });

    it("skips over all whitespaces", () => {
      let str = " \n\r\t";
      expect(test_lex(str)) == []
    });

    it("tokenises at-words", () => {
      let str = "@media;";
      expect(test_lex(str)) == [
        T_LITERAL_ATWORD("media"),
        T_SYMBOL_SEMI
      ]
    });

    it("tokenises words", () => {
      let str = "color:";
      expect(test_lex(str)) == [
        T_LITERAL_WORD("color"),
        T_SYMBOL_COLON
      ]
    });

    it("tokenises words starting with escaped content", () => {
      /* this can indeed be valid CSS literal; yep, I don't know why either */
      let str = "\\?color";
      expect(test_lex(str)) == [
        T_LITERAL_WORD("\\?color"),
      ]
    });

    it("tokenises words containing escaped content", () => {
      let str = "co\\?lor\\?";
      expect(test_lex(str)) == [
        T_LITERAL_WORD("co\\?lor\\?"),
      ]
    });

    it("tokenises words containing escaped hex codes", () => {
      let str = "co\\0f0 lor\\abc:";
      expect(test_lex(str)) == [
        T_LITERAL_WORD("co\\0f0 lor\\abc"),
        T_SYMBOL_COLON
      ]
    });

    it("tokenises words containing escaped hex codes", () => {
      let str = "co\\0f0 lor\\abc:";
      expect(test_lex(str)) == [
        T_LITERAL_WORD("co\\0f0 lor\\abc"),
        T_SYMBOL_COLON
      ]
    });

    it("throws when words end in backslash only", () => {
      expect(() => test_lex("color\\")) |> toThrow;
    });

    it("throws when comments are not closed", () => {
      expect(() => test_lex("/*")) |> toThrow;
    });

    it("throws when strings are not closed", () => {
      expect(() => test_lex("'test")) |> toThrow;
    });

    it("throws when whitespaces in unquoted url() are found", () => {
      expect(() => test_lex("url(x x)")) |> toThrow;
    });
  });

  describe("lex with correct Loc info", () => {
    open Expect;
    open! Expect.Operators;

    it("tokenises words and their positions", () => {
      let str = "\n color :\n    blue;";
      expect(test_lex_pos(str)) == [
        Token(T_LITERAL_WORD("color"), quickLoc((2, 2), (2, 6))),
        Token(T_SYMBOL_COLON, quickLoc((2, 8), (2, 8))),
        Token(T_LITERAL_WORD("blue"), quickLoc((3, 5), (3, 8))),
        Token(T_SYMBOL_SEMI, quickLoc((3, 9), (3, 9)))
      ]
    });
  });
});
