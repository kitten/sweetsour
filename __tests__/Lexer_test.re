open Jest;
open Lexer;

let it = test;

let create_interpolation : int => Common.interpolation = [%bs.raw{|
  function(x) { return x; }
|}];

let tokenise = (cssString: string): array(token) => {
  let lexerStream = lexer(Input.input([|cssString|], [||]));
  LazyStream.toArray(lexerStream);
};

describe("Lexer", () => {
  describe("Single char tokens", () => {
    open Expect;
    open! Expect.Operators;

    it("tokenises single chars correctly", () => {
      expect(tokenise("{}[]()!=:;+&>*~,|$")) == [|
        Token(Brace(Opening), (1, 1), (1, 1)),
        Token(Brace(Closing), (1, 2), (1, 2)),
        Token(Bracket(Opening), (1, 3), (1, 3)),
        Token(Bracket(Closing), (1, 4), (1, 4)),
        Token(Paren(Opening), (1, 5), (1, 5)),
        Token(Paren(Closing), (1, 6), (1, 6)),
        Token(Exclamation, (1, 7), (1, 7)),
        Token(Equal, (1, 8), (1, 8)),
        Token(Colon, (1, 9), (1, 9)),
        Token(Semicolon, (1, 10), (1, 10)),
        Token(Plus, (1, 11), (1, 11)),
        Token(Ampersand, (1, 12), (1, 12)),
        Token(Arrow, (1, 13), (1, 13)),
        Token(Asterisk, (1, 14), (1, 14)),
        Token(Tilde, (1, 15), (1, 15)),
        Token(Comma, (1, 16), (1, 16)),
        Token(Pipe, (1, 17), (1, 17)),
        Token(Dollar, (1, 18), (1, 18))
      |];
    });

    it("skips over whitespace-chars counting newlines", () => {
      expect(tokenise("\t\r{   }\n{   }   test")) == [|
        Token(Brace(Opening), (1, 3), (1, 3)),
        Token(Brace(Closing), (1, 7), (1, 7)),
        Token(Brace(Opening), (2, 1), (2, 1)),
        Token(Brace(Closing), (2, 5), (2, 5)),
        Token(Word("test"), (2, 9), (2, 12))
      |];
    });

    it("throws when unexpected tokens are encountered", () => {
      expect(() => tokenise("^")) |> toThrowMessage("unexpected '^' while parsing tokens");
    });
  });

  describe("Words & At-Words", () => {
    open Expect;
    open! Expect.Operators;

    it("tokenises words correctly", () => {
      expect(tokenise("property #id .class_name 10% 10px hello-world")) == [|
        Token(Word("property"), (1, 1), (1, 8)),
        Token(Word("#id"), (1, 10), (1, 12)),
        Token(Word(".class_name"), (1, 14), (1, 24)),
        Token(Word("10%"), (1, 26), (1, 28)),
        Token(Word("10px"), (1, 30), (1, 33)),
        Token(Word("hello-world"), (1, 35), (1, 45))
      |];
    });

    it("tokenises words containing escaped characters correctly", () => {
      expect(tokenise("pro\\p\\erty\\0024")) == [|
        Token(Word("pro\\p\\erty\\0024"), (1, 1), (1, 15))
      |];
    });

    it("tokenises words starting with escaped characters correctly", () => {
      expect(tokenise("\\property")) == [|
        Token(Word("\\property"), (1, 1), (1, 9))
      |];
    });

    it("tokenises at-words correctly", () => {
      expect(tokenise("@media")) == [|
        Token(AtWord("@media"), (1, 1), (1, 6))
      |];
    });
  });

  describe("Multiline Comments", () => {
    open Expect;
    open! Expect.Operators;

    it("ignores multiline comments", () => {
      expect(tokenise("/* comment */")) |> toEqual([||]);
    });

    it("keeps counting newlines in comments", () => {
      expect(tokenise("/* comment \n */;")) == [|
        Token(Semicolon, (2, 4), (2, 4))
      |];
    });
  });

  describe("Interpolations", () => {
    open Expect;
    open! Expect.Operators;

    let interpolationValue = create_interpolation(1);

    it("accepts interpolations in the normal lexer loop", () => {
      let lexerStream = lexer(Input.input([| ":", ";" |], [| interpolationValue |]));
      let tokens = LazyStream.toArray(lexerStream);

      expect(tokens) == [|
        Token(Colon, (1, 1), (1, 1)),
        Token(Interpolation(interpolationValue), (1, 2), (1, 2)),
        Token(Semicolon, (1, 3), (1, 3))
      |];
    });

    it("handled interleaved words and interpolations", () => {
      let interpolationValue = create_interpolation(1);
      let lexerStream = lexer(Input.input([| "hello", "", "world;" |], [| interpolationValue, interpolationValue |]));
      let tokens = LazyStream.toArray(lexerStream);

      expect(tokens) == [|
        Token(Word("hello"), (1, 1), (1, 5)),
        Token(Interpolation(interpolationValue), (1, 6), (1, 6)),
        Token(Interpolation(interpolationValue), (1, 7), (1, 7)),
        Token(Word("world"), (1, 8), (1, 12)),
        Token(Semicolon, (1, 13), (1, 13))
      |];
    });
  });

  describe("Strings", () => {
    open Expect;
    open! Expect.Operators;

    it("parses single and double quote strings", () => {
      expect(tokenise({| 'string 1' "string 2" |})) == [|
        Token(Quote(Single), (1, 2), (1, 2)),
        Token(Str("string 1"), (1, 3), (1, 10)),
        Token(Quote(Single), (1, 11), (1, 11)),
        Token(Quote(Double), (1, 13), (1, 13)),
        Token(Str("string 2"), (1, 14), (1, 21)),
        Token(Quote(Double), (1, 22), (1, 22))
      |];
    });

    it("emits interpolations interleaved with strings", () => {
      let interpolationValue = create_interpolation(1);
      let lexerStream = lexer(Input.input([|"'start ", " end'"|], [| interpolationValue |]));
      let tokens = LazyStream.toArray(lexerStream);

      expect(tokens) == [|
        Token(Quote(Single), (1, 1), (1, 1)),
        Token(Str("start "), (1, 2), (1, 7)),
        Token(Interpolation(interpolationValue), (1, 8), (1, 8)),
        Token(Str(" end"), (1, 9), (1, 12)),
        Token(Quote(Single), (1, 13), (1, 13))
      |];
    });

    it("throws when no end of string is reached", () => {
      expect(() => tokenise("'")) |> toThrowMessage("unexpected eof while parsing string");
    });

    it("throws when unescaped newline in string are found", () => {
      expect(() => tokenise("'\n")) |> toThrowMessage("unexpected newline while parsing string");
    });

    it("accepts escaped content inside strings", () => {
      expect(tokenise("'newline:\\n'")) == [|
        Token(Quote(Single), (1, 1), (1, 1)),
        Token(Str("newline:\\n"), (1, 2), (1, 11)),
        Token(Quote(Single), (1, 12), (1, 12))
      |];
    });
  });

  describe("Unquoted string arguments", () => {
    open Expect;
    open! Expect.Operators;

    it("handles url() argument", () => {
      expect(tokenise("url(  https://github.com/?v=\"  )")) == [|
        Token(Word("url"), (1, 1), (1, 3)),
        Token(Paren(Opening), (1, 4), (1, 4)),
        Token(Quote(Double), (1, 7), (1, 7)),
        Token(Str("https://github.com/?v=\\\""), (1, 7), (1, 29)),
        Token(Quote(Double), (1, 32), (1, 32)),
        Token(Paren(Closing), (1, 32), (1, 32))
      |];
    });

    it("handles empty url() argument", () => {
      expect(tokenise("url(  )")) == [|
        Token(Word("url"), (1, 1), (1, 3)),
        Token(Paren(Opening), (1, 4), (1, 4)),
        Token(Quote(Double), (1, 7), (1, 7)),
        Token(Quote(Double), (1, 7), (1, 7)),
        Token(Paren(Closing), (1, 7), (1, 7))
      |];
    });

    it("throws when unescaped parentheses in url() argument are found", () => {
      expect(() => tokenise("url(())")) |> toThrowMessage("unexpected '(' while parsing unquoted argument");
    });

    it("throws when whitespaces in url() argument are found", () => {
      expect(() => tokenise("url(https://  github.com)")) |> toThrowMessage("nexpected whitespace while parsing unquoted argument; expected: ')'");
    });

    it("emits interpolations interleaved with url() argument", () => {
      let username = create_interpolation(1);
      let lexerStream = lexer(Input.input([|"url(https://twitter.com/", "/)"|], [| username |]));
      let tokens = LazyStream.toArray(lexerStream);

      expect(tokens) == [|
        Token(Word("url"), (1, 1), (1, 3)),
        Token(Paren(Opening), (1, 4), (1, 4)),
        Token(Quote(Double), (1, 5), (1, 5)),
        Token(Str("https://twitter.com/"), (1, 5), (1, 24)),
        Token(Interpolation(username), (1, 25), (1, 25)),
        Token(Str("/"), (1, 26), (1, 26)),
        Token(Quote(Double), (1, 27), (1, 27)),
        Token(Paren(Closing), (1, 27), (1, 27))
      |];
    });

    it("emits interpolations contained after the url() argument", () => {
      let username = create_interpolation(1);
      let lexerStream = lexer(Input.input([|"url(https://twitter.com/", ")"|], [| username |]));
      let tokens = LazyStream.toArray(lexerStream);

      expect(tokens) == [|
        Token(Word("url"), (1, 1), (1, 3)),
        Token(Paren(Opening), (1, 4), (1, 4)),
        Token(Quote(Double), (1, 5), (1, 5)),
        Token(Str("https://twitter.com/"), (1, 5), (1, 24)),
        Token(Interpolation(username), (1, 25), (1, 25)),
        Token(Quote(Double), (1, 26), (1, 26)),
        Token(Paren(Closing), (1, 26), (1, 26))
      |];
    });

    it("handles calc() argument", () => {
      expect(tokenise("calc( (50% - 10px) * 2 )")) == [|
        Token(Word("calc"), (1, 1), (1, 4)),
        Token(Paren(Opening), (1, 5), (1, 5)),
        Token(Str(" (50% - 10px) * 2 "), (1, 5), (1, 23)),
        Token(Paren(Closing), (1, 24), (1, 24))
      |];
    });

    it("throws when no end of argument is reached", () => {
      expect(() => tokenise("url(https://")) |> toThrowMessage("unexpected eof while parsing unquoted argument; expected: ')'");
    });
  });

  describe("Quoted string arguments", () => {
    open Expect;
    open! Expect.Operators;

    it("parses strings inside arguments other than url() and calc() correctly", () => {
      expect(tokenise("partyparrot( 'https://github.com' )")) == [|
        Token(Word("partyparrot"), (1, 1), (1, 11)),
        Token(Paren(Opening), (1, 12), (1, 12)),
        Token(Quote(Single), (1, 14), (1, 14)),
        Token(Str("https://github.com"), (1, 15), (1, 32)),
        Token(Quote(Single), (1, 33), (1, 33)),
        Token(Paren(Closing), (1, 35), (1, 35))
      |];
    });
  });
});
