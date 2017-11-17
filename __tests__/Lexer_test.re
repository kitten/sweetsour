open Jest;
open Lexer;

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

    test("tokenises single chars correctly", () => {
      expect(tokenise("{}[]()!=:;+&>*~,|$") == [|
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 1),
        Token(Bracket(Opening), 1),
        Token(Bracket(Closing), 1),
        Token(Paren(Opening), 1),
        Token(Paren(Closing), 1),
        Token(Exclamation, 1),
        Token(Equal, 1),
        Token(Colon, 1),
        Token(Semicolon, 1),
        Token(Plus, 1),
        Token(Ampersand, 1),
        Token(Arrow, 1),
        Token(Asterisk, 1),
        Token(Tilde, 1),
        Token(Comma, 1),
        Token(Pipe, 1),
        Token(Dollar, 1)
      |]) |> toBe(true);
    });

    test("skips over whitespace-chars counting newlines", () => {
      expect(tokenise("\t\r{   }\n{   }") == [|
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 1),
        Token(Brace(Opening), 2),
        Token(Brace(Closing), 2)
      |]) |> toBe(true);
    });

    test("throws when unexpected tokens are encountered", () => {
      expect(() => tokenise("^")) |> toThrowMessage("Unexpected token encountered: ^");
    });
  });

  describe("Words & At-Words", () => {
    open Expect;

    test("tokenises words correctly", () => {
      expect(tokenise("property #id .class_name 10% 10px hello-world") == [|
        Token(Word("property"), 1),
        Token(Word("#id"), 1),
        Token(Word(".class_name"), 1),
        Token(Word("10%"), 1),
        Token(Word("10px"), 1),
        Token(Word("hello-world"), 1)
      |]) |> toBe(true);
    });

    test("tokenises words containing escaped characters correctly", () => {
      expect(tokenise("pro\\p\\erty\\0024") == [|
        Token(Word("pro\\p\\erty\\0024"), 1)
      |]) |> toBe(true);
    });

    test("tokenises words starting with escaped characters correctly", () => {
      expect(tokenise("\\property") == [|
        Token(Word("\\property"), 1)
      |]) |> toBe(true);
    });

    test("tokenises at-words correctly", () => {
      expect(tokenise("@media") == [|
        Token(AtWord("@media"), 1)
      |]) |> toEqual(true);
    });
  });

  describe("Multiline Comments", () => {
    open Expect;

    test("ignores multiline comments", () => {
      expect(tokenise("/* comment */")) |> toEqual([||]);
    });

    test("keeps counting newlines in comments", () => {
      expect(tokenise("/* comment \n */;") == [|
        Token(Semicolon, 2)
      |]) |> toBe(true);
    });
  });

  describe("Interpolations", () => {
    open Expect;
    let interpolationValue = create_interpolation(1);

    test("accepts interpolations in the normal lexer loop", () => {
      let lexerStream = lexer(Input.input([| ":", ";" |], [| interpolationValue |]));
      let tokens = LazyStream.toArray(lexerStream);

      expect(tokens == [|
        Token(Colon, 1),
        Token(Interpolation(interpolationValue), 1),
        Token(Semicolon, 1)
      |]) |> toBe(true);
    });

    test("combinates words and interpolations using special tokens", () => {
      let interpolationValue = create_interpolation(1);
      let lexerStream = lexer(Input.input([| "hello", "", "world;" |], [| interpolationValue, interpolationValue |]));
      let tokens = LazyStream.toArray(lexerStream);

      expect(tokens == [|
        Token(Word("hello"), 1),
        Token(WordCombinator, 1),
        Token(Interpolation(interpolationValue), 1),
        Token(WordCombinator, 1),
        Token(Interpolation(interpolationValue), 1),
        Token(WordCombinator, 1),
        Token(Word("world"), 1),
        Token(Semicolon, 1)
      |]) |> toBe(true);
    });
  });

  describe("Strings", () => {
    open Expect;

    test("parses single and double quote strings", () => {
      expect(tokenise({| 'string 1' "string 2" |}) == [|
        Token(Quote(Single), 1),
        Token(Str("string 1"), 1),
        Token(Quote(Single), 1),
        Token(Quote(Double), 1),
        Token(Str("string 2"), 1),
        Token(Quote(Double), 1)
      |]) |> toBe(true);
    });

    test("emits interpolations interleaved with strings", () => {
      let interpolationValue = create_interpolation(1);
      let lexerStream = lexer(Input.input([|"'start ", " end'"|], [| interpolationValue |]));
      let tokens = LazyStream.toArray(lexerStream);

      expect(tokens == [|
        Token(Quote(Single), 1),
        Token(Str("start "), 1),
        Token(Interpolation(interpolationValue), 1),
        Token(Str(" end"), 1),
        Token(Quote(Single), 1)
      |]) |> toBe(true);
    });

    test("throws when no end of string is reached", () => {
      expect(() => tokenise("'")) |> toThrowMessage("Unexpected EOF before end of string");
    });

    test("throws when unescaped newline in string are found", () => {
      expect(() => tokenise("'\n")) |> toThrowMessage("Expected newline inside string to be escaped");
    });

    test("accepts escaped content inside strings", () => {
      expect(tokenise("'newline:\\n'") == [|
        Token(Quote(Single), 1),
        Token(Str("newline:\\n"), 1),
        Token(Quote(Single), 1)
      |]) |> toBe(true);
    });
  });

  describe("Unquoted string arguments", () => {
    open Expect;

    test("handles url() argument", () => {
      expect(tokenise("url(  https://github.com  )") == [|
        Token(Word("url"), 1),
        Token(Paren(Opening), 1),
        Token(Str("https://github.com"), 1),
        Token(Paren(Closing), 1)
      |]) |> toBe(true);
    });

    test("throws when unescaped parentheses in url() argument are found", () => {
      expect(() => tokenise("url(())")) |> toThrowMessage("Unexpected opening parenthesis inside an unquoted argument");
    });

    test("throws when whitespaces in url() argument are found", () => {
      expect(() => tokenise("url(https://  github.com)")) |> toThrowMessage("Unexpected whitespace, expected closing parenthesis");
    });

    test("emits interpolations interleaved with url() argument", () => {
      let username = create_interpolation(1);
      let lexerStream = lexer(Input.input([|"url(https://twitter.com/", "/)"|], [| username |]));
      let tokens = LazyStream.toArray(lexerStream);

      expect(tokens == [|
        Token(Word("url"), 1),
        Token(Paren(Opening), 1),
        Token(Str("https://twitter.com/"), 1),
        Token(Interpolation(username), 1),
        Token(Str("/"), 1),
        Token(Paren(Closing), 1)
      |]) |> toBe(true);
    });

    test("handles calc() argument", () => {
      expect(tokenise("calc( (50% - 10px) * 2 )") == [|
        Token(Word("calc"), 1),
        Token(Paren(Opening), 1),
        Token(Str("(50% - 10px) * 2 "), 1),
        Token(Paren(Closing), 1)
      |]) |> toBe(true);
    });

    test("throws when no end of argument is reached", () => {
      expect(() => tokenise("url(https://")) |> toThrowMessage("Unexpected EOF before end of unquoted argument");
    });
  });

  describe("Quoted string arguments", () => {
    open Expect;

    test("parses strings inside arguments other than url() and calc() correctly", () => {
      expect(tokenise("partyparrot( 'https://github.com' )") == [|
        Token(Word("partyparrot"), 1),
        Token(Paren(Opening), 1),
        Token(Quote(Single), 1),
        Token(Str("https://github.com"), 1),
        Token(Quote(Single), 1),
        Token(Paren(Closing), 1)
      |]) |> toBe(true);
    });
  });
});
