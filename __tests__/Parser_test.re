open Jest;
open Parser;

let it = test;

let create_interpolation : int => Common.interpolation = [%bs.raw{|
  function(x) { return x; }
|}];

let parse = (tokens: array(Lexer.token)): array(node) => {
  let i = ref(0);
  let tokenStream = LazyStream.from([@bs] () => {
    if (i^ < Array.length(tokens)) {
      let token = Some(tokens[i^]);
      i := i^ + 1;
      token
    } else {
      None
    }
  });

  LazyStream.toArray(parser(tokenStream))
};

describe("Parser", () => {
  describe("Selectors", () => {
    open Expect;
    open! Expect.Operators;

    /* Parse: `.test {}` */
    it("parses plain words as selectors", () => {
      expect(parse([|
        Token(Word(".test"), (1, 1), (1, 5)),
        Token(Brace(Opening), (1, 7), (1, 7)),
        Token(Brace(Closing), (1, 8), (1, 8))
      |])) == [|
        RuleStart(StyleRule),
        Selector(".test"),
        RuleEnd
      |];
    });

    /* Parse: `.first${x} {}` */
    it("parses interpolation & words as compound selectors", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word(".first"), (1, 1), (1, 6)),
        Token(Interpolation(inter), (1, 7), (1, 7)),
        Token(Brace(Opening), (1, 9), (1, 9)),
        Token(Brace(Closing), (1, 10), (1, 10))
      |])) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".first"),
        SelectorRef(inter),
        CompoundSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `.first .second${x} {}` */
    it("parses space combinators for selectors", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word(".first"), (1, 1), (1, 6)),
        Token(Word(".second"), (1, 8), (1, 14)),
        Token(Interpolation(inter), (1, 15), (1, 15)),
        Token(Brace(Opening), (1, 17), (1, 17)),
        Token(Brace(Closing), (1, 18), (1, 18))
      |])) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".first"),
        SpaceCombinator,
        Selector(".second"),
        SelectorRef(inter),
        CompoundSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `.first {.second {}}` */
    it("parses nested rule selectors", () => {
      expect(parse([|
        Token(Word(".first"), (1, 1), (1, 6)),
        Token(Brace(Opening), (1, 8), (1, 8)),
        Token(Word(".second"), (1, 9), (1, 15)),
        Token(Brace(Opening), (1, 17), (1, 17)),
        Token(Brace(Closing), (1, 18), (1, 18)),
        Token(Brace(Closing), (1, 19), (1, 19))
      |])) == [|
        RuleStart(StyleRule),
        Selector(".first"),
        RuleStart(StyleRule),
        Selector(".second"),
        RuleEnd,
        RuleEnd
      |];
    });

    /* Parse: `.test:hover{}` */
    it("parses pseudo selectors", () => {
      expect(parse([|
        Token(Word(".test"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Word("hover"), (1, 7), (1, 11)),
        Token(Brace(Opening), (1, 12), (1, 12)),
        Token(Brace(Closing), (1, 13), (1, 13))
      |])) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".test"),
        Selector(":hover"),
        CompoundSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `.test::before{}` */
    it("parses explicit pseudo-element selectors", () => {
      expect(parse([|
        Token(Word(".test"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Colon, (1, 7), (1, 7)),
        Token(Word("before"), (1, 8), (1, 13)),
        Token(Brace(Opening), (1, 14), (1, 14)),
        Token(Brace(Closing), (1, 15), (1, 15))
      |])) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".test"),
        Selector(":before"),
        CompoundSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `.test :hover{}` */
    it("parses universal pseudo selector (shorthand)", () => {
      expect(parse([|
        Token(Word(".test"), (1, 1), (1, 5)),
        Token(Colon, (1, 7), (1, 7)),
        Token(Word("hover"), (1, 8), (1, 12)),
        Token(Brace(Opening), (1, 13), (1, 13)),
        Token(Brace(Closing), (1, 14), (1, 14))
      |])) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".test"),
        SpaceCombinator,
        Selector(":hover"),
        CompoundSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `.test :hover{}` */
    it("parses universal and parent selectors", () => {
      expect(parse([|
        Token(Ampersand, (1, 1), (1, 1)),
        Token(Word(".abc"), (1, 3), (1, 6)),
        Token(Asterisk, (1, 8), (1, 8)),
        Token(Colon, (1, 9), (1, 9)),
        Token(Word("hover"), (1, 10), (1, 14)),
        Token(Brace(Opening), (1, 15), (1, 15)),
        Token(Brace(Closing), (1, 15), (1, 15))
      |])) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        ParentSelector,
        SpaceCombinator,
        Selector(".abc"),
        SpaceCombinator,
        UniversalSelector,
        Selector(":hover"),
        CompoundSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `.test:${x} div {}` */
    it("parses pseudo selectors containing interpolations", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word(".test"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Interpolation(inter), (1, 7), (1, 7)),
        Token(Word("div"), (1, 9), (1, 11)),
        Token(Brace(Opening), (1, 13), (1, 13)),
        Token(Brace(Closing), (1, 14), (1, 14))
      |])) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".test"),
        Selector(":"),
        SelectorRef(inter),
        SpaceCombinator,
        Selector("div"),
        CompoundSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `.first, .second {}` */
    it("parses comma separated selectors", () => {
      expect(parse([|
        Token(Word(".first"), (1, 1), (1, 6)),
        Token(Comma, (1, 7), (1, 7)),
        Token(Word(".second"), (1, 9), (1, 15)),
        Token(Brace(Opening), (1, 17), (1, 17)),
        Token(Brace(Closing), (1, 18), (1, 18))
      |])) == [|
        RuleStart(StyleRule),
        Selector(".first"),
        Selector(".second"),
        RuleEnd
      |]
    });

    /* Parse: `.test:not (.first, .second) {}`
       NOTE: The whitespace in front of the parenthesis should not be significant */
    it("parses pseudo selector functions", () => {
      expect(parse([|
        Token(Word(".test"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Word("not"), (1, 7), (1, 9)),
        Token(Paren(Opening), (1, 11), (1, 11)),
        Token(Word(".first"), (1, 12), (1, 17)),
        Token(Comma, (1, 18), (1, 18)),
        Token(Word(".second"), (1, 20), (1, 26)),
        Token(Paren(Closing), (1, 27), (1, 27)),
        Token(Brace(Opening), (1, 29), (1, 29)),
        Token(Brace(Closing), (1, 30), (1, 30))
      |])) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".test"),
        FunctionStart(":not"),
        Selector(".first"),
        Selector(".second"),
        FunctionEnd,
        CompoundSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `:not(.test:not(div)) {}` */
    it("parses nested pseudo selector functions", () => {
      expect(parse([|
        Token(Colon, (1, 1), (1, 1)),
        Token(Word("not"), (1, 2), (1, 4)),
        Token(Paren(Opening), (1, 5), (1, 5)),
        Token(Word(".test"), (1, 6), (1, 10)),
        Token(Colon, (1, 11), (1, 11)),
        Token(Word("not"), (1, 12), (1, 14)),
        Token(Paren(Opening), (1, 15), (1, 15)),
        Token(Word("div"), (1, 16), (1, 18)),
        Token(Paren(Closing), (1, 19), (1, 19)),
        Token(Paren(Closing), (1, 20), (1, 20)),
        Token(Brace(Opening), (1, 22), (1, 22)),
        Token(Brace(Closing), (1, 23), (1, 23))
      |])) == [|
        RuleStart(StyleRule),
        FunctionStart(":not"),
        CompoundSelectorStart,
        Selector(".test"),
        FunctionStart(":not"),
        Selector("div"),
        FunctionEnd,
        CompoundSelectorEnd,
        FunctionEnd,
        RuleEnd
      |];
    });

    it("throws when an unexpected token is reached while parsing pseudo selectors", () => {
      expect(() => parse([|
        Token(Colon, (1, 1), (1, 1)),
        Token(Brace(Opening), (1, 2), (1, 2)),
        Token(Brace(Closing), (1, 3), (1, 3))
      |])) |> toThrowMessage("unexpected token while parsing pseudo selector");
    });

    /* Parse: `& > div {}` */
    it("parses child combinator", () => {
      expect(parse([|
        Token(Ampersand, (1, 1), (1, 1)),
        Token(Arrow, (1, 3), (1, 3)),
        Token(Word("div"), (1, 5), (1, 7)),
        Token(Brace(Opening), (1, 9), (1, 9)),
        Token(Brace(Closing), (1, 10), (1, 10))
      |])) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        ParentSelector,
        ChildCombinator,
        Selector("div"),
        CompoundSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `& >> div {}` */
    it("parses doubled child combinator", () => {
      expect(parse([|
        Token(Ampersand, (1, 1), (1, 1)),
        Token(Arrow, (1, 3), (1, 3)),
        Token(Arrow, (1, 4), (1, 4)),
        Token(Word("div"), (1, 6), (1, 8)),
        Token(Brace(Opening), (1, 10), (1, 10)),
        Token(Brace(Closing), (1, 11), (1, 11))
      |])) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        ParentSelector,
        DoubledChildCombinator,
        Selector("div"),
        CompoundSelectorEnd,
        RuleEnd
      |];
    });

    it("throws when two subsequent combinators are encountered", () => {
      expect(() => parse([|
        Token(Ampersand, (1, 1), (1, 1)),
        Token(Arrow, (1, 2), (1, 2)),
        Token(Tilde, (1, 3), (1, 3)),
        Token(Word("div"), (1, 4), (1, 6)),
        Token(Brace(Opening), (1, 7), (1, 7)),
        Token(Brace(Closing), (1, 8), (1, 8))
      |])) |> toThrowMessage("Unexpected token while parsing selectors");
    });

    it("throws when no selector is following a combinator", () => {
      expect(() => parse([|
        Token(Ampersand, (1, 1), (1, 1)),
        Token(Arrow, (1, 2), (1, 2)),
        Token(Brace(Opening), (1, 3), (1, 3)),
        Token(Brace(Closing), (1, 4), (1, 4))
      |])) |> toThrowMessage("unexpected combinator while parsing selectors");
    });
  });

  describe("Attribute Selectors", () => {
    open Expect;
    open! Expect.Operators;

    /*let inter = create_interpolation(1);*/

    /* Parse: `.test[attr] {}` */
    it("parses name-only attribute selectors", () => {
      expect(parse([|
        Token(Word(".test"), (1, 1), (1, 5)),
        Token(Bracket(Opening), (1, 6), (1, 6)),
        Token(Word("attr"), (1, 7), (1, 10)),
        Token(Bracket(Closing), (1, 8), (1, 8)),
        Token(Brace(Opening), (1, 10), (1, 10)),
        Token(Brace(Closing), (1, 11), (1, 11))
      |])) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".test"),
        AttributeSelectorStart(CaseSensitive),
        AttributeName("attr"),
        AttributeSelectorEnd,
        CompoundSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `.test [abc] {}` */
    it("parses universal attribute selectors (shorthand)", () => {
      expect(parse([|
        Token(Word(".test"), (1, 1), (1, 5)),
        Token(Bracket(Opening), (1, 7), (1, 7)),
        Token(Word("abc"), (1, 8), (1, 10)),
        Token(Bracket(Closing), (1, 8), (1, 8)),
        Token(Brace(Opening), (1, 10), (1, 10)),
        Token(Brace(Closing), (1, 11), (1, 11))
      |])) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".test"),
        SpaceCombinator,
        AttributeSelectorStart(CaseSensitive),
        AttributeName("abc"),
        AttributeSelectorEnd,
        CompoundSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `[attr="test"] {}` */
    it("parses comparative attribute selectors (=)", () => {
      expect(parse([|
        Token(Bracket(Opening), (1, 1), (1, 1)),
        Token(Word("attr"), (1, 2), (1, 5)),
        Token(Equal, (1, 6), (1, 6)),
        Token(Quote(Double), (1, 7), (1, 7)),
        Token(Str("test"), (1, 8), (1, 11)),
        Token(Quote(Double), (1, 12), (1, 12)),
        Token(Bracket(Closing), (1, 13), (1, 13)),
        Token(Brace(Opening), (1, 15), (1, 15)),
        Token(Brace(Closing), (1, 16), (1, 16))
      |])) == [|
        RuleStart(StyleRule),
        AttributeSelectorStart(CaseSensitive),
        AttributeName("attr"),
        AttributeOperator("="),
        Value("\"test\""),
        AttributeSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `[attr^="test"]{}` */
    it("parses comparative attribute selectors (^=)", () => {
      expect(parse([|
        Token(Bracket(Opening), (1, 1), (1, 1)),
        Token(Word("attr"), (1, 2), (1, 5)),
        Token(Caret, (1, 6), (1, 6)),
        Token(Equal, (1, 7), (1, 7)),
        Token(Quote(Double), (1, 8), (1, 8)),
        Token(Str("test"), (1, 9), (1, 12)),
        Token(Quote(Double), (1, 13), (1, 13)),
        Token(Bracket(Closing), (1, 14), (1, 14)),
        Token(Brace(Opening), (1, 15), (1, 15)),
        Token(Brace(Closing), (1, 16), (1, 16))
      |])) == [|
        RuleStart(StyleRule),
        AttributeSelectorStart(CaseSensitive),
        AttributeName("attr"),
        AttributeOperator("^="),
        Value("\"test\""),
        AttributeSelectorEnd,
        RuleEnd
      |];
    });

    /* Parse: `[attr^="test" i]{}` */
    it("parses caseinsensitive attribute selectors", () => {
      expect(parse([|
        Token(Bracket(Opening), (1, 1), (1, 1)),
        Token(Word("attr"), (1, 2), (1, 5)),
        Token(Caret, (1, 6), (1, 6)),
        Token(Equal, (1, 7), (1, 7)),
        Token(Quote(Double), (1, 8), (1, 8)),
        Token(Str("test"), (1, 9), (1, 12)),
        Token(Quote(Double), (1, 13), (1, 13)),
        Token(Word("i"), (1, 15), (1, 15)),
        Token(Bracket(Closing), (1, 16), (1, 16)),
        Token(Brace(Opening), (1, 17), (1, 17)),
        Token(Brace(Closing), (1, 18), (1, 18))
      |])) == [|
        RuleStart(StyleRule),
        AttributeSelectorStart(CaseInsensitive),
        AttributeName("attr"),
        AttributeOperator("^="),
        Value("\"test\""),
        AttributeSelectorEnd,
        RuleEnd
      |];
    });
  });

  describe("Declarations", () => {
    open Expect;
    open! Expect.Operators;

    /* Parse: `color: papayawhip;` */
    it("parses declarations containing only plain words", () => {
      expect(parse([|
        Token(Word("color"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Word("papayawhip"), (1, 8), (1, 18)),
        Token(Semicolon, (1, 19), (1, 19))
      |])) == [|
        Property("color"),
        Value("papayawhip"),
      |];
    });

    /* Parse: `;color: papayawhip;;` */
    it("ignores free semicolons", () => {
      expect(parse([|
        Token(Semicolon, (1, 1), (1, 1)),
        Token(Word("color"), (1, 2), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Word("papayawhip"), (1, 7), (1, 16)),
        Token(Semicolon, (1, 17), (1, 17)),
        Token(Semicolon, (1, 18), (1, 18))
      |])) == [|
        Property("color"),
        Value("papayawhip"),
      |];
    });

    /* Parse: `${x}: papayawhip;` */
    it("parses declarations having an interpolation as a property", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Interpolation(inter), (1, 1), (1, 1)),
        Token(Colon, (1, 2), (1, 2)),
        Token(Word("papayawhip"), (1, 4), (1, 13)),
        Token(Semicolon, (1, 14), (1, 14))
      |])) == [|
        PropertyRef(inter),
        Value("papayawhip")
      |];
    });

    /* Parse: `color: ${x};` */
    it("parses declarations having an interpolation as a value", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word("color"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Interpolation(inter), (1, 8), (1, 8)),
        Token(Semicolon, (1, 9), (1, 9))
      |])) == [|
        Property("color"),
        ValueRef(inter)
      |];
    });

    /* Parse: `color: papayawhip, palevioletred;` */
    it("parses comma separated values", () => {
      expect(parse([|
        Token(Word("color"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Word("papayawhip"), (1, 8), (1, 10)),
        Token(Comma, (1, 11), (1, 11)),
        Token(Word("palevioletred"), (1, 13), (1, 25)),
        Token(Semicolon, (1, 26), (1, 26))
      |])) == [|
        Property("color"),
        Value("papayawhip"),
        Value("palevioletred")
      |];
    });

    /* Parse: `content: "hello", 'world';` */
    it("parses strings as values", () => {
      expect(parse([|
        Token(Word("color"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Quote(Double), (1, 8), (1, 8)),
        Token(Str("hello"), (1, 9), (1, 14)),
        Token(Quote(Double), (1, 15), (1, 15)),
        Token(Comma, (1, 16), (1, 16)),
        Token(Quote(Single), (1, 18), (1, 19)),
        Token(Str("world"), (1, 20), (1, 24)),
        Token(Quote(Single), (1, 25), (1, 25)),
        Token(Semicolon, (1, 26), (1, 26))
      |])) == [|
        Property("color"),
        Value("\"hello\""),
        Value("'world'")
      |];
    });

    it("throws when unexpected tokens are encountered while parsing strings", () => {
      expect(() => parse([|
        Token(Word("color"), (1, 5), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Quote(Double), (1, 8), (1, 8)),
        Token(Str("hello"), (1, 9), (1, 13)),
        Token(Quote(Single), (1, 14), (1, 14)),
        Token(Semicolon, (1, 15), (1, 15))
      |])) |> toThrowMessage("unexpected token while parsing string");
    });

    /* Parse: `content: "hello ${x} world";` */
    it("parses strings interleaved with values", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word("color"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Quote(Double), (1, 8), (1, 8)),
        Token(Str("hello "), (1, 9), (1, 14)),
        Token(Interpolation(inter), (1, 15), (1, 15)),
        Token(Str(" world"), (1, 16), (1, 21)),
        Token(Quote(Double), (1, 22), (1, 22)),
        Token(Semicolon, (1, 23), (1, 23))
      |])) == [|
        Property("color"),
        StringStart("\""),
        Value("hello "),
        ValueRef(inter),
        Value(" world"),
        StringEnd
      |];
    });

    /* Parse: `width: calc(2 * (50% - 20px));` */
    it("parses unquoted calc() argument", () => {
      expect(parse([|
        Token(Word("width"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Word("calc"), (1, 8), (1, 11)),
        Token(Paren(Opening), (1, 12), (1, 12)),
        Token(Str("2 * (50% - 20px)"), (1, 13), (1, 28)),
        Token(Paren(Closing), (1, 29), (1, 29)),
        Token(Semicolon, (1, 30), (1, 30))
      |])) == [|
        Property("width"),
        FunctionStart("calc"),
        Value("2 * (50% - 20px)"),
        FunctionEnd
      |];
    });

    /* Parse: `width: calc(2 * (50% - ${x}));` */
    it("parses unquoted calc() argument containing interpolations", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word("width"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Word("calc"), (1, 7), (1, 10)),
        Token(Paren(Opening), (1, 11), (1, 11)),
        Token(Str("2 * (50% - "), (1, 12), (1, 22)),
        Token(Interpolation(inter), (1, 23), (1, 23)),
        Token(Str(")"), (1, 24), (1, 24)),
        Token(Paren(Closing), (1, 25), (1, 25)),
        Token(Semicolon, (1, 26), (1, 26))
      |])) == [|
        Property("width"),
        FunctionStart("calc"),
        CompoundValueStart,
        Value("2 * (50% - "),
        ValueRef(inter),
        Value(")"),
        CompoundValueEnd,
        FunctionEnd
      |];
    });

    /* Parse: `background-image: url(http://test.com);` */
    it("parses unquoted url() argument", () => {
      expect(parse([|
        Token(Word("background-image"), (1, 1), (1, 16)),
        Token(Colon, (1, 17), (1, 17)),
        Token(Word("url"), (1, 19), (1, 21)),
        Token(Paren(Opening), (1, 22), (1, 22)),
        Token(Quote(Double), (1, 23), (1, 23)),
        Token(Str("http://test.com"), (1, 23), (1, 37)),
        Token(Quote(Double), (1, 37), (1, 37)),
        Token(Paren(Closing), (1, 38), (1, 38)),
        Token(Semicolon, (1, 39), (1, 39))
      |])) == [|
        Property("background-image"),
        FunctionStart("url"),
        Value("\"http://test.com\""),
        FunctionEnd
      |];
    });

    /* Parse: `background-image: url(http://test.com/${x});` */
    it("parses unquoted url() argument containing interpolations", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word("background-image"), (1, 1), (1, 16)),
        Token(Colon, (1, 17), (1, 17)),
        Token(Word("url"), (1, 19), (1, 21)),
        Token(Paren(Opening), (1, 22), (1, 22)),
        Token(Quote(Double), (1, 23), (1, 23)),
        Token(Str("http://test.com/"), (1, 23), (1, 37)),
        Token(Interpolation(inter), (1, 38), (1, 38)),
        Token(Quote(Double), (1, 38), (1, 38)),
        Token(Paren(Closing), (1, 39), (1, 39)),
        Token(Semicolon, (1, 40), (1, 40))
      |])) == [|
        Property("background-image"),
        FunctionStart("url"),
        StringStart("\""),
        Value("http://test.com/"),
        ValueRef(inter),
        StringEnd,
        FunctionEnd
      |];
    });

    /* Parse: `background-image: url(${x});` */
    it("parses unquoted url() argument containing interpolations", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word("background-image"), (1, 1), (1, 16)),
        Token(Colon, (1, 17), (1, 17)),
        Token(Word("url"), (1, 19), (1, 21)),
        Token(Paren(Opening), (1, 22), (1, 22)),
        Token(Quote(Double), (1, 23), (1, 23)),
        Token(Interpolation(inter), (1, 23), (1, 23)),
        Token(Quote(Double), (1, 23), (1, 23)),
        Token(Paren(Closing), (1, 24), (1, 24)),
        Token(Semicolon, (1, 25), (1, 25))
      |])) == [|
        Property("background-image"),
        FunctionStart("url"),
        StringStart("\""),
        ValueRef(inter),
        StringEnd,
        FunctionEnd
      |];
    });

    /* Parse: `padding: 10px 20px;` */
    it("parses compound values", () => {
      expect(parse([|
        Token(Word("padding"), (1, 1), (1, 7)),
        Token(Colon, (1, 8), (1, 8)),
        Token(Word("10px"), (1, 10), (1, 13)),
        Token(Word("20px"), (1, 15), (1, 17)),
        Token(Semicolon, (1, 18), (1, 18))
      |])) == [|
        Property("padding"),
        CompoundValueStart,
        Value("10px"),
        Value("20px"),
        CompoundValueEnd
      |];
    });

    it("throws when unexpected tokens are encountered", () => {
      expect(() => parse([|
        Token(Word("color"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Word("rgba"), (1, 8), (1, 11)),
        Token(Paren(Opening), (1, 12), (1, 12)),
        Token(Paren(Opening), (1, 13), (1, 13)),
        Token(Semicolon, (1, 14), (1, 14))
      |])) |> toThrowMessage("unexpected token while parsing values");
    });
  });
});
