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
      None } });

  LazyStream.toArray(parser(tokenStream))
};

describe("Parser", () => {
  describe("Selectors", () => {
    open Expect;

    /* Parse: `.test {}` */
    it("parses plain words as selectors", () => {
      expect(parse([|
        Token(Word(".test"), 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |]) == [|
        RuleStart(StyleRule),
        Selector(".test"),
        RuleEnd
      |]) |> toBe(true)
    });

    /* Parse: `.first${x} {}` */
    it("parses interpolation & words as compound selectors", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word(".first"), 1),
        Token(WordCombinator, 1),
        Token(Interpolation(inter), 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |]) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".first"),
        SelectorRef(inter),
        CompoundSelectorEnd,
        RuleEnd
      |]) |> toBe(true)
    });

    /* Parse: `.first .second${x} {}` */
    it("parses space combinators for selectors", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word(".first"), 1),
        Token(Word(".second"), 1),
        Token(WordCombinator, 1),
        Token(Interpolation(inter), 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |]) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".first"),
        SpaceCombinator,
        Selector(".second"),
        SelectorRef(inter),
        CompoundSelectorEnd,
        RuleEnd
      |]) |> toBe(true)
    });

    /* Parse: `.first {.second {}}` */
    it("parses nested rule selectors", () => {
      expect(parse([|
        Token(Word(".first"), 1),
        Token(Brace(Opening), 1),
        Token(Word(".second"), 2),
        Token(Brace(Opening), 2),
        Token(Brace(Closing), 3),
        Token(Brace(Closing), 4)
      |]) == [|
        RuleStart(StyleRule),
        Selector(".first"),
        RuleStart(StyleRule),
        Selector(".second"),
        RuleEnd,
        RuleEnd
      |]) |> toBe(true)
    });

    /* Parse: `.test:hover {}` */
    it("parses pseudo selectors", () => {
      expect(parse([|
        Token(Word(".test"), 1),
        Token(Colon, 1),
        Token(Word("hover"), 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |]) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".test"),
        Selector(":hover"),
        CompoundSelectorEnd,
        RuleEnd
      |]) |> toBe(true)
    });

    /* Parse: `.test:${x} div {}` */
    it("parses pseudo selectors containing interpolations", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word(".test"), 1),
        Token(Colon, 1),
        Token(Interpolation(inter), 1),
        Token(Word("div"), 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |]) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".test"),
        Selector(":"),
        SelectorRef(inter),
        SpaceCombinator,
        Selector("div"),
        CompoundSelectorEnd,
        RuleEnd
      |]) |> toBe(true)
    });

    /* Parse: `.first, .second {}` */
    it("parses comma separated selectors", () => {
      expect(parse([|
        Token(Word(".first"), 1),
        Token(Comma, 1),
        Token(Word(".second"), 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |]) == [|
        RuleStart(StyleRule),
        Selector(".first"),
        Selector(".second"),
        RuleEnd
      |]) |> toBe(true)
    });

    /* Parse: `.test:not (.first, .second) {}`
       NOTE: The whitespace in front of the parenthesis should not be significant */
    it("parses pseudo selector functions", () => {
      expect(parse([|
        Token(Word(".test"), 1),
        Token(Colon, 1),
        Token(Word("not"), 1),
        Token(Paren(OpeningSeparated), 1),
        Token(Word(".first"), 1),
        Token(Paren(Closing), 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |]) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        Selector(".test"),
        FunctionStart(":not"),
        Selector(".first"),
        FunctionEnd,
        CompoundSelectorEnd,
        RuleEnd
      |]) |> toBe(true);
    });

    /* Parse: `:not(.test:not(div)) {}` */
    it("parses nested pseudo selector functions", () => {
      expect(parse([|
        Token(Colon, 1),
        Token(Word("not"), 1),
        Token(Paren(Opening), 1),
        Token(Word(".test"), 1),
        Token(Colon, 1),
        Token(Word("not"), 1),
        Token(Paren(Opening), 1),
        Token(Word("div"), 1),
        Token(Paren(Closing), 1),
        Token(Paren(Closing), 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |]) == [|
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
      |]) |> toBe(true);
    });

    it("throws when an unexpected token is reached while parsing pseudo selectors", () => {
      expect(() => parse([|
        Token(Colon, 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |])) |> toThrowMessage("unexpected token while parsing pseudo selector");
    });

    /* Parse: `& > div {}` */
    it("parses child combinator", () => {
      expect(parse([|
        Token(Ampersand, 1),
        Token(Arrow, 1),
        Token(Word("div"), 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |]) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        ParentSelector,
        ChildCombinator,
        Selector("div"),
        CompoundSelectorEnd,
        RuleEnd
      |]) |> toBe(true);
    });

    /* Parse: `& >> div {}` */
    it("parses doubled child combinator", () => {
      expect(parse([|
        Token(Ampersand, 1),
        Token(Arrow, 1),
        Token(Arrow, 1),
        Token(Word("div"), 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |]) == [|
        RuleStart(StyleRule),
        CompoundSelectorStart,
        ParentSelector,
        DoubledChildCombinator,
        Selector("div"),
        CompoundSelectorEnd,
        RuleEnd
      |]) |> toBe(true);
    });

    it("throws when two combinators in a row are encountered", () => {
      expect(() => parse([|
        Token(Ampersand, 1),
        Token(Arrow, 1),
        Token(Tilde, 1),
        Token(Word("div"), 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |])) |> toThrowMessage("Unexpected token while parsing selectors");
    });

    it("throws when no selector is following a combinator", () => {
      expect(() => parse([|
        Token(Ampersand, 1),
        Token(Arrow, 1),
        Token(Brace(Opening), 1),
        Token(Brace(Closing), 2)
      |])) |> toThrowMessage("unexpected combinator while parsing selectors");
    });
  });

  describe("Declarations", () => {
    open Expect;

    /* Parse: `color: papayawhip;` */
    it("parses declarations containing only plain words", () => {
      expect(parse([|
        Token(Word("color"), 1),
        Token(Colon, 1),
        Token(Word("papayawhip"), 1),
        Token(Semicolon, 1)
      |]) == [|
        Property("color"),
        Value("papayawhip"),
      |]) |> toBe(true)
    });

    /* Parse: `;color: papayawhip;;` */
    it("ignores free semicolons", () => {
      expect(parse([|
        Token(Semicolon, 1),
        Token(Word("color"), 1),
        Token(Colon, 1),
        Token(Word("papayawhip"), 1),
        Token(Semicolon, 1),
        Token(Semicolon, 1)
      |]) == [|
        Property("color"),
        Value("papayawhip"),
      |]) |> toBe(true)
    });

    /* Parse: `${x}: papayawhip;` */
    it("parses declarations having an interpolation as a property", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Interpolation(inter), 1),
        Token(Colon, 1),
        Token(Word("papayawhip"), 1),
        Token(Semicolon, 1)
      |]) == [|
        PropertyRef(inter),
        Value("papayawhip")
      |]) |> toBe(true)
    });

    /* Parse: `color: ${x};` */
    it("parses declarations having an interpolation as a value", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word("color"), 1),
        Token(Colon, 1),
        Token(Interpolation(inter), 1),
        Token(Semicolon, 1)
      |]) == [|
        Property("color"),
        ValueRef(inter)
      |]) |> toBe(true)
    });

    /* Parse: `color: papayawhip, palevioletred;` */
    it("parses comma separated values", () => {
      expect(parse([|
        Token(Word("color"), 1),
        Token(Colon, 1),
        Token(Word("papayawhip"), 1),
        Token(Comma, 1),
        Token(Word("palevioletred"), 1),
        Token(Semicolon, 1)
      |]) == [|
        Property("color"),
        Value("papayawhip"),
        Value("palevioletred")
      |]) |> toBe(true)
    });

    /* Parse: `content: "hello", 'world';` */
    it("parses strings as values", () => {
      expect(parse([|
        Token(Word("color"), 1),
        Token(Colon, 1),
        Token(Quote(Double), 1),
        Token(Str("hello"), 1),
        Token(Quote(Double), 1),
        Token(Comma, 1),
        Token(Quote(Single), 1),
        Token(Str("world"), 1),
        Token(Quote(Single), 1),
        Token(Semicolon, 1)
      |]) == [|
        Property("color"),
        Value("\"hello\""),
        Value("'world'")
      |]) |> toBe(true)
    });

    it("throws when unexpected tokens are encountered while parsing strings", () => {
      expect(() => parse([|
        Token(Word("color"), 1),
        Token(Colon, 1),
        Token(Quote(Double), 1),
        Token(Str("hello"), 1),
        Token(Quote(Single), 1),
        Token(Semicolon, 1)
      |])) |> toThrowMessage("unexpected token while parsing string");
    });

    /* Parse: `content: "hello ${x} world";` */
    it("parses strings interleaved with values", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word("color"), 1),
        Token(Colon, 1),
        Token(Quote(Double), 1),
        Token(Str("hello "), 1),
        Token(Interpolation(inter), 1),
        Token(Str(" world"), 1),
        Token(Quote(Double), 1),
        Token(Semicolon, 1)
      |]) == [|
        Property("color"),
        StringStart("\""),
        Value("hello "),
        ValueRef(inter),
        Value(" world"),
        StringEnd
      |]) |> toBe(true)
    });

    /* Parse: `width: calc(2 * (50% - 20px));` */
    it("parses unquoted calc() argument", () => {
      expect(parse([|
        Token(Word("width"), 1),
        Token(Colon, 1),
        Token(Word("calc"), 1),
        Token(Paren(Opening), 1),
        Token(Str("2 * (50% - 20px)"), 1),
        Token(Paren(Closing), 1),
        Token(Semicolon, 1)
      |]) == [|
        Property("width"),
        FunctionStart("calc"),
        Value("2 * (50% - 20px)"),
        FunctionEnd
      |]) |> toBe(true)
    });

    /* Parse: `width: calc(2 * (50% - ${x}));` */
    it("parses unquoted calc() argument containing interpolations", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word("width"), 1),
        Token(Colon, 1),
        Token(Word("calc"), 1),
        Token(Paren(Opening), 1),
        Token(Str("2 * (50% - "), 1),
        Token(Interpolation(inter), 1),
        Token(Str(")"), 1),
        Token(Paren(Closing), 1),
        Token(Semicolon, 1)
      |]) == [|
        Property("width"),
        FunctionStart("calc"),
        CompoundValueStart,
        Value("2 * (50% - "),
        ValueRef(inter),
        Value(")"),
        CompoundValueEnd,
        FunctionEnd
      |]) |> toBe(true)
    });

    /* Parse: `background-image: url(http://test.com);` */
    it("parses unquoted url() argument", () => {
      expect(parse([|
        Token(Word("background-image"), 1),
        Token(Colon, 1),
        Token(Word("url"), 1),
        Token(Paren(Opening), 1),
        Token(Quote(Double), 1),
        Token(Str("http://test.com"), 1),
        Token(Quote(Double), 1),
        Token(Paren(Closing), 1),
        Token(Semicolon, 1)
      |]) == [|
        Property("background-image"),
        FunctionStart("url"),
        Value("\"http://test.com\""),
        FunctionEnd
      |]) |> toBe(true)
    });

    /* Parse: `background-image: url(http://test.com/${x});` */
    it("parses unquoted url() argument containing interpolations", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word("background-image"), 1),
        Token(Colon, 1),
        Token(Word("url"), 1),
        Token(Paren(Opening), 1),
        Token(Quote(Double), 1),
        Token(Str("http://test.com/"), 1),
        Token(Interpolation(inter), 1),
        Token(Quote(Double), 1),
        Token(Paren(Closing), 1),
        Token(Semicolon, 1)
      |]) == [|
        Property("background-image"),
        FunctionStart("url"),
        StringStart("\""),
        Value("http://test.com/"),
        ValueRef(inter),
        StringEnd,
        FunctionEnd
      |]) |> toBe(true)
    });

    /* Parse: `background-image: url(${x});` */
    it("parses unquoted url() argument containing interpolations", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word("background-image"), 1),
        Token(Colon, 1),
        Token(Word("url"), 1),
        Token(Paren(Opening), 1),
        Token(Quote(Double), 1),
        Token(Interpolation(inter), 1),
        Token(Quote(Double), 1),
        Token(Paren(Closing), 1),
        Token(Semicolon, 1)
      |]) == [|
        Property("background-image"),
        FunctionStart("url"),
        StringStart("\""),
        ValueRef(inter),
        StringEnd,
        FunctionEnd
      |]) |> toBe(true)
    });

    /* Parse: `padding: 10px 20px;` */
    it("parses compound values", () => {
      expect(parse([|
        Token(Word("padding"), 1),
        Token(Colon, 1),
        Token(Word("10px"), 1),
        Token(Word("20px"), 1),
        Token(Semicolon, 1)
      |]) == [|
        Property("padding"),
        CompoundValueStart,
        Value("10px"),
        Value("20px"),
        CompoundValueEnd
      |]) |> toBe(true)
    });

    it("throws when unexpected tokens are encountered", () => {
      expect(() => parse([|
        Token(Word("color"), 1),
        Token(Colon, 1),
        Token(Word("rgba"), 1),
        Token(Paren(Opening), 1),
        Token(Paren(Opening), 1),
        Token(Semicolon, 1)
      |])) |> toThrowMessage("unexpected token while parsing values");
    });
  });
});
