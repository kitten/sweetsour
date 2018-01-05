open Jest;
open IstfNode;
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
        RuleKindNode(RuleStart, StyleRule),
        StringNode(Selector, ".test"),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        StringNode(Selector, ".first"),
        RefNode(SelectorRef, inter),
        Node(CompoundSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        StringNode(Selector, ".first"),
        Node(SpaceCombinator),
        StringNode(Selector, ".second"),
        RefNode(SelectorRef, inter),
        Node(CompoundSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        StringNode(Selector, ".first"),
        RuleKindNode(RuleStart, StyleRule),
        StringNode(Selector, ".second"),
        Node(RuleEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        StringNode(Selector, ".test"),
        StringNode(Selector, ":hover"),
        Node(CompoundSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        StringNode(Selector, ".test"),
        StringNode(Selector, ":before"),
        Node(CompoundSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        StringNode(Selector, ".test"),
        Node(SpaceCombinator),
        StringNode(Selector, ":hover"),
        Node(CompoundSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        Node(ParentSelector),
        Node(SpaceCombinator),
        StringNode(Selector, ".abc"),
        Node(SpaceCombinator),
        Node(UniversalSelector),
        StringNode(Selector, ":hover"),
        Node(CompoundSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        StringNode(Selector, ".test"),
        StringNode(Selector, ":"),
        RefNode(SelectorRef, inter),
        Node(SpaceCombinator),
        StringNode(Selector, "div"),
        Node(CompoundSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        StringNode(Selector, ".first"),
        StringNode(Selector, ".second"),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        StringNode(Selector, ".test"),
        StringNode(FunctionStart, ":not"),
        StringNode(Selector, ".first"),
        StringNode(Selector, ".second"),
        Node(FunctionEnd),
        Node(CompoundSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        StringNode(FunctionStart, ":not"),
        Node(CompoundSelectorStart),
        StringNode(Selector, ".test"),
        StringNode(FunctionStart, ":not"),
        StringNode(Selector, "div"),
        Node(FunctionEnd),
        Node(CompoundSelectorEnd),
        Node(FunctionEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        Node(ParentSelector),
        Node(ChildCombinator),
        StringNode(Selector, "div"),
        Node(CompoundSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        Node(ParentSelector),
        Node(DoubledChildCombinator),
        StringNode(Selector, "div"),
        Node(CompoundSelectorEnd),
        Node(RuleEnd)
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
      |])) |> toThrowMessage("unexpected token while parsing selectors");
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
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        StringNode(Selector, ".test"),
        AttributeKindNode(AttributeSelectorStart, CaseSensitive),
        StringNode(AttributeName, "attr"),
        Node(AttributeSelectorEnd),
        Node(CompoundSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        StringNode(Selector, ".test"),
        Node(SpaceCombinator),
        AttributeKindNode(AttributeSelectorStart, CaseSensitive),
        StringNode(AttributeName, "abc"),
        Node(AttributeSelectorEnd),
        Node(CompoundSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        AttributeKindNode(AttributeSelectorStart, CaseSensitive),
        StringNode(AttributeName, "attr"),
        StringNode(AttributeOperator, "="),
        StringNode(AttributeValue, "\"test\""),
        Node(AttributeSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        AttributeKindNode(AttributeSelectorStart, CaseSensitive),
        StringNode(AttributeName, "attr"),
        StringNode(AttributeOperator, "^="),
        StringNode(AttributeValue, "\"test\""),
        Node(AttributeSelectorEnd),
        Node(RuleEnd)
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
        RuleKindNode(RuleStart, StyleRule),
        AttributeKindNode(AttributeSelectorStart, CaseInsensitive),
        StringNode(AttributeName, "attr"),
        StringNode(AttributeOperator, "^="),
        StringNode(AttributeValue, "\"test\""),
        Node(AttributeSelectorEnd),
        Node(RuleEnd)
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
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
      |];
    });

    /* Parse: `-ms-color: papayawhip;` */
    it("parses declarations containing a vendored property", () => {
      expect(parse([|
        Token(Word("-ms-color"), (1, 1), (1, 9)),
        Token(Colon, (1, 10), (1, 10)),
        Token(Word("papayawhip"), (1, 12), (1, 21)),
        Token(Semicolon, (1, 22), (1, 22))
      |])) == [|
        StringNode(Property, "-ms-color"),
        StringNode(Value, "papayawhip"),
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
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
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
        RefNode(PropertyRef, inter),
        StringNode(Value, "papayawhip")
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
        StringNode(Property, "color"),
        RefNode(ValueRef, inter)
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
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
        StringNode(Value, "palevioletred")
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
        StringNode(Property, "color"),
        StringNode(Value, "\"hello\""),
        StringNode(Value, "'world'")
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
        StringNode(Property, "color"),
        StringNode(StringStart, "\""),
        StringNode(Value, "hello "),
        RefNode(ValueRef, inter),
        StringNode(Value, " world"),
        Node(StringEnd)
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
        StringNode(Property, "width"),
        StringNode(FunctionStart, "calc"),
        StringNode(Value, "2 * (50% - 20px)"),
        Node(FunctionEnd)
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
        StringNode(Property, "width"),
        StringNode(FunctionStart, "calc"),
        Node(CompoundValueStart),
        StringNode(Value, "2 * (50% - "),
        RefNode(ValueRef, inter),
        StringNode(Value, ")"),
        Node(CompoundValueEnd),
        Node(FunctionEnd)
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
        StringNode(Property, "background-image"),
        StringNode(FunctionStart, "url"),
        StringNode(Value, "\"http://test.com\""),
        Node(FunctionEnd)
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
        StringNode(Property, "background-image"),
        StringNode(FunctionStart, "url"),
        StringNode(StringStart, "\""),
        StringNode(Value, "http://test.com/"),
        RefNode(ValueRef, inter),
        Node(StringEnd),
        Node(FunctionEnd)
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
        StringNode(Property, "background-image"),
        StringNode(FunctionStart, "url"),
        StringNode(StringStart, "\""),
        RefNode(ValueRef, inter),
        Node(StringEnd),
        Node(FunctionEnd)
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
        StringNode(Property, "padding"),
        Node(CompoundValueStart),
        StringNode(Value, "10px"),
        StringNode(Value, "20px"),
        Node(CompoundValueEnd)
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

  describe("Partial interpolations", () => {
    open Expect;
    open! Expect.Operators;

    /* Parse: `${partial}\ncolor: papayawhip;` */
    it("parses a partial preceding a declaration", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Interpolation(inter), (1, 1), (1, 1)),
        Token(Word("color"), (2, 1), (2, 5)),
        Token(Colon, (2, 6), (2, 6)),
        Token(Word("papayawhip"), (2, 8), (2, 18)),
        Token(Semicolon, (2, 19), (2, 19))
      |])) == [|
        RefNode(PartialRef, inter),
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
      |];
    });

    /* Parse: `${partial}\n.test {}` */
    it("parses a partial preceding a selector", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Interpolation(inter), (1, 1), (1, 1)),
        Token(Word(".test"), (2, 1), (2, 5)),
        Token(Brace(Opening), (2, 7), (2, 7)),
        Token(Brace(Closing), (2, 8), (2, 8))
      |])) == [|
        RefNode(PartialRef, inter),
        RuleKindNode(RuleStart, StyleRule),
        StringNode(Selector, ".test"),
        Node(RuleEnd)
      |];
    });

    /* Parse: `${partial}\n.test {}` */
    it("parses a lone partial inside a rule block", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word(".test"), (1, 1), (1, 5)),
        Token(Brace(Opening), (1, 7), (1, 7)),
        Token(Interpolation(inter), (1, 8), (1, 8)),
        Token(Brace(Closing), (1, 9), (1, 9))
      |])) == [|
        RuleKindNode(RuleStart, StyleRule),
        StringNode(Selector, ".test"),
        RefNode(PartialRef, inter),
        Node(RuleEnd)
      |];
    });

    /* Parse: `color: blue;${partial}\ncolor: blue;` */
    it("parses a partial inbetween declarations", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Word("color"), (1, 1), (1, 5)),
        Token(Colon, (1, 6), (1, 6)),
        Token(Word("blue"), (1, 8), (1, 11)),
        Token(Semicolon, (1, 12), (1, 12)),
        Token(Interpolation(inter), (1, 13), (1, 13)),
        Token(Word("color"), (2, 1), (2, 5)),
        Token(Colon, (2, 6), (2, 6)),
        Token(Word("blue"), (2, 8), (2, 11)),
        Token(Semicolon, (2, 12), (2, 12))
      |])) == [|
        StringNode(Property, "color"),
        StringNode(Value, "blue"),
        RefNode(PartialRef, inter),
        StringNode(Property, "color"),
        StringNode(Value, "blue")
      |];
    });

    /* Parse: `${partial};color: blue;` */
    it("parses a partial followed by a semicolon and before a declaration on the same line", () => {
      let inter = create_interpolation(1);

      expect(parse([|
        Token(Interpolation(inter), (1, 1), (1, 1)),
        Token(Semicolon, (1, 2), (1, 2)),
        Token(Word("color"), (1, 3), (1, 7)),
        Token(Colon, (1, 8), (1, 8)),
        Token(Word("blue"), (1, 10), (1, 13)),
        Token(Semicolon, (1, 14), (1, 14))
      |])) == [|
        RefNode(PartialRef, inter),
        StringNode(Property, "color"),
        StringNode(Value, "blue")
      |];
    });

    it("throws when interpolation on the same line as a declaration are encountered", () => {
      let inter = create_interpolation(1);

      expect(() => parse([|
        Token(Interpolation(inter), (1, 1), (1, 1)),
        Token(Word("color"), (1, 2), (1, 6)),
        Token(Colon, (1, 7), (1, 7)),
        Token(Word("green"), (1, 9), (1, 13)),
        Token(Semicolon, (1, 14), (1, 14))
      |]))
        |> toThrowMessage(
          /* NOTE: Message says *selectors* since parsing declarations is bailed when it doesn't match
             the appropriate starting pattern (word|interpolation THEN colon) */
          "unexpected token while parsing selectors"
        );
    });
  });

  describe("At-Rules", () => {
    open Expect;
    open! Expect.Operators;

    /* Parse: `@media all {}` */
    it("parses plain words as conditions", () => {
      expect(parse([|
        Token(AtWord("@media"), (1, 1), (1, 6)),
        Token(Word("all"), (1, 8), (1, 10)),
        Token(Brace(Opening), (1, 12), (1, 12)),
        Token(Brace(Closing), (1, 13), (1, 13))
      |])) == [|
        RuleKindNode(RuleStart, MediaRule),
        StringNode(Condition, "all"),
        Node(RuleEnd)
      |];
    });

    /* Parse: `@media screen and (min-width: 900px) {}` */
    it("parses media queries with declarations", () => {
      expect(parse([|
        Token(AtWord("@media"), (1, 1), (1, 6)),
        Token(Word("screen"), (1, 8), (1, 13)),
        Token(Word("and"), (1, 15), (1, 17)),
        Token(Paren(Opening), (1, 19), (1, 19)),
        Token(Word("min-width"), (1, 20), (1, 28)),
        Token(Colon, (1, 29), (1, 29)),
        Token(Word("900px"), (1, 31), (1, 35)),
        Token(Paren(Closing), (1, 36), (1, 36)),
        Token(Brace(Opening), (1, 38), (1, 38)),
        Token(Brace(Closing), (1, 39), (1, 39))
      |])) == [|
        RuleKindNode(RuleStart, MediaRule),
        Node(CompoundConditionStart),
        StringNode(Condition, "screen"),
        StringNode(Condition, "and"),
        Node(ConditionGroupStart),
        StringNode(Property, "min-width"),
        StringNode(Value, "900px"),
        Node(ConditionGroupEnd),
        Node(CompoundConditionEnd),
        Node(RuleEnd)
      |];
    });

    /* Parse: `@media screen, print {}` */
    it("parses compounded media queries", () => {
      expect(parse([|
        Token(AtWord("@media"), (1, 1), (1, 6)),
        Token(Word("screen"), (1, 8), (1, 13)),
        Token(Comma, (1, 14), (1, 15)),
        Token(Word("print"), (1, 17), (1, 21)),
        Token(Brace(Opening), (1, 23), (1, 23)),
        Token(Brace(Closing), (1, 24), (1, 24))
      |])) == [|
        RuleKindNode(RuleStart, MediaRule),
        StringNode(Condition, "screen"),
        StringNode(Condition, "print"),
        Node(RuleEnd)
      |];
    });

    /* Parse: `@supports not (not (test)) {}` */
    it("parses nested groups in feature queries (@supports)", () => {
      expect(parse([|
        Token(AtWord("@supports"), (1, 1), (1, 6)),
        Token(Word("not"), (1, 8), (1, 10)),
        Token(Paren(Opening), (1, 12), (1, 12)),
        Token(Word("not"), (1, 13), (1, 15)),
        Token(Paren(Opening), (1, 17), (1, 17)),
        Token(Word("test"), (1, 18), (1, 21)),
        Token(Paren(Closing), (1, 22), (1, 22)),
        Token(Paren(Closing), (1, 23), (1, 23)),
        Token(Brace(Opening), (1, 24), (1, 24)),
        Token(Brace(Closing), (1, 25), (1, 25))
      |])) == [|
        RuleKindNode(RuleStart, SupportsRule),
        Node(CompoundConditionStart),
        StringNode(Condition, "not"),
        Node(ConditionGroupStart),
        Node(CompoundConditionStart),
        StringNode(Condition, "not"),
        Node(ConditionGroupStart),
        StringNode(Condition, "test"),
        Node(ConditionGroupEnd),
        Node(CompoundConditionEnd),
        Node(ConditionGroupEnd),
        Node(CompoundConditionEnd),
        Node(RuleEnd)
      |];
    });

    /* Parse: `@supports not (not (test)) {}` */
    it("parses nested groups in feature queries (@supports)", () => {
      expect(parse([|
        Token(AtWord("@supports"), (1, 1), (1, 6)),
        Token(Word("not"), (1, 8), (1, 10)),
        Token(Paren(Opening), (1, 12), (1, 12)),
        Token(Word("not"), (1, 13), (1, 15)),
        Token(Paren(Opening), (1, 17), (1, 17)),
        Token(Word("test"), (1, 18), (1, 21)),
        Token(Paren(Closing), (1, 22), (1, 22)),
        Token(Paren(Closing), (1, 23), (1, 23)),
        Token(Brace(Opening), (1, 24), (1, 24)),
        Token(Brace(Closing), (1, 25), (1, 25))
      |])) == [|
        RuleKindNode(RuleStart, SupportsRule),
        Node(CompoundConditionStart),
        StringNode(Condition, "not"),
        Node(ConditionGroupStart),
        Node(CompoundConditionStart),
        StringNode(Condition, "not"),
        Node(ConditionGroupStart),
        StringNode(Condition, "test"),
        Node(ConditionGroupEnd),
        Node(CompoundConditionEnd),
        Node(ConditionGroupEnd),
        Node(CompoundConditionEnd),
        Node(RuleEnd)
      |];
    });

    /* Parse: `@document url("test") {}` */
    it("parses document rules containing a url function", () => {
      expect(parse([|
        Token(AtWord("@document"), (1, 1), (1, 6)),
        Token(Word("url"), (1, 8), (1, 10)),
        Token(Paren(Opening), (1, 11), (1, 11)),
        Token(Quote(Double), (1, 12), (1, 12)),
        Token(Str("test"), (1, 13), (1, 16)),
        Token(Quote(Double), (1, 17), (1, 17)),
        Token(Paren(Closing), (1, 18), (1, 18)),
        Token(Brace(Opening), (1, 20), (1, 20)),
        Token(Brace(Closing), (1, 21), (1, 21))
      |])) == [|
        RuleKindNode(RuleStart, DocumentRule),
        StringNode(FunctionStart, "url"),
        StringNode(Condition, "\"test\""),
        Node(FunctionEnd),
        Node(RuleEnd)
      |];
    });
  });
});
