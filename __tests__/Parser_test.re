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

    /* Parse: `.test:not(.first, .second) {}` */
    it("parses pseudo selector functions", () => {
      expect(parse([|
        Token(Word(".test"), 1),
        Token(Colon, 1),
        Token(Word("not"), 1),
        Token(Paren(Opening), 1),
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
  });
});
