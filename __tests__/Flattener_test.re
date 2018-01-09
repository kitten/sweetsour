open Jest;
open IstfNode;
open Flattener;

let it = test;

let flatten = (nodes: array(node)): array(node) => {
  let i = ref(0);
  let nodeStream = LazyStream.from([@bs] () => {
    if (i^ < Array.length(nodes)) {
      let node = Some(nodes[i^]);
      i := i^ + 1;
      node
    } else {
      None
    }
  });

  LazyStream.toArray(flattener(nodeStream))
};

describe("Flattener", () => {
  describe("Selectors", () => {
    open Expect;
    open! Expect.Operators;

    /* Flatten: `.test { color: papayawhip; }` */
    it("flattens single-selectors", () => {
      expect(flatten([|
        RuleKindNode(RuleStart, StyleRule),
        StringNode(Selector, ".test"),
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
        Node(RuleEnd)
      |])) == [|
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        Node(ParentSelector),
        Node(SpaceCombinator),
        StringNode(Selector, ".test"),
        Node(CompoundSelectorEnd),
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
        Node(RuleEnd)
      |];
    });

    /* Flatten: `.test div { color: papayawhip; }` */
    it("flattens compound selectors", () => {
      expect(flatten([|
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        StringNode(Selector, ".test"),
        StringNode(Selector, "div"),
        Node(CompoundSelectorEnd),
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
        Node(RuleEnd)
      |])) == [|
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        Node(ParentSelector),
        Node(SpaceCombinator),
        StringNode(Selector, ".test"),
        StringNode(Selector, "div"),
        Node(CompoundSelectorEnd),
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
        Node(RuleEnd)
      |];
    });

    /* Flatten: `> div { color: papayawhip; }` */
    it("flattens single-selectors with leading combinator", () => {
      expect(flatten([|
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        Node(ChildCombinator),
        StringNode(Selector, "div"),
        Node(CompoundSelectorEnd),
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
        Node(RuleEnd)
      |])) == [|
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        Node(ParentSelector),
        Node(ChildCombinator),
        StringNode(Selector, "div"),
        Node(CompoundSelectorEnd),
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
        Node(RuleEnd)
      |];
    });

    /* Flatten: `div { color: papayawhip; span { font-size: 12px; }}` */
    it("flattens nested selectors containing declarations", () => {
      expect(flatten([|
        RuleKindNode(RuleStart, StyleRule),
        StringNode(Selector, "div"),
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
        RuleKindNode(RuleStart, StyleRule),
        StringNode(Selector, "span"),
        StringNode(Property, "font-size"),
        StringNode(Value, "12px"),
        Node(RuleEnd),
        Node(RuleEnd)
      |])) == [|
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        Node(ParentSelector),
        Node(SpaceCombinator),
        StringNode(Selector, "div"),
        Node(CompoundSelectorEnd),
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
        Node(RuleEnd),

        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        Node(CompoundSelectorStart),
        Node(ParentSelector),
        Node(SpaceCombinator),
        StringNode(Selector, "div"),
        Node(CompoundSelectorEnd),
        Node(SpaceCombinator),
        StringNode(Selector, "span"),
        Node(CompoundSelectorEnd),
        StringNode(Property, "font-size"),
        StringNode(Value, "12px"),
        Node(RuleEnd)
      |];
    });

    /* Flatten: `@media screen { div { color: papayawhip; }}` */
    it("flattens nested selectors containing declarations", () => {
      expect(flatten([|
        RuleKindNode(RuleStart, MediaRule),
        StringNode(Condition, "screen"),
        RuleKindNode(RuleStart, StyleRule),
        StringNode(Selector, "div"),
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
        Node(RuleEnd),
        Node(RuleEnd)
      |])) == [|
        RuleKindNode(RuleStart, MediaRule),
        StringNode(Condition, "screen"),
        RuleKindNode(RuleStart, StyleRule),
        Node(CompoundSelectorStart),
        Node(ParentSelector),
        Node(SpaceCombinator),
        StringNode(Selector, "div"),
        Node(CompoundSelectorEnd),
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
        Node(RuleEnd),
        Node(RuleEnd)
      |];
    });

  });
});
