open Jest;
open IstfNode;
open Stringifier;

let it = test;

let create_interpolation : int => Common.interpolation = [%bs.raw{|
  function(x) { return x; }
|}];

let stringOfRef : [@bs] Common.interpolation => string = [%bs.raw{|
  function(x) { return 'REF_' + x; }
|}];

let stringify = (nodes: array(node)): string => {
  let i = ref(0);
  let nodeStream = LazyStream.from([@bs] () => {
    if (i^ < Array.length(nodes)) {
      let token = Some(nodes[i^]);
      i := i^ + 1;
      token
    } else {
      None
    }
  });

  stringifier(~stringOfRef=stringOfRef)(nodeStream)
};

describe("Stringifier", () => {
  describe("Declarations", () => {
    open Expect;
    open! Expect.Operators;

    /* Stringify: `color: papayawhip;` */
    it("stringifies single-value declarations", () => {
      expect(stringify([|
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip")
      |])) === "color: papayawhip;"
    });

    /* Stringify: `color: papayawhip, palevioletred;` */
    it("stringifies multiple values", () => {
      expect(stringify([|
        StringNode(Property, "color"),
        StringNode(Value, "papayawhip"),
        StringNode(Value, "palevioletred")
      |])) === "color: papayawhip, palevioletred;";
    });

    /* Stringify: `border: 1px solid red;` */
    it("stringifies compound values", () => {
      expect(stringify([|
        StringNode(Property, "border"),
        Node(CompoundValueStart),
        StringNode(Value, "1px"),
        StringNode(Value, "solid"),
        StringNode(Value, "red"),
        Node(CompoundValueEnd)
      |])) === "border: 1px solid red;";
    });

    /* Stringify: `color: ${x};` */
    it("stringifies declarations with interpolation values", () => {
      let inter = create_interpolation(1);

      expect(stringify([|
        StringNode(Property, "color"),
        RefNode(ValueRef, inter)
      |])) === "color: REF_1;";
    });

    /* Stringify: `content: "hello", 'world';` */
    it("stringifies string values", () => {
      expect(stringify([|
        StringNode(Property, "content"),
        StringNode(Value, "\"hello\""),
        StringNode(Value, "'world'")
      |])) === "content: \"hello\", 'world';";
    });

    /* Stringify: `content: "hello ${x} world";` */
    it("stringifies string values interleaved with interpolations", () => {
      let inter = create_interpolation(1);

      expect(stringify([|
        StringNode(Property, "color"),
        StringNode(StringStart, "\""),
        StringNode(Value, "hello "),
        RefNode(ValueRef, inter),
        StringNode(Value, " world"),
        Node(StringEnd)
      |])) === "color: \"hello REF_1 world\";";
    });
  });
});

