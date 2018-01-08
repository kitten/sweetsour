open Jest;
open NestedList;

let it = test;

describe("NestedList", () => {
  describe("add & next", () => {
    open Expect;
    open! Expect.Operators;

    it("pushes values onto the end of a linked list", () => {
      let testList = create();
      add("test 1", testList);
      add("test 2", testList);

      let iterator = createIterator(testList);

      expect([ next(iterator), next(iterator), next(iterator) ])
        == [ Some("test 1"), Some("test 2"), None ]
    });
  });

  describe("unshift & next", () => {
    open Expect;
    open! Expect.Operators;

    it("pushes values onto the end of a linked list", () => {
      let testList = create();
      unshift("test 1", testList);
      unshift("test 2", testList);

      let iterator = createIterator(testList);

      expect([ next(iterator), next(iterator), next(iterator) ])
        == [ Some("test 2"), Some("test 1"), None ]
    });
  });

  describe("unshift & put & next", () => {
    open Expect;
    open! Expect.Operators;

    it("pushes and unshifts values onto the list without losing any", () => {
      let testList = create();
      unshift("test 1", testList);
      add("test 2", testList);
      unshift("test 3", testList);

      let iterator = createIterator(testList);

      expect([ next(iterator), next(iterator), next(iterator), next(iterator) ])
        == [ Some("test 3"), Some("test 1"), Some("test 2"), None ]
    });
  });

  describe("peek", () => {
    open Expect;
    open! Expect.Operators;

    it("returns the value at the beginning of a linked list w/o discarding it", () => {
      let testList = create();
      add("test 1", testList);
      add("test 2", testList);

      let iterator = createIterator(testList);

      expect([ peek(iterator), next(iterator), peek(iterator), next(iterator), peek(iterator), next(iterator) ])
        == [ Some("test 1"), Some("test 1"), Some("test 2"), Some("test 2"), None, None ]
    });
  });

  describe("concat", () => {
    open Expect;
    open! Expect.Operators;

    it("concatenates a and b", () => {
      let a = create();
      add("A1", a);
      add("A2", a);

      let b = create();
      add("B1", b);
      add("B2", b);

      let combined = concat(a, b);
      let iterator = createIterator(combined);

      expect([ next(iterator), next(iterator), next(iterator), next(iterator), next(iterator) ])
        == [ Some("A1"), Some("A2"), Some("B1"), Some("B2"), None ]
    });

    it("works when a is empty", () => {
      let a = create();

      let b = create();
      add("test 1", b);
      add("test 2", b);

      let combined = concat(a, b);
      let iterator = createIterator(combined);

      expect([ next(iterator), next(iterator), next(iterator) ])
        == [ Some("test 1"), Some("test 2"), None ]
    });

    it("works when b is empty", () => {
      let a = create();
      add("test 1", a);
      add("test 2", a);

      let b = create();

      let combined = concat(a, b);
      let iterator = createIterator(combined);

      expect([ next(iterator), next(iterator), next(iterator) ])
        == [ Some("test 1"), Some("test 2"), None ]
    });

    it("allows unshift/add after concatenation", () => {
      let a = create();
      add("test 1", a);
      let b = create();
      add("test 2", b);
      let combined = concat(a, b);
      add("test 3", combined);
      unshift("test 0", combined);
      let iterator = createIterator(combined);

      expect([ next(iterator), next(iterator), next(iterator), next(iterator), next(iterator) ])
        == [ Some("test 0"), Some("test 1"), Some("test 2"), Some("test 3"), None ]
    });

  });

  describe("buffer-ish", () => {
    open Expect;
    open! Expect.Operators;

    it("concurrently allows take and add", () => {
      let x = create();
      let iterator = createIterator(x);

      let a = next(iterator);
      add("test 1", x);
      let b = next(iterator);
      add("test 2", x);
      let c = next(iterator);
      let d = next(iterator);
      add("test 3", x);
      add("test 4", x);
      let e = next(iterator);
      let f = next(iterator);
      let g = next(iterator);

      expect([ a, b, c, d, e, f, g ])
        == [ None, Some("test 1"), Some("test 2"), None, Some("test 3"), Some("test 4"), None ]
    });
  });

  describe("appendBranch", () => {
    open Expect;
    open! Expect.Operators;

    it("adds an 'infix' list to the list", () => {
      let x = create();
      add("test 1", x);

      let infix = create();
      add("infix 1", infix);
      add("infix 2", infix);

      appendBranch(infix, x);

      add("test 2", x);

      let iterator = createIterator(x);

      expect([ next(iterator), next(iterator), next(iterator), next(iterator), next(iterator) ])
        == [ Some("test 1"), Some("infix 1"), Some("infix 2"), Some("test 2"), None ]
    });
  });

  describe("prependBranch", () => {
    open Expect;
    open! Expect.Operators;

    it("adds a 'precursor' list to the list", () => {
      let x = create();
      add("test 1", x);

      let infix = create();
      add("infix 1", infix);
      add("infix 2", infix);

      prependBranch(infix, x);

      unshift("test 2", x);

      let iterator = createIterator(x);

      expect([ next(iterator), next(iterator), next(iterator), next(iterator), next(iterator) ])
        == [ Some("test 2"), Some("infix 1"), Some("infix 2"), Some("test 1"), None ]
    });
  });
});
