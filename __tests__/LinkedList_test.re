open Jest;
open LinkedList;

let it = test;

describe("LinkedList", () => {
  describe("add", () => {
    open Expect;

    it("pushes values onto the end of a linked list", () => {
      let testList = create();
      add("test 1", testList);
      add("test 2", testList);

      [take(testList), take(testList), take(testList)]
        |> expect
        |> toEqual([ Some("test 1"), Some("test 2"), None ]);
    });

    it("updates the size property on a list", () => {
      let testList = create();
      add("test 1", testList);
      add("test 2", testList);

      let initialSize = testList.size;
      add("test 3", testList);

      expect([initialSize, testList.size]) |> toEqual([2, 3]);
    });
  });

  describe("unshift", () => {
    open Expect;

    it("unshifts values onto the beginning of a linked list", () => {
      let testList = create();
      add("test 1", testList);
      unshift("test 2", testList);

      [take(testList), take(testList), take(testList)]
        |> expect
        |> toEqual([ Some("test 2"), Some("test 1"), None ]);
    });

    it("updates the size property on a list", () => {
      let testList = create();
      add("test 1", testList);
      add("test 2", testList);

      let initialSize = testList.size;
      unshift("test 3", testList);

      expect([initialSize, testList.size]) |> toEqual([2, 3]);
    });
  });

  describe("peek", () => {
    open Expect;

    it("returns the value the beginning of a linked list", () => {
      let testList = create();
      add("test 1", testList);
      add("test 2", testList);

      [peek(testList), take(testList), take(testList), take(testList)]
        |> expect
        |> toEqual([ Some("test 1"), Some("test 1"), Some("test 2"), None ]);
    });
  });
});
