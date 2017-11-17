open Jest;
open LazyStream;

let it = test;

describe("LazyStream", () => {
  open Expect;

  it("correctly iterates values from a producer function", () => {
    let testStream = from([@bs] () => Some(1));

    expect([ next(testStream), next(testStream) ])
      |> toEqual([
        Some(1),
        Some(1)
      ]);
  });

  it("passes on None when it's emitted from a producer", () => {
    let hasEmitted = ref(false);
    let testStream = from([@bs] () => {
      if (!hasEmitted^) {
        hasEmitted := true;
        Some(1)
      } else {
        None
      }
    });

    expect([ next(testStream), next(testStream) ])
      |> toEqual([
        Some(1),
        None
      ]);
  });

  it("allows peek to cache a value until next is called", () => {
    let hasEmitted = ref(false);
    let testStream = from([@bs] () => {
      if (!hasEmitted^) {
        hasEmitted := true;
        Some(1)
      } else {
        None
      }
    });

    expect([ peek(testStream), peek(testStream), next(testStream), next(testStream) ])
      |> toEqual([
        Some(1),
        Some(1),
        Some(1),
        None
      ]);
  });

  it("allows junk to skip a value", () => {
    let hasEmitted = ref(false);
    let testStream = from([@bs] () => {
      if (!hasEmitted^) {
        hasEmitted := true;
        Some(1)
      } else {
        None
      }
    });

    junk(testStream);
    expect(next(testStream)) |> toEqual(None);
  });

  it("allows toArray to collect all values", () => {
    let index = ref(0);
    let testStream = from([@bs] () => {
      if (index^ < 5) {
        index := index^ + 1;
        Some(index^)
      } else {
        None
      }
    });

    expect(toArray(testStream)) |> toEqual([| 1, 2, 3, 4, 5 |]);
  });
});
