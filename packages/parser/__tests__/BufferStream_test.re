open Jest;
open BufferStream;

let it = test;

describe("BufferStream", () => {
  describe("basic LazyStream wrapping", () => {
    open Expect;
    open! Expect.Operators;

    it("correctly iterates values from a producer function, imitating the passed LazyStream", () => {
      let testStream = from(LazyStream.from([@bs] () => Some(1)));
      expect((next(testStream), next(testStream))) == (Some(1), Some(1));
    });
  });

  describe("buffer & put", () => {
    open Expect;
    open! Expect.Operators;

    it("buffers an element to the end of a buffer", () => {
      let testStream = from(LazyStream.from([@bs] () => Some(1)));

      buffer(42, testStream);
      bufferOption(Some(41), testStream);

      expect((next(testStream), next(testStream), next(testStream))) == (Some(42), Some(41), Some(1));
    });

    it("puts an element at the beginning of a buffer", () => {
      let testStream = from(LazyStream.from([@bs] () => Some(1)));
      put(42, testStream);
      putOption(Some(41), testStream);

      expect((next(testStream), next(testStream), next(testStream))) == (Some(41), Some(42), Some(1));
    });
  });
});
