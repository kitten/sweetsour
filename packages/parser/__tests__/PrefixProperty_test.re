open Jest;
open PrefixProperty;

let it = test;

describe("PrefixProperty", () => {
  describe("prefixForProperty", () => {
    open Expect;

    it("should prefix appearance", () =>
      expect(prefixForProperty("appearance")) |> toEqual(WebkitMoz));
    it("should prefix transform", () =>
      expect(prefixForProperty("transform")) |> toEqual(Webkit));
    it("should not prefix border", () =>
      expect(prefixForProperty("border")) |> toEqual(NoPrefix));
    it("should not prefix color", () =>
      expect(prefixForProperty("color")) |> toEqual(NoPrefix));
  });
});
