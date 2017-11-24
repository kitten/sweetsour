open Jest;
open PrefixProperty;

let it = test;

describe("PrefixProperty", () => {
  describe("prefixForProperty", () => {
    open Expect;

    it("should prefix appearance", () => {
      expect(prefixForProperty("appearance")) |> toEqual(Some(WebkitMoz));
    });

    it("should not prefix border", () => {
      expect(prefixForProperty("border")) |> toEqual(None);
    });
  });
});
