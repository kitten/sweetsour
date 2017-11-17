open Jest;
open Input;

let it = test;

let create_interpolation : int => Common.interpolation = [%bs.raw{|
  function(x) { return x; }
|}];

describe("Input", () => {
  open Expect;

  it("correctly iterates through chars in a string", () => {
    expect(LazyStream.toArray(input([| "hello" |], [||])) == [|
      Char('h'),
      Char('e'),
      Char('l'),
      Char('l'),
      Char('o')
    |]) |> toBe(true);
  });

  it("correctly interleaves interpolations inbetween strings", () => {
    let interpolationValueA = create_interpolation(1);
    let interpolationValueB = create_interpolation(1);

    LazyStream.toArray(input([| "xz", "-", "12" |], [| interpolationValueA, interpolationValueB |]))
      |> expect
      |> toEqual([|
        Char('x'),
        Char('z'),
        Interpolation(interpolationValueA),
        Char('-'),
        Interpolation(interpolationValueB),
        Char('1'),
        Char('2')
      |]);
  });

  it("throws when number of interpolations is invalid", () => {
    expect(() => input([| "abc", "def" |], [||]))
      |> toThrowMessage("Expected no of interpolations to equal no of strings - 1. The input is expected to be strings interleaved by the second interpolations array!");
  });
});
