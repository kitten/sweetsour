open Jest;

let it = test;

describe("Normalize Test Suite", () => {
  open Expect;

  it("parses normalize.css correctly", () => {
    let cwd = Node.Process.cwd();
    let css = Node.Fs.readFileSync(cwd ++ "/__tests__/suite/normalize.css", `utf8);
    let output = Input.input([| css |], [||])
      |> Lexer.lexer
      |> Parser.parser
      |> Printer.printer
      |> Printer.printToStr;

    expect(output) |> toMatchSnapshot
  });
});
