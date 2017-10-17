open Common;

let tokenise = fun (strings: array string) (interpolations: array interpolation): array Lexer.token => {
  let lexerStream = Lexer.lexer (Input.input strings interpolations);
  let tokens = [||];

  let rec populate (): array Lexer.token => {
    switch (LazyStream.next lexerStream) {
      | Some x => {
        ignore (Js.Array.push x tokens);
        populate ()
      }
      | None => tokens
    }
  };

  populate ()
};
