open IstfNode;

/* Stream type for the OutputStream */
type outputStream = LazyStream.t((int, rawNodePayload));

let output = (s: Parser.parserStream) : outputStream => {
  let next: [@bs] (unit => option((int, rawNodePayload))) = [@bs] (() => {
    switch (LazyStream.next(s)) {
    | Some(node) => Some(nodeToJs(node))
    | None => None
    }
  });

  LazyStream.from(next)
};
