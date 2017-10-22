exception Error string;

type t 'a = {
  gen: (unit => option 'a) [@bs],
  mutable buffer: option 'a
};

let from (fn: (unit => option 'a) [@bs]): t 'a => ({
  gen: fn,
  buffer: None
});

let next (stream: t 'a): option 'a => {
  switch stream.buffer {
    | Some value => {
      stream.buffer = None;
      Some value
    }
    | None => stream.gen () [@bs]
  }
};

let peek (stream: t 'a): option 'a => {
  switch stream.buffer {
    | Some value => Some value
    | None => {
      let value = stream.gen () [@bs];
      stream.buffer = value;
      value
    }
  }
};

let junk (stream: t 'a) => {
  ignore (next stream);
};
