exception Error string;

type t 'a = {
  gen: (unit => option 'a) [@bs],
  mutable buffer: option 'a
};

let from = fun (fn: (unit => option 'a) [@bs]): t 'a => ({
  gen: fn,
  buffer: None
});

let next = fun (stream: t 'a): option 'a => {
  switch stream.buffer {
    | Some value => {
      stream.buffer = None;
      Some value
    }
    | None => stream.gen () [@bs]
  }
};

let peek = fun (stream: t 'a): option 'a => {
  switch stream.buffer {
    | Some value => Some value
    | None => {
      let value = stream.gen () [@bs];
      stream.buffer = value;
      value
    }
  }
};

let junk = fun (stream: t 'a) => {
  next stream;
  ()
};
