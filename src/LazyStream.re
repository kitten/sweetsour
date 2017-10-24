exception Error string;

type t 'a = {
  gen: (unit => option 'a) [@bs],
  mutable buffer: option 'a
};

/* create a new LazyStream from a iterator function */
let from (fn: (unit => option 'a) [@bs]): t 'a => ({
  gen: fn,
  buffer: None
});

/* retrieves the next value, possibly from the buffer */
let next (stream: t 'a): option 'a => {
  switch stream.buffer {
    | Some value => {
      stream.buffer = None;
      Some value
    }
    | None => stream.gen () [@bs]
  }
};

/* look at the next value, possibly buffering it to not lose it */
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

/* throw away the next value */
let junk (stream: t 'a) => {
  ignore (next stream);
};
