exception Failure;
exception Error string;

type t 'a = {
  gen: unit => option 'a,
  buffer: ref (option 'a)
};

let from = fun (fn: unit => option 'a): t 'a => ({
  gen: fn,
  buffer: ref None
});

let next = fun (stream: t 'a): 'a => {
  switch !stream.buffer {
    | Some value => {
      stream.buffer := None;
      value
    }
    | None => {
      switch (stream.gen ()) {
        | Some value => value
        | None => raise Failure
      }
    }
  }
};

let peek = fun (stream: t 'a): option 'a => {
  switch !stream.buffer {
    | Some value => Some value
    | None => {
      let value = stream.gen ();
      stream.buffer := value;
      value
    }
  }
};

let junk = fun (stream: t 'a): unit => {
  next stream;
  ()
};
