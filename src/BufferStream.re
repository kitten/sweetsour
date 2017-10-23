type t 'a = {
  source: LazyStream.t 'a,
  buffer: Queue.t 'a
};

let from (stream: LazyStream.t 'a): t 'a => ({
  source: stream,
  buffer: Queue.create ()
});

let next (stream: t 'a): option 'a => {
  switch (Queue.is_empty stream.buffer) {
    | true => LazyStream.next stream.source
    | false => Some (Queue.take stream.buffer)
  }
};

let peek (stream: t 'a): option 'a => {
  switch (Queue.is_empty stream.buffer) {
    | true => LazyStream.peek stream.source
    | false => Some (Queue.peek stream.buffer)
  }
};

let junk (stream: t 'a) => {
  ignore (next stream);
};

let buffer (element: 'a) (stream: t 'a) => {
  Queue.add element stream.buffer
};
