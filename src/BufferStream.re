type t('a) = {
  buffer: LinkedList.t('a),
  source: LazyStream.t('a)
};

let from = source => {
  buffer: LinkedList.create(),
  source
};

let next = stream =>
  switch (LinkedList.take(stream.buffer)) {
  | Some(x) => Some(x)
  | None => LazyStream.next(stream.source)
  };

let peek = stream =>
  switch (LinkedList.peek(stream.buffer)) {
  | Some(x) => Some(x)
  | None => LazyStream.peek(stream.source)
  };

let junk = stream => ignore(next(stream));

let buffer = (element, stream) => LinkedList.add(element, stream.buffer);

let bufferOption = (value, stream) =>
  switch value {
  | Some(element) => buffer(element, stream)
  | None => ()
  };

let put = (element, stream) => LinkedList.unshift(element, stream.buffer);

let putOption = (value, stream) =>
  switch value {
  | Some(element) => LinkedList.unshift(element, stream.buffer)
  | None => ()
  };
