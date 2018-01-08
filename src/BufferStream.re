type t('a) = {
  source: LazyStream.t('a),
  buffer: NestedList.t('a),
  mutable iterator: NestedList.iterator('a)
};

let from = source => {
  let buffer = NestedList.create();
  let iterator = NestedList.createIterator(buffer);
  { buffer, iterator, source }
};

let next = stream =>
  switch (NestedList.next(stream.iterator)) {
  | Some(_) as x => x
  | None => LazyStream.next(stream.source)
  };

let peek = stream =>
  switch (NestedList.peek(stream.iterator)) {
  | Some(_) as x => x
  | None => LazyStream.peek(stream.source)
  };

let junk = stream => ignore(next(stream));

let buffer = (element, stream) => NestedList.add(element, stream.buffer);

let bufferOption = (value, stream) =>
  switch value {
  | Some(element) => buffer(element, stream)
  | None => ()
  };

let put = (element, stream) => NestedList.bufferUnshift(element, stream.iterator);

let putOption = (value, stream) =>
  switch value {
  | Some(element) => put(element, stream)
  | None => ()
  };
