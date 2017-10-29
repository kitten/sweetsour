type t('a) = {
  source: LazyStream.t('a),
  buffer: LinkedList.t('a)
};

/* adds a LinkedList buffer to a LazyStream */
let from = (stream: LazyStream.t('a)) : t('a) => {
  source: stream,
  buffer: LinkedList.create()
};

/* take next value from buffer and fall back to the LazyStream */
let next = (stream: t('a)) : option('a) => {
  switch (LinkedList.take(stream.buffer)) {
  | Some(x) => Some(x)
  | None => LazyStream.next(stream.source)
  }
};

/* peek at next value in the buffer and fall back to the LazyStream */
let peek = (stream: t('a)) : option('a) => {
  switch (LinkedList.peek(stream.buffer)) {
  | Some(x) => Some(x)
  | None => LazyStream.peek(stream.source)
  }
};

/* throw away the next value */
let junk = (stream: t('a)) => {
  ignore(next(stream))
};

/* add an element to the end of the buffer */
let buffer = (element: 'a, stream: t('a)) => {
  LinkedList.add(element, stream.buffer);
};

/* add an optional element to the end of the buffer */
let bufferOption = (value: option('a), stream: t('a)) => {
  switch value {
  | Some(element) => buffer(element, stream)
  | None => ()
  }
};

/* add an element to the beginning of the buffer */
let put = (element: 'a, stream: t('a)) => {
  LinkedList.unshift(element, stream.buffer);
};

/* add an optional element to the beginning of the buffer */
let putOption = (value: option('a), stream: t('a)) => {
  switch value {
  | Some(element) => LinkedList.unshift(element, stream.buffer)
  | None => ()
  }
};
