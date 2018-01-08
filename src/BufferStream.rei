/* A wrapper around LazyStream that features a arbitrarily large buffer */
type t('a);

/* adds a NestedList buffer to a LazyStream */
let from: LazyStream.t('a)=> t('a);
/* take next value from buffer and fall back to the LazyStream */
let next: t('a) => option('a);
/* peek at next value in the buffer and fall back to the LazyStream */
let peek: t('a) => option('a);
/* throw away the next value */
let junk: t('a) => unit;
/* add an element to the end of the buffer */
let buffer: ('a, t('a)) => unit;
/* add an optional element to the end of the buffer */
let bufferOption: (option('a), t('a)) => unit;
/* add an element to the beginning of the buffer */
let put: ('a, t('a)) => unit;
/* add an optional element to the beginning of the buffer */
let putOption: (option('a), t('a)) => unit;
