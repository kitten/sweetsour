/* a LinkedList implementation for buffers */

/* a type to hold an element as a LinkedList node */
type elementNode('a) = {
  value: 'a,
  mutable next: option(elementNode('a))
};

type t('a) = {
  mutable head: option(elementNode('a)),
  mutable tail: option(elementNode('a)),
  mutable size: int
};

/* create a new LinkedList */
let create: unit => t('a);
/* look at the first value in the queue */
let peek: t('a) => option('a);
/* emit the first value in the queue and shift LinkedList */
let take: t('a) => option('a);
/* add a new value to the end of the LinkedList */
let add: ('a, t('a)) => unit;
/* add a new value to the beginning of the LinkedList */
let unshift: ('a, t('a)) => unit;
/* consumes `b` and appends its values onto `a` */
let concat: (t('a), t('a)) => t('a);
