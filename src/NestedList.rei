/* a nested linked list implementation for buffers and binary trees */

/* a type to optionally hold a value or branch off of */
type nestedListNode('a);

/* nested list data type */
type t('a);

/* create a new NestedList */
let create: unit => t('a);
/* add a new value to the end of the NestedList */
let add: ('a, t('a)) => unit;
/* appends a branch to the NestedList that traverses concatList before the rest of nestedList */
let appendBranch: (t('a), t('a)) => unit;
/* add a new value to the beginning of the NestedList */
let unshift: ('a, t('a)) => unit;
/* prepends a branch to the NestedList that traverses concatList before the rest of nestedList */
let prependBranch: (t('a), t('a)) => unit;
/* mutates the first NestedList to append the second's node chain */
let concat: (t('a), t('a)) => t('a);
/* returns size of NestedList */
let getSize: (t('a)) => int;

/* data type to iterate over NestedList */
type iterator('a);

let createIterator: (t('a)) => iterator('a);
/* look at the first value in the NestedList without discarding it */
let peek: (iterator('a)) => option('a);
/* emit the first value in the queue and shift NestedList */
let next: (iterator('a)) => option('a);
/* puts back an element onto the iterator */
let bufferUnshift: ('a, iterator('a)) => unit;
