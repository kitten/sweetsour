/* A lazy stream i.e. iterator implementation */
type t('a);

/* create a new LazyStream from an iterator function */
let from: ([@bs] unit => option('a)) => t('a);
/* retrieves the next value, possibly from the buffer */
let next: t('a) => option('a);
/* look at the next value, possibly buffering it to not lose it */
let peek: t('a) => option('a);
/* throw away the next value */
let junk: t('a) => unit;
/* collects all emissions of a stream in an array */
let toArray: t('a) => array('a);
/* wraps a LazyStream and executes a sideeffect when a new value is being emitted */
let withSideeffect: ([@bs] ('a) => unit, t('a)) => t('a);
