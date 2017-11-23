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
let create = () : t('a) => {
  head: None,
  tail: None,
  size: 0
};

/* look at the first value in the queue */
let peek = (queue: t('a)) : option('a) => {
  switch queue.head {
  | None => None
  | Some(node) => Some(node.value)
  }
};

/* emit the first value in the queue and shift LinkedList */
let take = (queue: t('a)) : option('a) => {
  switch (queue.head) {
  /* emit next value, and progress queue */
  | Some(head) => {
    if (queue.size === 1) {
      queue.size = 0;
      queue.head = None;
      queue.tail = None;
    } else {
      queue.size = queue.size - 1;
      queue.head = head.next;
    };

    Some(head.value)
  }

  /* emit None when queue is empty */
  | None => None
  }
};

/* add a new value to the end of the LinkedList */
let add = (value: 'a, queue: t('a)) => {
  let node = {
    value,
    next: None
  };

  queue.size = queue.size + 1;

  switch queue.tail {
  | None =>
    queue.head = Some(node);
    queue.tail = Some(node)
  | Some(tail) =>
    tail.next = Some(node);
    queue.tail = Some(node)
  }
};

/* add a new value to the beginning of the LinkedList */
let unshift = (value: 'a, queue: t('a)) => {
  let node = {value, next: None};
  queue.size = queue.size + 1;
  switch queue.head {
  | None =>
    queue.head = Some(node);
    queue.tail = Some(node)
  | Some(head) =>
    node.next = Some(head);
    queue.head = Some(node)
  }
};

/* consumes `b` and appends its values onto `a` */
let concat = (first: t('a), second: t('a)) => {
  switch (first.tail, second.head, second.tail) {
  | (Some (oldTail), Some(head), Some(newTail)) => {
    oldTail.next = Some(head);
    first.tail = Some(newTail);
    first.size = first.size + second.size;
    first
  }

  | (_, _, None) => first
  | (_, None, _) => first
  | (None, _, _) => second
  }
};
