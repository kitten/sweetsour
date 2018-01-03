type elementNode('a) = {
  value: 'a,
  mutable next: option(elementNode('a))
};

type t('a) = {
  mutable head: option(elementNode('a)),
  mutable tail: option(elementNode('a)),
  mutable size: int
};

let create = () => {
  head: None,
  tail: None,
  size: 0
};

let peek = queue =>
  switch queue.head {
  | None => None
  | Some(node) => Some(node.value)
  };

let take = queue =>
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
  };

let add = (value, queue) => {
  let node = { value, next: None };

  queue.size = queue.size + 1;

  switch queue.tail {
  | None =>
    queue.head = Some(node);
    queue.tail = Some(node);
  | Some(tail) =>
    tail.next = Some(node);
    queue.tail = Some(node);
  }
};

let unshift = (value, queue) => {
  let node = { value, next: None };
  queue.size = queue.size + 1;

  switch queue.head {
  | None =>
    queue.head = Some(node);
    queue.tail = Some(node);
  | Some(head) =>
    node.next = Some(head);
    queue.head = Some(node);
  }
};

let concat = (first, second) =>
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
  };
