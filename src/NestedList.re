type nestedListNode('a) =
  | Value('a, ref(nestedListNode('a)))
  | Branch(ref(nestedListNode('a)), ref(nestedListNode('a)))
  | Empty;

type t('a) = {
  mutable head: ref(nestedListNode('a)),
  mutable tail: ref(nestedListNode('a)),
  mutable size: int
};

let create = () : t('a) => {
  let node = ref(Empty);
  { head: node, tail: node, size: 0 }
};

let getSize = (nestedList: t('a)) => nestedList.size;

let add = (x: 'a, nestedList: t('a)) => {
  let empty = ref(Empty);
  nestedList.size = nestedList.size + 1;
  nestedList.tail := Value(x, empty);
  nestedList.tail = empty;
};

let appendBranch = (concatList: t('a), nestedList: t('a)) => {
  let empty = ref(Empty);
  nestedList.size = nestedList.size + concatList.size;
  nestedList.tail := Branch(concatList.head, empty);
  nestedList.tail = empty;
};

let unshift = (x: 'a, nestedList: t('a)) => {
  nestedList.size = nestedList.size + 1;
  nestedList.head = ref(Value(x, nestedList.head))
};

let unshiftSecond = (x: 'a, nestedList: t('a)) =>
  switch (nestedList.head^) {
  | Value(first, secondNode) => {
    nestedList.size = nestedList.size + 1;
    nestedList.head = ref(Value(
      first,
      ref(Value(x, secondNode))
    ));
  }
  | _ => ()
  };

let unshiftBranch = (concatList: t('a), nestedList: t('a)) => {
  nestedList.size = nestedList.size + concatList.size;
  nestedList.head = ref(Branch(concatList.head, nestedList.head));
};

let unshiftBranchSecond = (concatList: t('a), nestedList: t('a)) =>
  switch (nestedList.head^) {
  | Value(first, secondNode) => {
    nestedList.size = nestedList.size + concatList.size;
    nestedList.head = ref(Value(
      first,
      ref(Branch(concatList.head, secondNode))
    ));
  }
  | _ => ()
  };

let concat = (first: t('a), second: t('a)) : t('a) =>
  switch (first.head^, second.head^) {
  | (Empty, _) => second
  | (_, Empty) => first
  | _ => {
    first.tail := second.head^;
    first.tail = second.tail;
    first.size = first.size + second.size;
    first
  }
  };

type iterator('a) = {
  nestedList: t('a),
  mutable node: ref(nestedListNode('a)),
  mutable branches: list(ref(nestedListNode('a)))
};

let createIterator = (nestedList: t('a)) : iterator('a) => {
  nestedList,
  node: nestedList.head,
  branches: []
};

let internal_next = (iterator: iterator('a), ~shouldMutate: bool) : option('a) => {
  let rec iterate = (node: nestedListNode('a)) =>
    switch (node) {
    | Value(x, next) => {
      if (shouldMutate) {
        iterator.node = next;
      };

      Some(x)
    }
    | Branch(a, b) => {
      iterator.node = a;
      iterator.branches = [b, ...iterator.branches];
      iterate(a^)
    }
    | Empty => {
      switch (iterator.branches) {
      | [b, ...branches] => {
        iterator.node = b;
        iterator.branches = branches;
        iterate(b^)
      }
      | [] => None
      }
    }
    };

  iterate(iterator.node^)
};

let next = (iterator: iterator('a)) : option('a) => internal_next(iterator, ~shouldMutate=true);
let peek = (iterator: iterator('a)) : option('a) => internal_next(iterator, ~shouldMutate=false);

let bufferUnshift = (x: 'a, iterator: iterator('a)) => {
  iterator.node = ref(Value(x, iterator.node));
}
