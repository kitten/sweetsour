/* capture stream until takeUntil yields true and returns a function
   that when called returns a LazyStream which emits this slice without
   destroying it */
let from = (s: LazyStream.t('a), takeUntil: [@bs] ('a) => bool) => {
  /* recursively take items from stream until takeUntil yields true */
  let bufferUntil = () => {
    let buffer = LinkedList.create();
    let rec explode = () => {
      switch (LazyStream.peek(s)) {
        | Some(x) when ([@bs] takeUntil(x)) === false => {
          LazyStream.junk(s);
          LinkedList.add(x, buffer);
          explode()
        }
        | _ => buffer
      }
    };

    explode()
  };

  let slice = bufferUntil();

  /* create a LazyStream non-mutably iterating the slice */
  [@bs] () => {
    let head = ref(slice.head);

    LazyStream.from([@bs] () => {
      switch (head^) {
        | Some(x) => {
          head := x.next;
          Some(x.value)
        }
        | None => None
      }
    });
  };
};
