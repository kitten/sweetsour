type t('a) = {
  gen: [@bs] (unit => option('a)),
  mutable buffer: option('a)
};

let from = fn => {
  gen: fn,
  buffer: None
};

let next = stream => {
  switch stream.buffer {
  | Some(value) =>
    stream.buffer = None;
    Some(value)
  | None => [@bs] stream.gen()
  }
};

let peek = stream => {
  switch stream.buffer {
  | Some(value) => Some(value)
  | None =>
    let value = [@bs] stream.gen();
    stream.buffer = value;
    value
  }
};

let junk = stream => ignore(next(stream));

let toArray = stream => {
  let emissions = [||];
  let rec populate = () : array('a) => {
    switch (next(stream)) {
    | Some(x) => {
      ignore(Js.Array.push(x, emissions));
      populate()
    }
    | None => emissions
    }
  };

  populate()
};

let withSideeffect = (sideeffect, stream) => {
  from([@bs] () => {
    switch (next(stream)) {
    | Some(value) as x => {
      ([@bs] sideeffect(value));
      x
    }
    | None => None
    }
  });
};
