exception TemplateSizeMismatch;

type input =
  | S_CHAR(char)
  | S_REF(Token.interpolation)
  | S_EOF;

type envT = {
  mutable str: string,
  mutable size: int,
  mutable index: int,
  mutable offset: int
};

let make = (
  strs: array(string),
  refs: array(Token.interpolation)
): (unit => input) => {
  let strSize = Array.length(strs);
  let refSize = Array.length(refs);

  if (strSize - 1 !== refSize) raise(TemplateSizeMismatch);

  let env: envT = {
    str: "",
    size: 0,
    index: -1,
    offset: -1
  };

  let nextString = () => {
    env.index = env.index + 1;
    env.str = Array.unsafe_get(strs, env.index);
    env.size = String.length(env.str);
    env.offset = -1;
  };

  let rec next = () => {
    let nextOffset = env.offset + 1;
    let nextIndex = env.index + 1;

    if (nextOffset < env.size) {
      env.offset = nextOffset;
      S_CHAR(String.unsafe_get(env.str, env.offset))
    } else if (nextIndex >= strSize) {
      S_EOF
    } else if (nextIndex > 0) {
      nextString();
      S_REF(Array.unsafe_get(refs, env.index - 1))
    } else {
      nextString();
      next()
    }
  };

  next
};
