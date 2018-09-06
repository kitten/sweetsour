exception TemplateSizeMismatch;

type input =
  | S_CHAR(char)
  | S_REF(Token.interpolation)
  | S_EOF;

let calcSize = (strs: array(string)): int => {
  let endIndex = Array.length(strs) - 1;
  let sum = ref(0);

  for (index in 0 to endIndex) {
    let size = String.length(Array.unsafe_get(strs, index));
    sum := sum^ + size
  };

  sum^
};

let make = (
  strs: array(string),
  refs: array(Token.interpolation)
): array(input) => {
  let strSize = Array.length(strs);
  let refSize = Array.length(refs);
  if (strSize - 1 !== refSize) {
    raise(TemplateSizeMismatch)
  };

  let endIndex = strSize + refSize - 1;
  let buffer = Array.make(calcSize(strs) + refSize, S_EOF);
  let bufferIndex = ref(0);

  for (index in 0 to endIndex) {
    if (index mod 2 === 0) {
      let str = Array.unsafe_get(strs, index / 2);
      let size = String.length(str);

      for (strIndex in 0 to size - 1) {
        let c = S_CHAR(String.unsafe_get(str, strIndex));
        Array.unsafe_set(buffer, bufferIndex^ + strIndex, c);
      };

      bufferIndex := bufferIndex^ + size;
    } else {
      let _ref = Array.unsafe_get(refs, (index - 1) / 2);
      Array.unsafe_set(buffer, bufferIndex^, S_REF(_ref));
      bufferIndex := bufferIndex^ + 1;
    };
  };

  buffer
};
