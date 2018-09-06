exception TemplateSizeMismatch;

type input =
  | S_CHAR(char)
  | S_REF(Token.interpolation)
  | S_EOF;

let calcSize = (strs: array(string)): int => {
  let strSize = Array.length(strs);

  let rec sumStrSizes = (index: int) =>
    if (index < strSize) {
      let size = String.length(Array.unsafe_get(strs, index));
      size + sumStrSizes(index + 1)
    } else {
      0
    };

  sumStrSizes(0)
};

let assignChars = (
  str: string,
  start: int,
  length: int,
  buffer: array(input)
) => {
  let rec explode = (index: int) =>
    if (index < length) {
      let c = String.unsafe_get(str, index);
      Array.unsafe_set(buffer, start + index, S_CHAR(c));
      explode(index + 1);
    };

  explode(0)
};

let makeBuffer = (
  strs: array(string),
  refs: array(Token.interpolation)
): array(input) => {
  let strSize = Array.length(strs);
  let refSize = Array.length(refs);
  let totalSize = strSize + refSize;
  let buffer = Array.make(calcSize(strs) + refSize, S_EOF);

  let rec populate = (index: int, bufferIndex: int) =>
    if (index >= totalSize) {
      buffer
    } else if (index mod 2 === 0) {
      let str = Array.unsafe_get(strs, index / 2);
      let size = String.length(str);
      assignChars(str, bufferIndex, size, buffer);
      populate(index + 1, bufferIndex + size)
    } else {
      let _ref = Array.unsafe_get(refs, (index - 1) / 2);
      Array.unsafe_set(buffer, bufferIndex, S_REF(_ref));
      populate(index + 1, bufferIndex + 1)
    };

  populate(0, 0)
};

let make = (
  strs: array(string),
  refs: array(Token.interpolation)
): array(input) => {
  let strSize = Array.length(strs);
  let refSize = Array.length(refs);

  if (strSize - 1 !== refSize) {
    raise(TemplateSizeMismatch)
  } else {
    makeBuffer(strs, refs)
  }
};
