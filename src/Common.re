/* Placeholder type for an interpolation */
type interpolation;

let string_of_char = (c: char) : string => {
  Js.String.fromCharCode(Char.code(c))
};

let unexpected_msg = (whatKind: string, whenParsing: string) =>
  "unexpected " ++ whatKind ++ " while parsing " ++ whenParsing;

let expected_msg = (whatKind: string) =>
  "; expected: " ++ whatKind;
