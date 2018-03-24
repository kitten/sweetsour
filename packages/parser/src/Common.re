type interpolation;

type locationRange = {
  startLoc: (int, int),
  endLoc: (int, int)
};

let string_of_char = c => Js.String.fromCharCode(Char.code(c));

let unexpected_msg = (whatKind, whenParsing) => "unexpected " ++ whatKind ++ " while parsing " ++ whenParsing;

let expected_msg = whatKind => "; expected: " ++ whatKind;
