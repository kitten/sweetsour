/* Placeholder type for an interpolation */
type interpolation;

/* Location range of start and end, signifying (row, column) */
type locationRange = {
  startLoc: (int, int),
  endLoc: (int, int)
};

let string_of_char: char => string;
let unexpected_msg: (string, string) => string;
let expected_msg: string => string;
