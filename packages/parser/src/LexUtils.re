/* recognise all chars that are valid starts of a word */
let isWordStart = (c: char) => {
  c === '#' ||
  c === '.' ||
  c === '-' ||
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9')
};

let isHex = (c: char) => {
  (c >= 'a' && c <= 'f') ||
  (c >= 'A' && c <= 'F') ||
  (c >= '0' && c <= '9')
};
