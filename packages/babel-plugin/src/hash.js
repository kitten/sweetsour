// Source: https://github.com/garycourt/murmurhash-js/blob/master/murmurhash2_gc.js
function murmurhash(str) {
  var
    l = str.length | 0,
    h = l | 0,
    i = 0,
    k;

  while (l >= 4) {
    k =
      ((str.charCodeAt(i) & 0xff)) |
      ((str.charCodeAt(++i) & 0xff) << 8) |
      ((str.charCodeAt(++i) & 0xff) << 16) |
      ((str.charCodeAt(++i) & 0xff) << 24);

    k = (((k & 0xffff) * 0x5bd1e995) + ((((k >>> 16) * 0x5bd1e995) & 0xffff) << 16));
    k ^= k >>> 24;
    k = (((k & 0xffff) * 0x5bd1e995) + ((((k >>> 16) * 0x5bd1e995) & 0xffff) << 16));

    h = (((h & 0xffff) * 0x5bd1e995) + ((((h >>> 16) * 0x5bd1e995) & 0xffff) << 16)) ^ k;

    l -= 4;
    ++i;
  }

  switch (l) {
    case 3: h ^= (str.charCodeAt(i + 2) & 0xff) << 16;
    case 2: h ^= (str.charCodeAt(i + 1) & 0xff) << 8;
    case 1: h ^= (str.charCodeAt(i) & 0xff);
            h = (((h & 0xffff) * 0x5bd1e995) + ((((h >>> 16) * 0x5bd1e995) & 0xffff) << 16));
  }

  h ^= h >>> 13;
  h = (((h & 0xffff) * 0x5bd1e995) + ((((h >>> 16) * 0x5bd1e995) & 0xffff) << 16));
  h ^= h >>> 15;

  return h >>> 0;
}

// See: https://github.com/styled-components/styled-components/blob/master/src/utils/generateAlphabeticName.js
const charsLength = 52
const getAlphabeticChar = code => String.fromCharCode(code + (code > 25 ? 39 : 97));

/* input a number, usually a hash and convert it to base-52 */
const generateAlphabeticName = code => {
  let name = ''
  let x

  /* get a char and divide by alphabet-length */
  for (x = code; x > charsLength; x = Math.floor(x / charsLength)) {
    name = getAlphabeticChar(x % charsLength) + name
  }

  return getAlphabeticChar(x % charsLength) + name
}

const hash = str => {
  const hashNum = murmurhash(str);
  const hashName = generateAlphabeticName(hashNum);
  return hashName
};

module.exports = {
  murmurhash,
  generateAlphabeticName,
  hash
};
