#!/usr/bin/env node

const w = 1;
const wm = 2;
const wms = 3;
const wmms = 4;
const ms = 5;
const m = 6;

const prefixSubstrings = {
  'j': w,
  'an': w,
  'ap': wm,
  'us': wmms,
  'br': wmms,
  'wr': wms,
  'pe': w,
  'hy': wmms,
  'gr': ms,
  'wra': ms,
  'fle': w,
  'mas': w,
  'ord': w,
  'reg': wms,
  'colu': wm,
  'filt': w,
  'flow': wms,
  'tab-s': m,
  'backd': w,
  'backf': w,
  'text-e': w,
  'font-k': w,
  'box-de': w,
  'clip-p': w,
  'font-fe': w,
  'align-c': w,
  'align-i': w,
  'align-s': w,
  'shape-i': w,
  'text-si': wms,
  'scroll-s': wms,
  'border-im': w,
  'transform': w,
  'text-align-l': m,
  'transition-d': w,
  'transition-p': w,
  'transition-t': w,
  'text-decoration-s': w,
  'text-decoration-l': w,
  'text-decoration-c': w
};

const charCodeBit = (str, at) => {
  const code = str.charCodeAt(at);
  // encode "any char" as 1, and all other a-z letters as 2-27
  if (code < 97 || code > 122) {
    return 1;
  } else {
    return (1 << (code - 96 | 0));
  }
};

const prefixes = Object.keys(prefixSubstrings);
const prefixesSize = prefixes.length;
const maxKeyLength = Math.max(...prefixes.map(x => x.length));

const recurseBitmaps = [];
const resultBitmaps = [];
const valueBitmaps = [];

const hammingWeight = x => {
  x = x - ((x >> 1) & 0x55555555)
  x = (x & 0x33333333) + ((x >> 2) & 0x33333333)
  x = (x + (x >> 4)) & 0x0f0f0f0f
  x = x + (x >> 8)
  x = x + (x >> 16)
  return x & 0x7f
}

const indexBitOnBitmap = (bitmap, positionBitmap) => (
  hammingWeight(bitmap & (positionBitmap - 1))
)

for (let i = 0; i <= maxKeyLength; i++) {
  const insertValue = [];

  let recurseBitmap = 0;
  let resultBitmap = 0;

  for (let j = 0; j < prefixesSize; j++) {
    const prefix = prefixes[j];
    if (i < prefix.length) {
      const charBitmap = charCodeBit(prefix, i);

      if (i === prefix.length - 1) {
        resultBitmap = resultBitmap | charBitmap;

        insertValue.push([
          charBitmap,
          prefixSubstrings[prefix]
        ]);
      } else {
        recurseBitmap = recurseBitmap | charBitmap;
      }
    }
  }

  let valueBitmap = 0;

  for (const [charBitmap, prefix] of insertValue) {
    const k = indexBitOnBitmap(resultBitmap, charBitmap);
    if (k > Math.floor(32 / 4)) {
      throw new Error('Value bitmap capacity is exhausted!');
    }

    valueBitmap = valueBitmap | (prefix << (k * 4));
  }

  recurseBitmaps.push(recurseBitmap);
  resultBitmaps.push(resultBitmap);
  valueBitmaps.push(valueBitmap);
}

const combinedBitmaps = [];
for (let i = 0; i < resultBitmaps.length; i++) {
  combinedBitmaps.push(recurseBitmaps[i]);
  combinedBitmaps.push(resultBitmaps[i]);
  combinedBitmaps.push(valueBitmaps[i]);
  combinedBitmaps.push(0); // If our offset is divisible by 4, the operation can be optimised
}

const output = `
/* --------------------------------------------------------------- */
/* START: DO NOT EDIT! AUTO GENERATED USING: scripts/prefix-map.js */
let combinedBitmaps: array(int) = [|
${combinedBitmaps.map(x => `  ${x}`).join(',\n')}
|];
/* END: DO NOT EDIT! AUTO GENERATED USING: scripts/prefix-map.js */
/* --------------------------------------------------------------- */
`.trim() + '\n'

console.log(output);
