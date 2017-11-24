#!/usr/bin/env node

const w = 'Webkit';
const wm = 'WebkitMoz';
const wms = 'WebkitMs';
const wmms = 'WebkitMozMs';
const ms = 'Ms';
const m = 'Moz';

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
  'tab-s': m,
  'colu': wm,
  'filt': w,
  'flow': wms,
  'font-k': w,
  'backd': w,
  'text-e': w,
  'box-de': w,
  'clip-p': w,
  'backf': w,
  'font-fe': w,
  'align-c': w,
  'align-i': w,
  'align-s': w,
  'shape-i': w,
  'text-si': wms,
  'scroll-s': wms,
  'border-im': w,
  'transform': w,
  'transform-o': w,
  'transform-s': w,
  'text-align-l': m,
  'transition-d': w,
  'transition-p': w,
  'transition-t': w,
  'text-decoration-s': w,
  'text-decoration-l': w,
  'text-decoration-c': w
};

const charCodeBit = (str, at) => {
  const charCode = str.charCodeAt(at);
  // encode "any char" as 0, and all other a-z letters as 1-27
  const bitShift = charCode < 97 || charCode > 122 ? 0 : charCode - 96;
  // set n-th bit to 1
  const charBitmap = 1 << bitShift;
  return charBitmap | 0;
};

const prefixes = Object.keys(prefixSubstrings);
const prefixesSize = prefixes.length;
const maxKeyLength = Math.max(...prefixes.map(x => x.length));

const recurseBitmaps = [];
const resultBitmaps = [];
const values = [];

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

for (let i = 0; i < maxKeyLength; i++) {
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

  const value = [];

  for (const [charBitmap, prefix] of insertValue) {
    const k = indexBitOnBitmap(resultBitmap, charBitmap);
    value[k] = prefix;
  }

  recurseBitmaps.push(recurseBitmap);
  resultBitmaps.push(resultBitmap);
  values.push(value);
}

const output = `
/* --------------------------------------------------------------- */
/* START: DO NOT EDIT! AUTO GENERATED USING: scripts/prefix-map.js */
let recurseBitmaps: array(int32) = [|
${recurseBitmaps.map(x => `  Int32.of_int(${x})`).join(',\n')}
|];

let resultBitmaps: array(int32) = [|
${resultBitmaps.map(x => `  Int32.of_int(${x})`).join(',\n')}
|];

let results: array(array(prefix)) = [|
${values.map(arr => `  [| ${arr.join(', ').trim()} |]`).join(',\n')}
|];
/* END: DO NOT EDIT! AUTO GENERATED USING: scripts/prefix-map.js */
/* --------------------------------------------------------------- */
`.trim() + '\n'

console.log(output);
