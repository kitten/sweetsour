/* identify all possible prefix combinations */
type prefix =
  | Webkit
  | WebkitMoz
  | WebkitMs
  | WebkitMozMs
  | Ms
  | Moz
  | NoPrefix;

/* How does this work?
  A previous implementation put every single property into a huge switch expression:
  - https://github.com/philpl/sweetsour/blob/0aed780/src/Prefixer.re#L1-L152
  This was sufficiently fast but bloated the final bundle size tremendously.

  It turns out that this list can be reduced by generating a list of substrings from all
  properties that need prefixing. So a property like `appearance` can be expressed using
  the unambiguous `ap...`, since all properties that start with "ap" must be prefixed.

  This data was generated from the inline-style-prefixer's static data. The problem then
  was that this data would need to be traversed one by one. Every property would have to
  be checked against every one of those substrings.

  This can be optimised by checking every character of the input property one by one,
  against all property substrings. This implies that a tree structure can be used.

  Instead of a normal tree structure we can replace every node and encode every
  char of the substrings in a bitmap. So such a bitmap would have a `1` in the n-th place
  if there's a char (n) in one of the substrings.

  This is what the `recurseBitmap` is below. The `resultBitmap` then encodes which chars
  have a result inside `results`.

  Thus the algorithm below can check the input char by char against the bitmaps below,
  one bitmap per char, and will eventually arrive at its result.
  */

/* --------------------------------------------------------------- */
/* START: DO NOT EDIT! AUTO GENERATED USING: scripts/prefix-map.js */
let combinedBitmaps: array(int) = [|
  12427726,
  1024,
  1,
  0,
  299818,
  34422816,
  4485649,
  0,
  17093134,
  524466,
  78101,
  0,
  1165457,
  11534336,
  801,
  0,
  544817,
  524368,
  1553,
  0,
  791123,
  67616,
  273,
  0,
  1085473,
  524840,
  4881,
  0,
  262664,
  524288,
  3,
  0,
  32896,
  8192,
  1,
  0,
  278528,
  0,
  0,
  0,
  3,
  0,
  0,
  0,
  1048576,
  1118224,
  4449,
  0,
  512,
  0,
  0,
  0,
  32768,
  0,
  0,
  0,
  16384,
  0,
  0,
  0,
  1,
  0,
  0,
  0,
  0,
  528392,
  273,
  0,
  0,
  0,
  0,
  0
|];
/* END: DO NOT EDIT! AUTO GENERATED USING: scripts/prefix-map.js */
/* --------------------------------------------------------------- */

/* generate a code for a char from the alphabet a-z + unknown; turn that into a bitmap
   where the code (an int) corresponds to a bit set to `1` */
let bitmapOfCharAt = (str: string, at: int): int32 => {
  let code = int_of_char(String.unsafe_get(str, at));
  if (code < 97 || code > 122) {
    Int32.of_int(1) /* char code for unknown chars */
  } else {
    Int32.shift_left(Int32.of_int(1), code - 96)
  }
};

/* calculate the hamming weight of a bitmap */
let hammingWeight = (x: int32): int => {
  let a = Int32.sub(x, Int32.logand(Int32.shift_right(x, 1), Int32.of_int(0x55555555)));
  let a = Int32.add(Int32.logand(a, Int32.of_int(0x33333333)), Int32.logand(Int32.shift_right(a, 2), Int32.of_int(0x33333333)));
  let a = Int32.logand(Int32.add(a, Int32.shift_right(a, 4)), Int32.of_int(0x0f0f0f0f));
  let a = Int32.add(a, Int32.shift_right(a, 8));
  let a = Int32.add(a, Int32.shift_right(a, 16));
  Int32.to_int(Int32.logand(a, Int32.of_int(0x7f)))
};

/* return how many `1`s are inside the `bitmap` until the `1` in `charBitmap` can be reached
   from the least significant bit */
let indexBitOnBitmap = (bitmap: int32, charBitmap: int32): int => {
  Int32.of_int(1)
    |> Int32.sub(charBitmap)
    |> Int32.logand(bitmap)
    |> hammingWeight
};

/* find the prefixes for a CSS property */
let prefixForProperty = (prop: string) => {
  let propSize = String.length(prop);

  let rec findPrefix = (index: int) : prefix => {
    let charBitmap = bitmapOfCharAt(prop, index);
    let recurseBitmap = Int32.of_int(Array.unsafe_get(combinedBitmaps, index * 4));
    let resultBitmap = Int32.of_int(Array.unsafe_get(combinedBitmaps, index * 4 + 1));
    let valueBitmap = Int32.of_int(Array.unsafe_get(combinedBitmaps, index * 4 + 2));

    if (index >= propSize) {
      NoPrefix
    } else if (Int32.logand(recurseBitmap, charBitmap) > Int32.of_int(0)) {
      findPrefix(index + 1) /* check next char if the recurseBitmap has a `1` for the current char in prop */
    } else if (Int32.logand(resultBitmap, charBitmap) > Int32.of_int(0)) {
      /* retrieve shift-right index to move 4-bit value slot to the right */
      let valueIndex = indexBitOnBitmap(resultBitmap, charBitmap) * 4;
      /* mask 4-bits on the right */
      let value = Int32.logand(Int32.shift_right_logical(valueBitmap, valueIndex), Int32.of_int(15));
      /* convert int to prefix constructor */

      switch (Int32.to_int(value)) {
        | 1 => Webkit
        | 2 => WebkitMoz
        | 3 => WebkitMs
        | 4 => WebkitMozMs
        | 5 => Ms
        | 6 => Moz
        | _ => NoPrefix
      }
    } else {
      /* if there's no `1` at the char's place in either bitmaps, there's no prefixing to be done */
      NoPrefix
    }
  };

  findPrefix(0)
};
