type prefix =
  | Webkit
  | WebkitMoz
  | WebkitMs
  | WebkitMozMs
  | Ms
  | Moz;

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
let recurseBitmaps: array(int) = [|
  12427726,
  299818,
  17093134,
  1165457,
  544817,
  791123,
  1085473,
  262664,
  41088,
  278529,
  3,
  1048576,
  512,
  32768,
  16384,
  1,
  0
|];

let resultBitmaps: array(int) = [|
  1024,
  34422816,
  524466,
  11534336,
  524368,
  67616,
  524840,
  524288,
  8192,
  0,
  557056,
  1118224,
  0,
  0,
  0,
  0,
  528392
|];

let results: array(array(prefix)) = [|
  [| Webkit |],
  [| Webkit, Webkit, WebkitMoz, Ms, WebkitMozMs, WebkitMozMs |],
  [| Ms, Webkit, Webkit, WebkitMs, Webkit |],
  [| Webkit, WebkitMoz, WebkitMs |],
  [| Webkit, Webkit, Moz |],
  [| Webkit, Webkit, Webkit |],
  [| Webkit, Webkit, WebkitMs, Webkit |],
  [| WebkitMs |],
  [| Webkit |],
  [|  |],
  [| Webkit, Webkit |],
  [| Webkit, Moz, Webkit, Webkit |],
  [|  |],
  [|  |],
  [|  |],
  [|  |],
  [| Webkit, Webkit, Webkit |]
|];
/* END: DO NOT EDIT! AUTO GENERATED USING: scripts/prefix-map.js */
/* --------------------------------------------------------------- */

let resultsSize = Array.length(results);

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
  let rec findPrefix = (index: int) : option(prefix) => {
    let charBitmap = bitmapOfCharAt(prop, index);
    let recurseBitmap = Int32.of_int(Array.unsafe_get(recurseBitmaps, index));
    let resultBitmap = Int32.of_int(Array.unsafe_get(resultBitmaps, index));
    let shouldRecurse = Int32.logand(recurseBitmap, charBitmap);

    if (shouldRecurse !== Int32.of_int(0) && index < resultsSize) {
      findPrefix(index + 1) /* check next char if the recurseBitmap has a `1` for the current char in prop */
    } else if (shouldRecurse !== Int32.of_int(0)) {
      None
    } else if (
      (resultBitmap !== Int32.of_int(0)) &&
      (Int32.logand(resultBitmap, charBitmap) !== Int32.of_int(0))
    ) {
      /* retrieve a result from the results array if we've reached a `1` in the resultBitmap */
      let resultArray = Array.unsafe_get(results, index);
      let resultIndex = indexBitOnBitmap(resultBitmap, charBitmap);
      Some(Array.unsafe_get(resultArray, resultIndex))
    } else {
      /* if there's no `1` at the char's place in either bitmaps, there's no prefixing to be done */
      None
    }
  };

  findPrefix(0)
};
