type prefix =
  | Webkit
  | WebkitMoz
  | WebkitMs
  | WebkitMozMs
  | Ms
  | Moz;

/* --------------------------------------------------------------- */
/* START: DO NOT EDIT! AUTO GENERATED USING: scripts/prefix-map.js */
let recurseBitmaps: array(int) = [|
  12427726,
  299818,
  17093134,
  1165457, 544817,
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

let bitmapOfCharAt = (str: string, at: int): int32 => {
  let code = int_of_char(String.unsafe_get(str, at));
  if (code < 97 || code > 122) {
    Int32.of_int(1)
  } else {
    Int32.shift_left(Int32.of_int(1), code - 96)
  }
};

let hammingWeight = (x: int32): int => {
  let a = Int32.sub(x, Int32.logand(Int32.shift_right(x, 1), Int32.of_int(0x55555555)));
  let a = Int32.add(Int32.logand(a, Int32.of_int(0x33333333)), Int32.logand(Int32.shift_right(a, 2), Int32.of_int(0x33333333)));
  let a = Int32.logand(Int32.add(a, Int32.shift_right(a, 4)), Int32.of_int(0x0f0f0f0f));
  let a = Int32.add(a, Int32.shift_right(a, 8));
  let a = Int32.add(a, Int32.shift_right(a, 16));
  Int32.to_int(Int32.logand(a, Int32.of_int(0x7f)))
};

let indexBitOnBitmap = (bitmap: int32, charBitmap: int32): int => {
  Int32.of_int(1)
    |> Int32.sub(charBitmap)
    |> Int32.logand(bitmap)
    |> hammingWeight
};

let prefixForProperty = (prop: string) => {
  let rec findPrefix = (index: int) : option(prefix) => {
    let charBitmap = bitmapOfCharAt(prop, index);
    let recurseBitmap = Int32.of_int(Array.unsafe_get(recurseBitmaps, index));
    let resultBitmap = Int32.of_int(Array.unsafe_get(resultBitmaps, index));
    let shouldRecurse = Int32.logand(recurseBitmap, charBitmap);

    if (shouldRecurse !== Int32.of_int(0) && index < resultsSize) {
      findPrefix(index + 1)
    } else if (shouldRecurse !== Int32.of_int(0)) {
      None
    } else if (
      (resultBitmap !== Int32.of_int(0)) &&
      (Int32.logand(resultBitmap, charBitmap) !== Int32.of_int(0))
    ) {
      let resultArray = Array.unsafe_get(results, index);
      let resultIndex = indexBitOnBitmap(resultBitmap, charBitmap);
      Some(Array.unsafe_get(resultArray, resultIndex))
    } else {
      None
    }
  };

  findPrefix(0)
};
