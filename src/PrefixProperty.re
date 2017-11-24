type prefix =
  | Webkit
  | WebkitMoz
  | WebkitMs
  | WebkitMozMs
  | Ms
  | Moz;

/* --------------------------------------------------------------- */
/* START: DO NOT EDIT! AUTO GENERATED USING: scripts/prefix-map.js */
let recurseBitmaps: array(int32) = [|
  Int32.of_int(12427726),
  Int32.of_int(299818),
  Int32.of_int(17093134),
  Int32.of_int(1165457),
  Int32.of_int(544817),
  Int32.of_int(791123),
  Int32.of_int(1085473),
  Int32.of_int(262664),
  Int32.of_int(41088),
  Int32.of_int(278529),
  Int32.of_int(3),
  Int32.of_int(1048576), Int32.of_int(512),
  Int32.of_int(32768),
  Int32.of_int(16384),
  Int32.of_int(1),
  Int32.of_int(0)
|];

let resultBitmaps: array(int32) = [|
  Int32.of_int(1024),
  Int32.of_int(34422816),
  Int32.of_int(524466),
  Int32.of_int(11534336),
  Int32.of_int(524368),
  Int32.of_int(67616),
  Int32.of_int(524840),
  Int32.of_int(524288),
  Int32.of_int(8192),
  Int32.of_int(0),
  Int32.of_int(557056),
  Int32.of_int(1118224),
  Int32.of_int(0),
  Int32.of_int(0),
  Int32.of_int(0),
  Int32.of_int(0),
  Int32.of_int(52839),
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
  [||],
  [| Webkit, Webkit |],
  [| Webkit, Moz, Webkit, Webkit |],
  [||],
  [||],
  [||],
  [||],
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

let hammingWeight = (x: int32): int32 => {
  let a = Int32.sub(x, Int32.logand(Int32.shift_right(x, 1), Int32.of_int(0x55555555)));
  let b = Int32.add(Int32.logand(a, Int32.of_int(0x33333333)), Int32.logand(Int32.shift_right(a, 2), Int32.of_int(0x33333333)));
  let c = Int32.logand(Int32.add(b, Int32.shift_right(b, 4)), Int32.of_int(0x0f0f0f0f));
  let d = Int32.add(c, Int32.shift_right(c, 8));
  let e = Int32.add(d, Int32.shift_right(d, 16));
  Int32.logand(e, Int32.of_int(0x7f))
};

let indexBitOnBitmap = (bitmap: int32, charBitmap: int32): int => {
  Int32.of_int(1)
    |> Int32.sub(charBitmap)
    |> Int32.logand(bitmap)
    |> hammingWeight
    |> Int32.to_int
};

let prefixForProperty = (prop: string) => {
  let rec findPrefix = (index: int) : option(prefix) => {
    let charBitmap = bitmapOfCharAt(prop, index);
    let recurseBitmap = Array.unsafe_get(recurseBitmaps, index);
    let resultBitmap = Array.unsafe_get(resultBitmaps, index);
    let shouldRecurse = Int32.logand(recurseBitmap, charBitmap) !== Int32.of_int(0);

    if (shouldRecurse && index >= resultsSize) {
      None
    } else if (shouldRecurse) {
      findPrefix(index + 1)
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
