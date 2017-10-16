open Common;

type inputValue =
  | Char char
  | Interpolation interpolation;

type inputStream = Stream.t inputValue;

let input = fun (strings: array string) (interpolations: array interpolation): inputStream => {
  let bufferedItem = ref None;
  let stringsSize = Js.Array.length strings;
  let stringIndex = ref 0;
  let charIndex = ref 0;

  let advance () => {
    let str = strings.(!stringIndex);
    let strSize = String.length str;

    if (!charIndex >= strSize - 1) {
      bufferedItem := Some (Interpolation interpolations.(!stringIndex));
      stringIndex := !stringIndex + 1;
      charIndex := 0;
    } else {
      charIndex := !charIndex + 1;
    }
  };

  let next _: option inputValue => {
    switch !bufferedItem {
      | Some item => {
        Some item
      }
      | None => {
        if (!stringIndex >= stringsSize) {
          None
        } else {
          let value = strings.(!stringIndex).[!charIndex];
          advance ();
          Some (Char value)
        }
      }
    }
  };

  Stream.from next
};
