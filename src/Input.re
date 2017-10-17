open Common;

/* An input value that can either be a char or an interpolation */
type inputValue =
  | Char char
  | Interpolation interpolation;

type inputStream = LazyStream.t inputValue;

type state = {
  mutable currString: string,
  mutable currStringSize: int,
  mutable stringIndex: int,
  mutable charIndex: int
};

let input = fun (strings: array string) (interpolations: array interpolation): inputStream => {
  let stringsSize = Js.Array.length strings;

  let state = {
    currString: "",
    currStringSize: 0,
    stringIndex: -1,
    charIndex: -1,
  };

  let rec nextInputValue (): option inputValue => {
    state.charIndex = state.charIndex + 1;

    if (state.charIndex >= state.currStringSize) {
      state.stringIndex = state.stringIndex + 1;

      if (state.stringIndex < stringsSize) {
        state.currString = strings.(state.stringIndex);
        state.currStringSize = String.length state.currString;
        state.charIndex = -1;

        if (state.stringIndex > 0) {
          Some (Interpolation interpolations.(state.stringIndex - 1))
        } else {
          nextInputValue ()
        }
      } else {
        None
      }
    } else {
      Some (Char state.currString.[state.charIndex])
    }
  };

  /* next function needs to be defined as uncurried and arity-0 at its definition */
  let next: (unit => option inputValue) [@bs] = (fun () => {
    nextInputValue ()
  }) [@bs];

  LazyStream.from next
};
