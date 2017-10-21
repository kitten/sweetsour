open Common;

exception InputError string;

/* An input value that can either be a char or an interpolation */
type inputValue =
  | Char char
  | Interpolation interpolation;

/* Stream type for the InputStream */
type inputStream = LazyStream.t inputValue;

/* Running state for input serialisation */
type state = {
  mutable currString: string,
  mutable currStringSize: int,
  mutable stringIndex: int,
  mutable charIndex: int
};

let input (strings: array string) (interpolations: array interpolation): inputStream => {
  let stringsSize = Array.length strings;

  /* We expect the interpolations to "fit" inbetween all strings i.e be "interleavable" */
  if (stringsSize - 1 !== Array.length interpolations) {
    raise (
      InputError
      "Expected no of interpolations to equal no of strings - 1. The input is expected to be strings interleaved by the second interpolations array!"
    );
  };

  /* Initialise a state that prompts the loop to load the first string */
  let state = {
    currString: "",
    currStringSize: 0,
    stringIndex: -1,
    charIndex: -1
  };

  let nextString () => {
    state.stringIndex = state.stringIndex + 1;
    state.currString = strings.(state.stringIndex);
    state.currStringSize = String.length state.currString;
    state.charIndex = -1;
  };

  let rec nextInputValue (): option inputValue => {
    let nextCharIndex = state.charIndex + 1;
    let nextStringIndex = state.stringIndex + 1;

    if (nextCharIndex < state.currStringSize) {
      state.charIndex = nextCharIndex;
      Some (Char state.currString.[state.charIndex])
    } else if (nextStringIndex >= stringsSize) {
      None
    } else if (nextStringIndex > 0) {
      nextString ();
      Some (Interpolation interpolations.(state.stringIndex - 1))
    } else {
      nextString ();
      nextInputValue ()
    }
  };

  /* next function needs to be defined as uncurried and arity-0 at its definition */
  let next: (unit => option inputValue) [@bs] = (fun () => {
    nextInputValue ()
  }) [@bs];

  LazyStream.from next
};
