open Common;

exception InputError(string);

type inputValue =
  | Char(char)
  | Interpolation(interpolation);

type inputStream = LazyStream.t(inputValue);

type state = {
  mutable currString: string,
  mutable currStringSize: int,
  mutable stringIndex: int,
  mutable charIndex: int
};

let input = (strings: array(string), interpolations: array(interpolation)) : inputStream => {
  let stringsSize = Array.length(strings);

  /* We expect the interpolations to "fit" inbetween all strings i.e be "interleavable" */
  if (stringsSize - 1 !== Array.length(interpolations)) {
    raise(InputError(
      "Expected no of interpolations to equal no of strings - 1. The input is expected to be strings interleaved by the second interpolations array!"
    ))
  };

  /* Initialise a state that prompts the loop to load the first string */
  let state = {
    currString: "",
    currStringSize: 0,
    stringIndex: -1,
    charIndex: -1
  };

  let nextString = () => {
    state.stringIndex = state.stringIndex + 1;
    state.currString = Array.unsafe_get(strings, state.stringIndex);
    state.currStringSize = String.length(state.currString);
    state.charIndex = -1;
  };

  let rec nextInputValue = () => {
    let nextCharIndex = state.charIndex + 1;
    let nextStringIndex = state.stringIndex + 1;

    if (nextCharIndex < state.currStringSize) {
      state.charIndex = nextCharIndex;
      let charVal = String.unsafe_get(state.currString, state.charIndex);
      Some(Char(charVal))
    } else if (nextStringIndex >= stringsSize) {
      None
    } else if (nextStringIndex > 0) {
      nextString();
      let interpolationVal = Array.unsafe_get(interpolations, state.stringIndex - 1);
      Some(Interpolation(interpolationVal))
    } else {
      nextString();
      nextInputValue()
    }
  };

  /* next function needs to be defined as uncurried and arity-0 at its definition */
  let next = ([@bs] () => nextInputValue());

  LazyStream.from(next)
};
