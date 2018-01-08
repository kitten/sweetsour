open Common;
open IstfNode;

exception StringifierError;

type stringifyValuesMode =
  | Main
  | Function
  | Compound;

let stringifier = (~stringOfRef: [@bs] (interpolation) => string) => (s: nodeStream) : string => {
  let stringifyString = (quote: string) => {
    let rec explode = (css: string) : string => {
      switch (LazyStream.next(s)) {
      | Some(StringNode(Value, str)) => explode(css ++ str)
      | Some(RefNode(ValueRef, x)) => explode(css ++ [@bs] stringOfRef(x))
      | Some(Node(StringEnd)) => quote ++ css ++ quote
      | _ => raise(StringifierError)
      }
    };

    explode("")
  };

  let stringifyValues = () => {
    let makePrefix = (mode: stringifyValuesMode, isFirst: bool) : string =>
      switch (isFirst, mode) {
      | (true, _) => ""
      | (false, Function | Main) => ", "
      | (false, Compound) => " "
      };

    let rec explode = (css: string, ~mode: stringifyValuesMode, ~isFirst: bool) : string => {
      let prefix = makePrefix(mode, isFirst);

      switch (LazyStream.peek(s), mode) {
      | (Some(StringNode(StringStart, quote)), _) => {
        LazyStream.junk(s);
        explode(css ++ prefix ++ stringifyString(quote), ~mode, ~isFirst=false)
      }

      | (Some(StringNode(Value, value)), _) => {
        LazyStream.junk(s);
        explode(css ++ prefix ++ value, ~mode, ~isFirst=false)
      }

      | (Some(RefNode(ValueRef, x)), _) => {
        LazyStream.junk(s);
        explode(css ++ prefix ++ [@bs] stringOfRef(x), ~mode, ~isFirst=false)
      }

      | (Some(Node(CompoundValueStart)), Main | Function) => {
        LazyStream.junk(s);
        let inner = explode(css, ~mode=Compound, ~isFirst=true);
        explode(prefix ++ inner, ~mode, ~isFirst)
      }

      | (Some(Node(CompoundValueEnd)), Compound) => {
        LazyStream.junk(s);
        css
      }

      | (Some(StringNode(FunctionStart, fnName)), Main | Compound) => {
        LazyStream.junk(s);
        let prepend = prefix ++ fnName ++ "(";
        let inner = explode(css, ~mode=Function, ~isFirst=true);
        explode(prepend ++ inner, ~mode, ~isFirst)
      }

      | (Some(Node(FunctionEnd)), Function) => {
        LazyStream.junk(s);
        css ++ ")"
      }

      | (_, Main) => css
      | _ => raise(StringifierError)
      }
    };

    explode("", ~mode=Main, ~isFirst=true) ++ ";"
  };

  let rec next = (css: string) =>
    switch (LazyStream.next(s)) {
    | Some(StringNode(Property, property)) => next(property ++ ": " ++ stringifyValues())
    | None => css
    | _ => raise(StringifierError)
    };

  next("")
};
