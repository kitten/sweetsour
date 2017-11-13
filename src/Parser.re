open Common;

/* For RuleStart kinds */
type ruleKind =
  | StyleRule /* CSSOM */
  | CharsetRule /* CSSOM */
  | ImportRule /* CSSOM */
  | MediaRule /* CSSOM */
  | FontFaceRule /* CSSOM */
  | PageRule /* CSSOM */
  | KeyframesRule /* CSS 3 Animations */
  | KeyframeRule /* CSS 3 Animations */
  | MarginRule /* CSSOM */
  | NamespaceRule /* CSSOM */
  | CounterStyleRule /* CSS 3 Lists */
  | SupportsRule /* CSS 3 Conditional */
  | DocumentRule /* CSS 3 Conditional */
  | FontFeatureValuesRule /* CSS 3 Fonts */
  | ViewportRule /* CSS Device Adapt */
  | RegionStyleRule; /* Proposed for CSS 3 Regions */

/* A node is represented by its (ISTF) type and potentially a value */
type node =
  | RuleStart(ruleKind)
  | RuleEnd
  | RuleName(string)
  | Selector(string)
  | ParentSelector
  | CompoundSelectorStart
  | CompoundSelectorEnd
  | SpaceCombinator
  | DoubledChildCombinator
  | ChildCombinator
  | NextSiblingCombinator
  | SubsequentSiblingCombinator
  | Property(string)
  | Value(string)
  | CompoundValueStart
  | CompoundValueEnd
  | Condition(string)
  | FunctionStart(string)
  | FunctionEnd
  | AnimationName(string)
  | SelectorRef(interpolation)
  | PropertyRef(interpolation)
  | ValueRef(interpolation)
  | PartialRef(interpolation)
  | StringStart
  | StringEnd
  | EOF;

/* Stream type for the ParserStream */
type parserStream = LazyStream.t(node);

/* Modes the parser can be in, allowing encapsulated and specialised logic */
type parserMode =
  | MainLoop
  | PropertyLoop
  | ValueLoop
  | CompoundValueMode
  | SelectorLoop;

/* Running state for parsing */
type state = {
  /* value to keep track of the current line number, copied from incoming tokens */
  mutable line: int,
  /* value to keep track of the current rule nesting */
  mutable nestedness: int,
  /* a buffer holding the last emitted node */
  mutable lastNode: node,
  /* the current mode of the parser */
  mutable mode: parserMode
};

let parser = (s: Lexer.lexerStream) => {
  /* a buffer stream emitting tokens combined with an internal buffer */
  let buffer = BufferStream.from(s);

  let state = {
    line: 0,
    nestedness: 0,
    lastNode: EOF,
    mode: MainLoop
  };

  let parseToken = (t: option(Lexer.token)) : option(Lexer.tokenValue) => {
    switch t {
    | Some(Token(value, line)) => {
      state.line = line;
      Some(value)
    }

    | None => None
    }
  };

  let parseValueOrInterpolation = (t: Lexer.tokenValue) => {
    switch t {
    | Word(word) => Value(word)
    | Interpolation(x) => ValueRef(x)
    | _ => {
      raise(LazyStream.Error(
        "Unexpected token while parsing a value, expected a Word or Interpolation"
      ))
    }
    }
  };

  /* recognises all tokens that are not valid as part of a selector;
     must be updated when Lexer tokens are changed */
  let isSelectorToken = (t: Lexer.tokenValue) => {
    switch t {
    | Brace(_)
    | AtWord(_)
    | Exclamation
    | Semicolon => false
    | _ => true
    }
  };

  let selectorLoop = () : node => {
    RuleStart(StyleRule) /* TODO: parse selector */
  };

  let propertyLoop = () : node => {
    /* emit node for word or interpolation property */
    let node =
      switch (parseToken(BufferStream.next(buffer))) {
      | Some(Word(str)) => Property(str)
      | Some(Interpolation(x)) => PropertyRef(x)
      | _ => {
        raise(LazyStream.Error(
          "Unexpected token while parsing a property, expected a Word or Interpolation"
        ))
      }
      };

    /* enforce a colon token after a property */
    switch (parseToken(BufferStream.next(buffer))) {
    | Some(Colon) => ()
    | _ => raise(LazyStream.Error("Unexpected token after parsing a property, expected a Colon"))
    };

    /* switch to ValueLoop */
    state.mode = ValueLoop;
    node
  };

  let compoundValueLoop = () : node => {
    switch (parseToken(BufferStream.next(buffer))) {
    /* emit value tokens inside compound value */
    | Some(Word(str)) => Value(str)
    | Some(Interpolation(x)) => ValueRef(x)

    /* if a comma is encountered, drop back into the ValueLoop */
    | Some(Comma) => {
      state.mode = ValueLoop;
      CompoundValueEnd
    }

    /* if a semicolon or closing brace is encountered, escape back into the MainLoop */
    | Some(Semicolon)
    | Some(Brace(Closing)) => {
      state.mode = MainLoop;
      CompoundValueEnd
    }

    | _ => {
      raise(LazyStream.Error(
        "Unexpected token while parsing a compound value, expected a Word or Interpolation"
      ))
    }
    }
  };

  let valueLoop = () : node => {
    let token = BufferStream.next(buffer);

    switch (parseToken(token), parseToken(BufferStream.peek(buffer))) {
    | (Some(Interpolation(_)), Some(Interpolation(_)))
    | (Some(Interpolation(_)), Some(Word(_)))
    | (Some(Word(_)), Some(Interpolation(_)))
    | (Some(Word(_)), Some(Word(_))) => {
      BufferStream.putOption(token, buffer); /* put token back onto the buffer */
      state.mode = CompoundValueMode;
      CompoundValueStart
    }

    /* emit an interpolation value and continue parsing values in ValueLoop mode */
    | (Some(value), Some(Comma)) => {
      BufferStream.junk(buffer); /* skip over comma token */
      parseValueOrInterpolation(value)
    }

    /* emit an interpolation or value and switch back to MainLoop mode */
    | (Some(value), Some(Semicolon)) => {
      BufferStream.junk(buffer); /* skip over semicolon token */
      state.mode = MainLoop;
      parseValueOrInterpolation(value)
    }

    /* emit a value or interpolation, keep closing brace around, and switch back to MainLoop mode */
    | (Some(value), Some(Brace(Closing))) => {
      state.mode = MainLoop;
      parseValueOrInterpolation(value)
    }

    | _ => {
      raise(LazyStream.Error(
        "Unexpected token while parsing a value, expected a Word or Interpolation"
      ))
    }
    }
  };

  /* buffers all tokens until a semicolon or brace is reached to determine whether
     a declaration or selector should be parsed */
  let rec parseDeclOrSelector = () : node => {
    let token = LazyStream.peek(s);

    switch (parseToken(token)) {
    /* if an opening curly brace is reached, the selector loop should be triggered */
    | Some(Brace(Opening)) => {
      state.mode = SelectorLoop;
      selectorLoop()
    }

    /* if a semicolon or closing curly brace is reached, the property loop should be triggered */
    | Some(Brace(Closing))
    | Some(Semicolon) => {
      state.mode = PropertyLoop;
      propertyLoop()
    }

    /* TODO: bail if tokens are not selector/declaration tokens to prevent unnecessary buffering */

    /* buffer any other token while skipping it in the LazyStream */
    | Some(_) => {
      BufferStream.bufferOption(token, buffer);
      LazyStream.junk(s);
      parseDeclOrSelector()
    }

    | None => raise(LazyStream.Error("Unexpected EOF, expected selector or declaration"))
    }
  };

  let rec mainLoop = () : node => {
    let firstToken = LazyStream.next(s);

    switch (parseToken(firstToken), parseToken(LazyStream.peek(s))) {
    /* skip over free semicolons */
    | (Some(Semicolon), _) => mainLoop()
    | (Some(Word(_)), Some(Colon))
    | (Some(Interpolation(_)), Some(Colon)) => {
      /* buffer first token for future decl/selector parsing */
      BufferStream.bufferOption(firstToken, buffer);
      parseDeclOrSelector()
    }

    | (Some(AtWord(_)), _) => {
      /* buffer first token for future at-rule parsing */
      BufferStream.bufferOption(firstToken, buffer);
      RuleEnd /* TODO: parse at-rule */
    }

    /* increase nestedness and start SelectorLoop when opening curly brace is encountered */
    | (Some(t), _) when isSelectorToken(t) => {
      /* buffer first token for future selector parsing */
      BufferStream.bufferOption(firstToken, buffer);

      state.nestedness = state.nestedness + 1;
      state.mode = SelectorLoop;
      selectorLoop()
    }

    /* decrease nestedness when closing curly brace is encountered */
    | (Some(Brace(Closing)), _) when state.nestedness > 0 => {
      state.nestedness = state.nestedness - 1;
      RuleEnd
    }

    | (Some(_), _) => {
      raise(LazyStream.Error("Unexpected token; expected selector, declaration, or at-rule"))
    }

    /* EOF is only allowed when all rules have been closed with closing curly braces */
    | (None, _) when state.nestedness > 0 => {
      let msg = "Unexpected EOF, expected " ++ (string_of_int(state.nestedness) ++ " rule(s) to be closed");
      raise(LazyStream.Error(msg))
    }

    | (None, _) => EOF
    }
  };

  let next: [@bs] (unit => option(node)) = [@bs] (() => {
    let node =
      switch state.mode {
      | MainLoop => mainLoop()
      | PropertyLoop => propertyLoop()
      | ValueLoop => valueLoop()
      | CompoundValueMode => compoundValueLoop()
      | SelectorLoop => selectorLoop()
      };

    switch node {
    | EOF => None
    | value => {
      /* store node as "last node" */
      state.lastNode = value;
      Some(value)
    }
    }
  });

  LazyStream.from(next)
};
