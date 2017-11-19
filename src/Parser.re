open Common;

/* an error raised by the parser contains a message and a line number */
exception ParserError(string, int);

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
  | StringStart(string)
  | StringEnd
  | EOF;

/* Stream type for the ParserStream */
type parserStream = LazyStream.t(node);

/* Modes the parser can be in, allowing encapsulated and specialised logic */
type parserMode =
  | MainLoop
  | PropertyLoop
  | BufferLoop(LinkedList.t(node))
  | SelectorLoop;

/* Running state for parsing */
type state = {
  /* value to keep track of the current line number; must be updated for every incoming lexer token */
  mutable line: int,
  /* value to keep track of the current rule nesting */
  mutable ruleLevel: int,
  /* the current mode of the parser */
  mutable mode: parserMode
};

let parser = (s: Lexer.lexerStream) => {
  /* a buffer stream emitting tokens combined with an internal buffer */
  let buffer = BufferStream.from(s);

  let state = {
    line: 1,
    ruleLevel: 0,
    mode: MainLoop
  };

  let getTokenValue = (t: option(Lexer.token)) : option(Lexer.tokenValue) => {
    switch t {
    | Some(Token(value, line)) => {
      state.line = line;
      Some(value)
    }
    | None => None
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

  /* parses all value nodes recursively, including functions and compound values,
     and returns the resulting node buffer */
  let parseValues = () : LinkedList.t(node) => {
    /* wraps node buffer in compound value nodes when more than one value was parsed (length > 1) */
    let wrapBufferAsCompound = (nodeBuffer: LinkedList.t(node), length: int) => {
      switch (length) {
        | 1 => nodeBuffer
        | _ => {
          LinkedList.unshift(CompoundValueStart, nodeBuffer);
          LinkedList.add(CompoundValueEnd, nodeBuffer);
          nodeBuffer
        }
      }
    };

    /* wraps node buffer in function nodes using the passed function name (fnName) */
    let wrapBufferAsFunction = (nodeBuffer: LinkedList.t(node), fnName: string) => {
      LinkedList.unshift(FunctionStart(fnName), nodeBuffer);
      LinkedList.add(FunctionEnd, nodeBuffer);
      nodeBuffer
    };

    let addValueNode = (str: string, nodeBuffer: LinkedList.t(node)) => {
      switch (str) {
      | "" => nodeBuffer
      | _ => {
        LinkedList.add(Value(str), nodeBuffer);
        nodeBuffer
      }
      }
    };

    let parseString = (kind: Lexer.quoteKind) : LinkedList.t(node) => {
      let nodeBuffer = LinkedList.create();
      let quoteStr = switch (kind) {
        | Double => "\""
        | Single => "'"
      };

      let rec parse = (str: string, containsInterpolation: bool) => {
        switch (getTokenValue(BufferStream.next(buffer))) {
        | Some(Interpolation(x)) => {
          LinkedList.add(ValueRef(x), addValueNode(str, nodeBuffer));
          parse("", true)
        }

        | Some(Quote(endKind)) when endKind === kind => {
          if (nodeBuffer.size > 1 || containsInterpolation) {
            LinkedList.unshift(StringStart(quoteStr), addValueNode(str, nodeBuffer));
            LinkedList.add(StringEnd, nodeBuffer);
            nodeBuffer
          } else {
            addValueNode(quoteStr ++ str ++ quoteStr, nodeBuffer)
          }
        }

        | Some(Str(rest)) => parse(str ++ rest, containsInterpolation)

        | _ => raise(ParserError("Unexpected token while parsing string", state.line))
        }
      };

      parse("", false)
    };

    /* recursively parse all values by dividing the stream into functions, compounds, and lastly values */
    let rec parseValueLevel = (nodeBuffer: LinkedList.t(node), length: int, level: int) => {
      switch (getTokenValue(BufferStream.peek(buffer))) {
      | Some(Word(word)) => {
        BufferStream.junk(buffer);

        switch (getTokenValue(BufferStream.peek(buffer))) {
          | Some(Paren(Opening)) => {
            BufferStream.junk(buffer);

            let innerValues = wrapBufferAsFunction(
              parseValueLevel(LinkedList.create(), 0, level + 1),
              word
            );

            parseValueLevel(LinkedList.concat(nodeBuffer, innerValues), length + 1, level)
          }

          | _ => {
            LinkedList.add(Value(word), nodeBuffer);
            parseValueLevel(nodeBuffer, length + 1, level)
          }
        }
      }

      | Some(Quote(kind)) => {
        BufferStream.junk(buffer);
        let innerValues = parseString(kind);
        parseValueLevel(LinkedList.concat(nodeBuffer, innerValues), length + 1, level)
      }

      | Some(Str(str)) when level > 0 => {
        LinkedList.add(Value(str), nodeBuffer);
        parseValueLevel(nodeBuffer, length + 1, level)
      }

      | Some(Interpolation(x)) => {
        BufferStream.junk(buffer);
        LinkedList.add(ValueRef(x), nodeBuffer);
        parseValueLevel(nodeBuffer, length + 1, level)
      }

      | Some(Comma) => {
        BufferStream.junk(buffer);
        let first = wrapBufferAsCompound(nodeBuffer, length);
        let second = parseValueLevel(LinkedList.create(), 0, level);
        LinkedList.concat(first, second)
      }

      | Some(Paren(Closing)) when level > 0 => {
        BufferStream.junk(buffer);
        wrapBufferAsCompound(nodeBuffer, length)
      }

      | None
      | Some(Brace(Closing))
      | Some(Semicolon) when level === 0 => {
        wrapBufferAsCompound(nodeBuffer, length)
      }

      | _ => raise(ParserError("Unexpected token while parsing values", state.line))
      }
    };

    parseValueLevel(LinkedList.create(), 0, 0);
  };

  let selectorLoop = () : node => {
    RuleStart(StyleRule) /* TODO: parse selector */
  };

  let propertyLoop = () : node => {
    /* emit node for word or interpolation property */
    let node =
      switch (getTokenValue(BufferStream.next(buffer))) {
      | Some(Word(str)) => Property(str)
      | Some(Interpolation(x)) => PropertyRef(x)
      | _ => {
        raise(ParserError(
          "Unexpected token while parsing a property, expected a Word or Interpolation",
          state.line
        ))
      }
      };

    /* enforce a colon token after a property */
    switch (getTokenValue(BufferStream.next(buffer))) {
    | Some(Colon) => ()
    | _ => raise(ParserError("Unexpected token after parsing a property, expected a Colon", state.line))
    };

    /* preparse values and start the buffer loop to consume & emit them */
    state.mode = BufferLoop(parseValues());

    node
  };

  /* buffers all tokens until a semicolon or brace is reached to determine whether
     a declaration or selector should be parsed */
  let rec parseDeclOrSelector = () : node => {
    let token = LazyStream.peek(s);

    switch (getTokenValue(token)) {
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

    | None => raise(ParserError("Unexpected EOF, expected selector or declaration", state.line))
    }
  };

  let rec mainLoop = () : node => {
    let firstToken = LazyStream.next(s);

    switch (getTokenValue(firstToken), getTokenValue(LazyStream.peek(s))) {
    /* skip over free semicolons */
    | (Some(Semicolon), _) => mainLoop()

    /* enter declaration or selector parser */
    | (Some(Word(_)), Some(Colon))
    | (Some(Interpolation(_)), Some(Colon)) => {
      /* buffer first token for future decl/selector parsing */
      BufferStream.bufferOption(firstToken, buffer);
      parseDeclOrSelector()
    }

    /* parse at-rules */
    | (Some(AtWord(_)), _) => {
      /* buffer first token for future at-rule parsing */
      BufferStream.bufferOption(firstToken, buffer);
      RuleEnd /* TODO: parse at-rule */
    }

    /* increase ruleLevel and start SelectorLoop when opening curly brace is encountered */
    | (Some(t), _) when isSelectorToken(t) => {
      /* buffer first token for future selector parsing */
      BufferStream.bufferOption(firstToken, buffer);

      state.ruleLevel = state.ruleLevel + 1;
      state.mode = SelectorLoop;
      selectorLoop()
    }

    /* decrease ruleLevel when closing curly brace is encountered */
    | (Some(Brace(Closing)), _) when state.ruleLevel > 0 => {
      state.ruleLevel = state.ruleLevel - 1;
      RuleEnd
    }

    | (Some(_), _) => {
      raise(ParserError("Unexpected token; expected selector, declaration, or at-rule", state.line))
    }

    /* EOF is only allowed when all rules have been closed with closing curly braces */
    | (None, _) when state.ruleLevel > 0 => {
      raise(ParserError("Unexpected EOF, expected all rules to be closed", state.line))
    }

    | (None, _) => EOF
    }
  };

  /* emits nodes from a preparsed buffer */
  let bufferLoop = (nodes: LinkedList.t(node)) : node => {
    switch (LinkedList.take(nodes)) {
      | None => {
        state.mode = MainLoop;
        mainLoop()
      }
      | Some(node) => node
    }
  };

  let next: [@bs] (unit => option(node)) = [@bs] (() => {
    let node =
      switch state.mode {
      | MainLoop => mainLoop()
      | PropertyLoop => propertyLoop()
      | BufferLoop(nodes) => bufferLoop(nodes)
      | SelectorLoop => selectorLoop()
      };

    switch node {
    | EOF => None
    | value => Some(value)
    }
  });

  LazyStream.from(next)
};
