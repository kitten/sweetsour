open Common;

/* an error raised by the lexer contains a message and a row/column location */
exception LexerError(string, (int, int));

type pairwiseKind =
  | Opening
  | Closing;

type quoteKind =
  | Double
  | Single;

type tokenValue =
  | Interpolation(interpolation)
  | Paren(pairwiseKind)
  | Bracket(pairwiseKind)
  | Brace(pairwiseKind)
  | Word(string)
  | AtWord(string)
  | Str(string)
  | Quote(quoteKind)
  | Exclamation
  | Equal
  | Colon
  | Semicolon
  | Plus
  | Ampersand
  | Arrow
  | Asterisk
  | Tilde
  | Comma
  | Pipe
  | Dollar
  | Caret
  | EOF;

type token = Token(tokenValue, (int, int), (int, int));

type lexerStream = LazyStream.t(token);

type argumentMode =
  | StringArg
  | ContentArg;

type lexerMode =
  | MainLoop
  | StringEndLoop
  | SingleQuoteStringLoop
  | DoubleQuoteStringLoop
  | UnquotedStringArgStartLoop
  | UnquotedStringArgLoop
  | UnquotedStringArgEndLoop
  | UnquotedContentArgLoop;

type state = {
  /* location variable to keep track of (row, line) */
  mutable loc: (int, int),
  /* location variable to keep track of the potential end of a token */
  mutable endLoc: option((int, int)),
  /* location variable to mark the potential start of a token */
  mutable startLoc: option((int, int)),
  /* instructs the sideeffect stream to set the startLoc on the next char */
  mutable shouldSetStartLoc: bool,
  /* a buffer holding the last emitted tokenValue */
  mutable lastTokenValue: tokenValue,
  /* the current mode of the lexer */
  mutable mode: lexerMode
};

let lexer = (input: Input.inputStream) => {
  let state = {
    loc: (1, 0),
    startLoc: None,
    endLoc: None,
    shouldSetStartLoc: false,
    lastTokenValue: EOF,
    mode: MainLoop
  };

  /* get the current endLoc */
  let getEndLocation = () => state.loc;

  /* sets the current start/end location at loc */
  let markStartLocation = () => state.startLoc = Some(state.loc);
  let markEndLocation = () => state.endLoc = Some(state.loc);

  /* count up the current column on endLoc */
  let nextColumn = () => {
    let (row, column) = state.loc;
    state.loc = (row, column + 1);
  };

  /* count up the current row on endLoc */
  let nextRow = () => {
    let (row, _) = state.loc;
    state.loc = (row + 1, 0);
  };

  /* wrap inputStream in row/column counting sideeffect */
  let s = input |> LazyStream.withSideeffect([@bs] (x: Input.inputValue) => {
    switch x {
    | Char('\n') => nextRow()
    | _ => nextColumn()
    };

    if (state.shouldSetStartLoc) {
      markStartLocation();
      state.shouldSetStartLoc = false;
    };
  });

  /* checks whether next input value is equal to char */
  let isNextCharEqual = (matching: char) => {
    switch (LazyStream.peek(s)) {
    | Some(Char(c)) when c === matching => true
    | _ => false
    }
  };

  /* recognise all chars that are valid starts of a word */
  let isWordStartChar = (c: char) => {
    switch c {
    | 'a'..'z'
    | 'A'..'Z'
    | '0'..'9'
    | '#'
    | '.' => true
    | '-' => true
    | _ => false
    }
  };

  /* recognise all chars that are valid hex digits */
  let isHexDigitChar = (c: char) => {
    switch c {
    | 'a'..'f'
    | 'A'..'F'
    | '0'..'9' => true
    | _ => false
    }
  };

  /* skip over all chars until end of comment is reached */
  let rec skipCommentContent = () => {
    switch (LazyStream.next(s)) {
    /* end a comment on asterisk + slash */
    | Some(Char('*')) when isNextCharEqual('/') => {
      LazyStream.junk(s) /* throw away the trailing slash */
    }

    /* any char is recursively ignored */
    | Some(_) => skipCommentContent()

    /* a comment should be closed, since we don't want to deal with input weirdness */
    | None => raise(LexerError(unexpected_msg("eof", "comment") ++ expected_msg("comment to be closed"), getEndLocation()))
    }
  };

  /* skip all whitespace-like characters */
  let rec skipWhitespaces = () => {
    switch (LazyStream.peek(s)) {
    | Some(Char('\n'))
    | Some(Char(' '))
    | Some(Char('\t'))
    | Some(Char('\r')) => {
      LazyStream.junk(s); /* skip over whitespace-like char */
      skipWhitespaces()
    }

    /* end sequence on all other characters */
    | _ => ()
    }
  };

  /* captures any hex code of 1-n digits (spec-compliancy is 1-6), assuming that the code's start is in `str` */
  let rec captureHexDigits = (str: string) : string => {
    switch (LazyStream.peek(s)) {
    /* recursively capture all hex code characters */
    | Some(Char(c)) when isHexDigitChar(c) => {
      LazyStream.junk(s);
      captureHexDigits(str ++ string_of_char(c))
    }

    /* hex code escapes are optionally followed by an extra whitespace */
    | Some(Char(' ')) => {
      LazyStream.junk(s);
      str ++ " "
    }

    /* non-matched characters end the hex sequence */
    | Some(_)
    | None => str
    }
  };

  /* captures the content of an escape, assuming a backslash preceded it */
  let captureEscapedContent = () : string => {
    let escaped =
      switch (LazyStream.next(s)) {
      /* detect start of a hex code as those have a different escape syntax */
      | Some(Char(c)) when isHexDigitChar(c) => captureHexDigits(string_of_char(c))

      /* capture a single non-hex char as the escaped content (spec-compliancy disallows newlines, but they are supported in practice) */
      | Some(Char(c)) => string_of_char(c)

      /* it's too risky to allow value-interpolations as part of escapes */
      | Some(Interpolation(_)) => {
        raise(LexerError(unexpected_msg("interpolation", "escape sequence"), getEndLocation()));
      }

      /* an escape (backslash) must be followed by at least another char, thus an EOF is unacceptable */
      | None => raise(LexerError(unexpected_msg("eof", "escape sequence"), getEndLocation()))
      };

    "\\" ++ escaped
  };

  /* captures the content of a word, assuming that the word's start is captured in `str` */
  let rec captureWordContent = (str: string) : string => {
    markEndLocation(); /* mark potential end location for word token */

    switch (LazyStream.peek(s)) {
    /* escaped content is allowed anywhere inside a word */
    | Some(Char('\\')) => {
      LazyStream.junk(s);
      captureWordContent(str ++ captureEscapedContent())
    }

    /* the following ranges of chars are part of the word's tail */
    | Some(Char('%' as c))
    | Some(Char('-' as c))
    | Some(Char('_' as c)) => {
      LazyStream.junk(s);
      captureWordContent(str ++ string_of_char(c))
    }

    | Some(Char(c)) when isWordStartChar(c) => {
      LazyStream.junk(s);
      captureWordContent(str ++ string_of_char(c))
    }

    /* all non-word characters end the sequence */
    | Some(_)
    | None => str
    }
  };

  /* adds a double quote to the beginning of an unquoted url() argument */
  let unquotedStringArgStartLoop = () : tokenValue => {
    /* mark location for double quote token */
    markStartLocation();
    markEndLocation();

    state.mode = UnquotedStringArgLoop;

    Quote(Double)
  };

  /* adds a double quote to the end of an unquoted url() argument */
  let unquotedStringArgEndLoop = () : tokenValue => {
    /* mark location for double quote token */
    markStartLocation();
    markEndLocation();

    state.mode = MainLoop;
    Quote(Double)
  };

  /* tokenise unquoted arguments like the content of url() or calc() */
  let rec unquotedArgumentLoop = (kind: argumentMode, nestedness: int, str: string) : tokenValue => {
    if (str == "") {
      markStartLocation(); /* mark start location when str is not set yet */
    };

    markEndLocation(); /* mark potential end location for word token */

    switch (LazyStream.peek(s), kind, nestedness) {
    /* escaped content is allowed anywhere inside an unquoted argument */
    | (Some(Char('\\')), _, _) => {
      LazyStream.junk(s); /* throw away leading backslash */
      unquotedArgumentLoop(kind, nestedness, str ++ captureEscapedContent())
    }

    /* for url(), exit UnquotedArgumentLoop when closing parenthesis is found and enter UnquotedStringArgEndLoop */
    | (Some(Char(')')), StringArg, 0) => {
      if (str !== "") {
        state.mode = UnquotedStringArgEndLoop;
        Str(str)
      } else {
        unquotedStringArgEndLoop()
      }
    }

    /* for calc(), exit UnquotedArgumentLoop when closing parenthesis is found and nestedness is 0 */
    | (Some(Char(')')), ContentArg, 0) => {
      state.mode = MainLoop;
      Str(str)
    }

    /* count nestedness down when closing parenthesis is found */
    | (Some(Char(')')), ContentArg, _) => {
      LazyStream.junk(s); /* throw away closing parenthesis */
      unquotedArgumentLoop(kind, nestedness - 1, str ++ ")")
    }

    /* nested arguments (parentheses) inside StrictString arguments are not allowed */
    | (Some(Char('(')), StringArg, _) => {
      raise(LexerError(unexpected_msg("'('", "unquoted argument"), getEndLocation()));
    }

    | (Some(Char('(')), ContentArg, _) => {
      LazyStream.junk(s); /* throw away opening parenthesis */
      unquotedArgumentLoop(kind, nestedness + 1, str ++ "(")
    }

    /* in url() whitespaces can only appear at the end of a StrictString unquoted argument */
    | (Some(Char(' ')), StringArg, _)
    | (Some(Char('\t')), StringArg, _)
    | (Some(Char('\r')), StringArg, _)
    | (Some(Char('\n')), StringArg, _) => {
      skipWhitespaces();

      switch (LazyStream.peek(s)) {
      | Some(Char(')')) when str !== "" => {
        state.mode = UnquotedStringArgEndLoop;
        Str(str)
      }

      | Some(Char(')')) when str === "" => unquotedStringArgEndLoop()

      | _ => raise(LexerError(unexpected_msg("whitespace", "unquoted argument") ++ expected_msg("')'"), getEndLocation()))
      }
    }

    /* in url() quotes must be escaped, since extra quotes will be added */
    | (Some(Char('"')), StringArg, _) => {
      LazyStream.junk(s); /* throw away char */
      unquotedArgumentLoop(kind, nestedness, str ++ "\\\"")
    }

    /* every char is accepted into the unquoted argument */
    | (Some(Char(c)), _, _) => {
      LazyStream.junk(s); /* throw away char */
      unquotedArgumentLoop(kind, nestedness, str ++ string_of_char(c))
    }

    /* emit string and wait for next loop when encountering an interpolation during an unquoted argument... */
    | (Some(Interpolation(_)), _, _) when str !== "" => Str(str)

    /* ...but when the string is empty (next loop) we emit the interpolation */
    | (Some(Interpolation(value)), _, _) => {
      LazyStream.junk(s); /* throw away the interpolation */
      state.shouldSetStartLoc = true; /* capture next-char's location as the start of the next string */
      Interpolation(value)
    }

    /* an unquoted argument must be explicitly ended, thus an EOF is unacceptable */
    | (None, _, _) => raise(LexerError(unexpected_msg("eof", "unquoted argument") ++ expected_msg("')'"), getEndLocation()))
    }
  };

  /* tokenise a string (excluding quotes which are separate tokens) */
  let rec stringLoop = (kind: quoteKind, str: string) : tokenValue => {
    markEndLocation(); /* mark potential end location for word token */

    switch (LazyStream.peek(s)) {
    /* escaped content is allowed anywhere inside a string */
    | Some(Char('\\')) => {
      LazyStream.junk(s); /* throw away leading backslash */
      stringLoop(kind, str ++ captureEscapedContent())
    }

    /* exit StringLoop when ending quote is found */
    | Some(Char('\'')) when kind === Single => {
      state.mode = StringEndLoop;
      Str(str)
    }

    | Some(Char('"')) when kind === Double => {
      state.mode = StringEndLoop;
      Str(str)
    }

    /* newlines inside strings are not permitted, except when they're escaped */
    | Some(Char('\n')) => raise(LexerError(unexpected_msg("newline", "string"), getEndLocation()))

    /* every char is accepted into the string */
    | Some(Char(c)) => {
      LazyStream.junk(s); /* throw away char */
      stringLoop(kind, str ++ string_of_char(c))
    }

    /* emit string and wait for next loop when encountering an interpolation during a string... */
    | Some(Interpolation(_)) when str !== "" => Str(str)

    /* ...but when the string is empty (next loop) we emit the interpolation */
    | Some(Interpolation(value)) => {
      LazyStream.junk(s); /* throw away the interpolation */
      state.shouldSetStartLoc = true; /* capture next-char's location as the start of the next string */
      Interpolation(value)
    }

    /* a string must be explicitly ended, thus an EOF is unacceptable */
    | None => raise(LexerError(unexpected_msg("eof", "string"), getEndLocation()))
    }
  };

  /* capture the quote that comes after a string's end (quote has already been detected) */
  let stringEndLoop = () : tokenValue => {
    state.mode = MainLoop;

    switch (LazyStream.next(s)) {
    | Some(Char('"')) => Quote(Double)
    | Some(Char('\'')) => Quote(Single)
    | _ => raise(LexerError(unexpected_msg("token", "string") ++ expected_msg("end of string"), getEndLocation()))
    }
  };

  let rec mainLoop = () : tokenValue => {
    switch (LazyStream.next(s)) {
    /* single char tokens */
    | Some(Char('{')) => Brace(Opening)
    | Some(Char('}')) => Brace(Closing)
    | Some(Char('[')) => Bracket(Opening)
    | Some(Char(']')) => Bracket(Closing)
    | Some(Char('!')) => Exclamation
    | Some(Char('=')) => Equal
    | Some(Char(':')) => Colon
    | Some(Char(';')) => Semicolon
    | Some(Char('+')) => Plus
    | Some(Char('>')) => Arrow
    | Some(Char('~')) => Tilde
    | Some(Char(',')) => Comma
    | Some(Char('|')) => Pipe
    | Some(Char('$')) => Dollar
    | Some(Char('*')) => Asterisk
    | Some(Char('&')) => Ampersand
    | Some(Char('^')) => Caret

    /* single char token; closing parenthesis, see below for opening */
    | Some(Char(')')) => Paren(Closing)

    /* detect whether parenthesis is part of url() or calc(), and emit (non-/)separated parenthesis */
    | Some(Char('(')) => {
      switch state.lastTokenValue {
      | Word("url") => {
        /* mark location for opening paren token */
        markStartLocation();
        markEndLocation();

        skipWhitespaces(); /* skip all leading whitespaces */

        state.mode = UnquotedStringArgStartLoop;
        Paren(Opening)
      }
      | Word("calc") => {
        state.mode = UnquotedContentArgLoop;
        Paren(Opening)
      }

      | _ => Paren(Opening)
      }
    }

    /* skip over carriage returns, tabs, newlines, and whitespaces */
    | Some(Char('\r'))
    | Some(Char('\t'))
    | Some(Char('\n'))
    | Some(Char(' ')) => mainLoop()

    /* detect double quotes and queue StringLoop */
    | Some(Char('"')) => {
      state.mode = DoubleQuoteStringLoop;
      state.shouldSetStartLoc = true; /* capture next-char's location as the start of the string */
      Quote(Double)
    }

    /* detect single quotes and queue StringLoop */
    | Some(Char('\'')) => {
      state.mode = SingleQuoteStringLoop;
      state.shouldSetStartLoc = true; /* capture next-char's location as the start of the string */
      Quote(Single)
    }

    /* detect at-words and parse it like a normal word afterwards */
    | Some(Char('@')) => {
      markStartLocation(); /* mark start of AtWord */

      /* at-words will never be composed with interpolations, so no switch to WordLoop */
      let wordContent = captureWordContent("@");
      AtWord(wordContent)
    }

    /* detect start of a word and parse it */
    | Some(Char(c)) when isWordStartChar(c) => {
      markStartLocation(); /* mark start of Word */

      /* capture rest of word */
      let wordContent = captureWordContent(string_of_char(c));
      Word(wordContent)
    }

    /* detect backslash i.e. escape as start of a word and parse it */
    | Some(Char('\\')) => {
      markStartLocation(); /* mark start of Word */

      /* capture escaped content and rest of word */
      let wordContent = captureWordContent(captureEscapedContent());
      Word(wordContent)
    }

    /* pass-through interpolation */
    | Some(Interpolation(x)) => Interpolation(x)

    /* detect and skip comments, then search next tokenValue */
    | Some(Char('/')) when isNextCharEqual('*') => {
      LazyStream.junk(s); /* throw away the leading asterisk */

      skipCommentContent();
      mainLoop()
    }

    /* all unrecognised characters will be raised outside of designated matching loops */
    | Some(Char(c)) => {
      raise(LexerError(unexpected_msg("'" ++ string_of_char(c) ++ "'", "tokens"), getEndLocation()));
    }

    | None => EOF
    }
  };

  /* next function needs to be defined as uncurried and arity-0 at its definition */
  let next: [@bs] (unit => option(token)) = [@bs](() => {
    let token =
      switch state.mode {
      | MainLoop => mainLoop()
      | SingleQuoteStringLoop => stringLoop(Single, "")
      | DoubleQuoteStringLoop => stringLoop(Double, "")
      | StringEndLoop => stringEndLoop()
      | UnquotedStringArgStartLoop => unquotedStringArgStartLoop()
      | UnquotedStringArgLoop => unquotedArgumentLoop(StringArg, 0, "")
      | UnquotedStringArgEndLoop => unquotedStringArgEndLoop()
      | UnquotedContentArgLoop => unquotedArgumentLoop(ContentArg, 0, "")
      };

    switch token {
    | EOF => None /* special token to signalise the end of the stream */
    | value => {
      /* store token as "last token" for unquote argument detection */
      state.lastTokenValue = value;

      /* fallback to endLoc and then loc */
      let startLoc = switch (state.startLoc, state.endLoc) {
        | (None, None) => state.loc
        | (None, Some(loc)) => loc
        | (Some(loc), _) => loc
      };

      /* fallback to loc */
      let endLoc = switch (state.endLoc) {
        | None => state.loc
        | Some(loc) => loc
      };

      /* reset start/end location markers */
      state.startLoc = None;
      state.endLoc = None;

      Some(Token(value, startLoc, endLoc))
    }
    }
  });

  LazyStream.from(next)
};
