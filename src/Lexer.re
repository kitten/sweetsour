open Common;

/* For tokens that are opening or closing the difference can be abstracted w/o their value */
type pairwiseKind =
  | Opening
  | Closing;

/* A token's value represented by its type constructor and the value type */
type tokenValue =
  | Interpolation interpolation
  | Paren pairwiseKind
  | Brace pairwiseKind
  | Bracket pairwiseKind
  | Word string
  | AtWord string
  | Str string
  | Equal
  | Colon
  | Semicolon
  | Plus
  | Ampersand
  | Arrow
  | Asterisk
  | Tilde
  | Comma;

/* A token represented by its value and a line number */
type token = Token tokenValue int;

type lexerStream = LazyStream.t token;

let lexer = fun (s: Input.inputStream) => {
  /* ref value to keep track of the current line number; must be updated for every newline */
  let line = ref 1;

  /* a buffer holding the last emitted tokenValue */
  let lastTokenValue: ref (option tokenValue) = ref None;

  /* a buffer holding tokenValues to compute some tokens ahead of time */
  let tokenValueBuffer: ref (list tokenValue) = ref [];

  /* skip over all chars until end of comment is reached */
  let rec skipCommentContent () => {
    switch (LazyStream.next s) {
      /* end a comment on asterisk + slash */
      | Char '*' when (LazyStream.peek s == Some (Char '/')) => {
        LazyStream.junk s; /* throw away the trailing slash */
      }

      /* count up current line number inside comments */
      | Char '\n' => {
        line := !line + 1;
        skipCommentContent ()
      }

      /* any char is recursively ignored */
      | _ => skipCommentContent ()

      /* a comment should be closed, since we don't want to deal with input weirdness */
      | exception LazyStream.Failure => {
        raise (LazyStream.Error "Unexpected EOF, expected end of comment")
      }
    };
  };

  /* skip all whitespace-like characters */
  let rec skipWhitespaces () => {
    switch (LazyStream.peek s) {
      /* count up current line number */
      | Some (Char '\n') => {
        LazyStream.junk s; /* skip over newline */
        line := !line + 1;
        skipWhitespaces ()
      }

      | Some (Char ' ')
      | Some (Char '\r') => {
        LazyStream.junk s; /* skip over whitespace-like char */
        skipWhitespaces ()
      }

      /* end sequence on all other characters */
      | _ => ()
    }
  };

  /* captures any hex code of 1-n digits (spec-compliancy is 1-6), assuming that the code's start is in `str` */
  let rec captureHexDigits (str: string): string => {
    switch (LazyStream.peek s) {
      /* recursively capture all hex code characters */
      | Some (Char ('a'..'f' as c))
      | Some (Char ('A'..'F' as c))
      | Some (Char ('0'..'9' as c)) => {
        LazyStream.junk s;
        captureHexDigits (str ^ (String.make 1 c))
      }

      /* hex code escapes are optionally followed by an extra whitespace */
      | Some (Char ' ') => {
        LazyStream.junk s;
        str ^ " "
      }

      /* non-matched characters end the hex sequence */
      | Some _
      | None => str
    }
  };

  /* captures the content of an escape, assuming a backslash preceded it */
  let captureEscapedContent (): string => {
    let escaped = switch (LazyStream.next s) {
      /* detect start of a hex code as those have a different escape syntax */
      | Char ('A'..'F' as c)
      | Char ('a'..'f' as c)
      | Char ('0'..'9' as c) => {
        captureHexDigits (String.make 1 c)
      }

      /* count up current line number even for escaped newlines */
      | Char '\n' => {
        line := !line + 1;
        "\n"
      }

      /* capture a single non-hex char as the escaped content (spec-compliancy disallows newlines, but they are supported in practice) */
      | Char c => String.make 1 c

      /* it's too risky to allow value-interpolations as part of escapes */
      | Interpolation _ => {
        raise (LazyStream.Error "Unexpected interpolation after backslash, expected escaped content")
      }

      /* an escape (backslash) must be followed by at least another char, thus an EOF is unacceptable */
      | exception LazyStream.Failure => {
        raise (LazyStream.Error "Unexpected EOF after backslash, expected escaped content")
      }
    };

    "\\" ^ escaped
  };

  /* captures the content of a string, `quote` being the terminating character */
  let rec captureStringContent (quote: char) (str: string): string => {
    switch (LazyStream.next s) {
      /* escaped content is allowed anywhere inside a string */
      | Char '\\' => {
        captureStringContent quote (str ^ (captureEscapedContent ()))
      }

      /* every char is accepted into the string */
      | Char c => {
        /* check if end of string is reached or continue */
        if (c === quote) {
          str
        } else {
          captureStringContent quote (str ^ (String.make 1 c))
        }
      }

      /* we only have a value-interpolation, which is not the same as an interpolation inside a string */
      /* the only sane thing to do is to disallow interpolations inside strings so that the user replaces the entire string */
      | Interpolation _ => {
        raise (LazyStream.Error "Unexpected interpolation inside a string")
      }

      /* a string must be explicitly ended, thus an EOF is unacceptable */
      | exception LazyStream.Failure => {
        raise (LazyStream.Error "Unexpected EOF before end of string")
      }
    }
  };

  let bufferURLContent () => {
    skipWhitespaces (); /* Skip all whitespaces */

    switch (LazyStream.peek s) {
      /* capture normal strings inside url() argument (closing parenthesis handled by main loop) */
      | Some (Char ('\"' as c))
      | Some (Char ('\'' as c)) => {
        LazyStream.junk s; /* throw away the leading quote */

        let urlContent = captureStringContent c "";
        tokenValueBuffer := [Str urlContent, ...!tokenValueBuffer];
      }

      | Some (Char _) => {
        /* capture contents of url() argument until closing paren, and trim result */
        let urlContent = String.trim (captureStringContent ')' "");

        /* add closing paren that captureStringContent skipped over (reverse order) */
        tokenValueBuffer := [Paren Closing, ...!tokenValueBuffer];
        tokenValueBuffer := [Str urlContent, ...!tokenValueBuffer];
      }

      /* ignore all other cases as they're either handled by the main loop or the main parsing stage */
      | _ => ()
    }
  };

  /* captures the content of a word, assuming that the word's start is captured in `str` */
  let rec captureWordContent (str: string): string => {
    switch (LazyStream.peek s) {
      /* escaped content is allowed anywhere inside a word */
      | Some (Char '\\') => {
        LazyStream.junk s;
        captureWordContent (str ^ (captureEscapedContent ()))
      }

      /* the following ranges of chars are part of the word's tail */
      | Some (Char ('a'..'z' as c))
      | Some (Char ('A'..'Z' as c))
      | Some (Char ('0'..'9' as c))
      | Some (Char ('#' as c))
      | Some (Char ('.' as c))
      | Some (Char ('%' as c))
      | Some (Char ('-' as c))
      | Some (Char ('_' as c)) => {
        LazyStream.junk s;
        captureWordContent (str ^ (String.make 1 c))
      }

      /* all non-word characters end the sequence */
      | Some _
      | None => {
        str
      }
    }
  };

  let rec nextTokenValue (): tokenValue => {
    switch (LazyStream.next s) {
      /* single character tokens */
      | Char ')' => Paren Closing
      | Char '{' => Brace Opening
      | Char '}' => Brace Closing
      | Char '[' => Bracket Opening
      | Char ']' => Bracket Closing
      | Char '=' => Equal
      | Char ':' => Colon
      | Char ';' => Semicolon
      | Char '+' => Plus
      | Char '&' => Ampersand
      | Char '>' => Arrow
      | Char '*' => Asterisk
      | Char '~' => Tilde
      | Char ',' => Comma

      /* detect whether parenthesis is part of url() */
      | Char '(' => {
        if (!lastTokenValue == Some (Word "url")) {
          bufferURLContent ();
          Paren Opening
        } else {
          Paren Opening
        }
      }

      /* skip over carriage return and whitespace */
      | Char '\r'
      | Char ' ' => nextTokenValue ()

      /* count up current line number and search next tokenValue */
      | Char '\n' => {
        line := !line + 1;
        nextTokenValue ()
      }

      /* detect quotes and parse string */
      | Char ('\"' as c)
      | Char ('\'' as c) => {
        let stringContent = captureStringContent c "";
        Str stringContent
      }

      /* detect backslash i.e. escape as start of a word and parse it */
      | Char '\\' => Word (captureEscapedContent ())

      /* detect start of a word and parse it */
      | Char ('a'..'z' as c)
      | Char ('A'..'Z' as c)
      | Char ('0'..'9' as c)
      | Char ('#' as c)
      | Char ('!' as c)
      | Char ('.' as c) => {
        let wordContent = captureWordContent (String.make 1 c);
        Word wordContent
      }

      /* detect at-words and parse it like a normal word afterwards */
      | Char '@' => {
        let wordContent = captureWordContent "@";
        AtWord wordContent
      }

      /* pass-through interpolation */
      | Interpolation x => Interpolation x

      /* detect and skip comments, then search next tokenValue */
      | Char '/' when (LazyStream.peek s == Some (Char '*')) => {
        LazyStream.junk s; /* throw away the leading asterisk */
        skipCommentContent ();
        nextTokenValue ()
      }

      /* all unrecognised characters will be raised outside of designated matching loops */
      | Char c => {
        let msg = "Unexpected token encountered: " ^ (String.make 1 c);
        raise (LazyStream.Error msg)
      }
    }
  };

  let next _: option token => {
    switch !tokenValueBuffer {
      /* empty tokenValue buffer before scanning new tokens */
      | [bufferedItem, ...rest] => {
        tokenValueBuffer := rest;
        Some (Token bufferedItem !line)
      }

      /* get next token and return it, except if stream is empty */
      | [] => switch (nextTokenValue ()) {
        | value => {
          lastTokenValue := Some value;
          Some (Token value !line)
        }
        | exception LazyStream.Failure => None
      }
    }
  };

  LazyStream.from next
};
