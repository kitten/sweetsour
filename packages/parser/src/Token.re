type interpolation;

type pair =
  | T_PAIR_OPENING
  | T_PAIR_CLOSING;

type quote =
  | T_QUOTE_DOUBLE
  | T_QUOTE_SINGLE;

type value =
  | T_REF(interpolation)
  | T_BRACKET_ROUND(pair) /* `(` & `)`, aka parentheses */
  | T_BRACKET_SQUARE(pair) /* `[` & `]`, aka brackets */
  | T_BRACKET_CURLY(pair) /* `{` & `}`, aka braces */
  | T_LITERAL_WORD(string)
  | T_LITERAL_ATWORD(string)
  | T_LITERAL_STRING(string)
  | T_SYMBOL_QUOTE(quote) /* double or single quotes */
  | T_SYMBOL_EXCLAMATION /* `!` */
  | T_SYMBOL_EQUAL /* `=` */
  | T_SYMBOL_COLON /* `:` */
  | T_SYMBOL_SEMI /* `;` */
  | T_SYMBOL_PLUS /* `+` */
  | T_SYMBOL_AMPERSAND /* `&` */
  | T_SYMBOL_GREATER /* `>`, aka arrow */
  | T_SYMBOL_ASTERISK /* `*` */
  | T_SYMBOL_TILDE /* `~` */
  | T_SYMBOL_COMMA /* `,` */
  | T_SYMBOL_PIPE /* `|` */
  | T_SYMBOL_DOLLAR /* `$` */
  | T_SYMBOL_CARET /* `^` */
  | T_EOF; /* end of tokens/file */

type t = Token(value, Loc.t);
