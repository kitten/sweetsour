open Ast;
open Token;

exception TODO;
exception UnexpectedEof;
exception UnexpectedToken(Token.t);
exception ExpectedDeclaration;

let parseLiteral = (token: Token.t): literal => {
  let Token(value, _) = token;

  switch (value) {
  | T_LITERAL_WORD(x) => Word(x)
  | T_REF(x) => Ref(x)
  | T_EOF => raise(UnexpectedEof)
  | _ => raise(UnexpectedToken(token))
  }
};

let parseValues = (tokens: list(Token.t)): list(value) => {
  let rec parseValue = (tokens: list(Token.t), values: list(value)) => {
    let [token, ...tokens] = tokens;

    let value = switch (token) {
    | T_LITERAL_WORD(word) => Value(Word(word))
    | T_REF(x) => Value(Ref(x))
    };
  };

  explode(tokens)
};

let parseDeclaration = (env: ParseEnv.t): body => {
  let (env, propertyToken) = ParseEnv.source(env);
  let property = parseLiteral(propertyToken);
  let declaration = Declaration(property, []);
  declaration
};

let rec parseDeclarationOrSelector = (
  env: ParseEnv.t,
  tokens: list(Token.t)
): (ParseEnv.t, body) => {
  let (env, token) = ParseEnv.source(env);
  let Token(value, _) = token;

  switch (value) {
  | T_BRACKET_CURLY(T_PAIR_OPENING) => {
    /* TODO: Trigger selector parsing */
    raise(TODO)
  }

  | T_SYMBOL_SEMI
  | T_BRACKET_CURLY(T_PAIR_CLOSING) => {
    let env = ParseEnv.buffer(token, env);
    let innerEnv = ParseEnv.make(List.rev(tokens));
    (env, parseDeclaration(innerEnv))
  }

  | T_EOF => raise(UnexpectedEof)
  | _ => parseDeclarationOrSelector(env, [token, ...tokens])
  }
};

let parseBody = (env: ParseEnv.t): (ParseEnv.t, body) => {
  let (env, a) = ParseUtils.sourceAfterSemis(env);
  let (env, b) = ParseEnv.source(env);

  let Token(aVal, aLoc) = a;
  let Token(bVal, bLoc) = b;

  switch (aVal, bVal) {
  | (T_REF(x), T_EOF)
  | (T_REF(x), T_BRACKET_CURLY(T_PAIR_CLOSING))
  | (T_REF(x), T_SYMBOL_SEMI) => {
    (env, Ref(x))
  }

  | (T_REF(x), _) when ParseUtils.isDistantRow(aLoc, bLoc) => {
    (env, Ref(x))
  }

  | _ => parseDeclarationOrSelector(env, [b, a])
  }
};

let parse = (env: ParseEnv.t): ruleSet => {
  kind: StyleRule,
  selectors: [],
  rules: []
};
