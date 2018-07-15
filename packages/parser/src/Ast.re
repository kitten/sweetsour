/* This is a nested representation of a CSS AST, closely following the choices
   made by the ISTF spec. The advantafe of this structure is improved efficiency
   when parsing recursively, while the structure itself is easily converted to
   flat ISTF nodes */

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

type literal =
  | Word(string)
  | Ref(Token.interpolation);

type case =
  | CaseSensitive
  | CaseInsensitive;

type attributeT = {
  name: literal,
  operator: string,
  value: literal,
  case: case
};

type selector =
  | Selector(literal)
  | SelectorFn(string, list(selector))
  | Compound(list(selector))
  | Attribute(attributeT)
  | ParentSelector
  | UniversalSelector
  | SpaceCombinator
  | ChildCombinator
  | DoubledChildCombinator
  | NextSiblingCombinator
  | SubsequentSiblingCombinator;

type value =
  | Value(literal)
  | ValueFn(string, list(value))
  | Compound(list(value))
  | StringLiteral(list(literal));

type body =
  | Declaration(literal, value)
  | Rule(ruleSet)
  | Ref(Token.interpolation)
and ruleSet = {
  kind: ruleKind,
  selectors: list(selector),
  rules: list(body)
};
