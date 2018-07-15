/* This is a nested representation of a CSS AST, closely following the choices
   made by the ISTF spec. The advantafe of this structure is improved efficiency
   when parsing recursively, while the structure itself is easily converted to
   flat ISTF nodes */

type literal =
  | Word(string)
  | Ref(Token.interpolation);

type case =
  | CaseSensitive
  | CaseInsensitive;

type attribute = {
  name: literal,
  operator: string,
  value: literal,
  case: case
};

type selector =
  | Literal(literal)
  | Attribute(attribute)
  | Compound(list(selector))
  | Function(string, list(selector))
  | ParentSelector
  | UniversalSelector
  | SpaceCombinator
  | ChildCombinator
  | DoubledChildCombinator
  | NextSiblingCombinator
  | SubsequentSiblingCombinator;

type value =
  | Literal(literal)
  | Compound(list(value))
  | Function(string, list(value))
  | StringLiteral(list(literal));

type declaration = {
  property: literal,
  value: value
};

type rule =
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

type ruleSet = {
  kind: rule,
  selectors: list(selector),
  declarations: list(declaration)
};
