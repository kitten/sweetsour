open Common;

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

type node =
  | RuleStart ruleKind
  | RuleEnd
  | RuleName string
  | Selector string
  | ParentSelector
  | CompoundSelectorStart
  | CompoundSelectorEnd
  | SpaceCombinator
  | DoubledChildCombinator
  | ChildCombinator
  | NextSiblingCombinator
  | SubsequentSiblingCombinator
  | Property string
  | Value string
  | CompoundValueStart
  | CompoundValueEnd
  | Condition string
  | FunctionStart string
  | FunctionEnd
  | AnimationName string
  | SelectorInterpolation Common.interpolation
  | PropertyInterpolation Common.interpolation
  | ValueInterpolation Common.interpolation
  | PartialInterpolation Common.interpolation;

type parserStream = LazyStream.t node;

let parser (s: Lexer.lexerStream) => {
  let nextNode (): node => {
    RuleEnd
  };

  let next: (unit => option node) [@bs] = (fun () => {
    None
  }) [@bs];

  LazyStream.from next
};
