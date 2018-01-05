open Common;

/* an error raised by the converter when accepting raw (JS) nodes */
exception ConversionError(string);

/* For RuleStart kinds */
[@bs.deriving jsConverter]
type ruleKind =
  [@bs.as 1] | StyleRule /* CSSOM */
  [@bs.as 2] | CharsetRule /* CSSOM */
  [@bs.as 3] | ImportRule /* CSSOM */
  [@bs.as 4] | MediaRule /* CSSOM */
  [@bs.as 5] | FontFaceRule /* CSSOM */
  [@bs.as 6] | PageRule /* CSSOM */
  [@bs.as 7] | KeyframesRule /* CSS 3 Animations */
  [@bs.as 8] | KeyframeRule /* CSS 3 Animations */
  [@bs.as 9] | MarginRule /* CSSOM */
  [@bs.as 10] | NamespaceRule /* CSSOM */
  [@bs.as 11] | CounterStyleRule /* CSS 3 Lists */
  [@bs.as 12] | SupportsRule /* CSS 3 Conditional */
  [@bs.as 13] | DocumentRule /* CSS 3 Conditional */
  [@bs.as 14] | FontFeatureValuesRule /* CSS 3 Fonts */
  [@bs.as 15] | ViewportRule /* CSS Device Adapt */
  [@bs.as 16] | RegionStyleRule; /* Proposed for CSS 3 Regions */

/* For attribute selector kinds */
[@bs.deriving jsConverter]
type attributeSelectorKind =
  [@bs.as 1] | CaseSensitive
  [@bs.as 2] | CaseInsensitive;

/* A node is represented by its (ISTF) type and potentially a value */

[@bs.deriving jsConverter]
type nodeKind_ruleKind =
  [@bs.as 0] | RuleStart;

[@bs.deriving jsConverter]
type nodeKind_attributeKind =
  [@bs.as 26] | AttributeSelectorStart;

[@bs.deriving jsConverter]
type nodeKind_ref =
  [@bs.as 20] | SelectorRef
  [@bs.as 21] | PropertyRef
  [@bs.as 22] | ValueRef
  [@bs.as 23] | PartialRef
  [@bs.as 29] | AttributeNameRef
  [@bs.as 32] | AttributeValueRef
  [@bs.as 34] | ConditionRef;

[@bs.deriving jsConverter]
type nodeKind_string =
  [@bs.as 3] | Selector
  [@bs.as 13] | Property
  [@bs.as 14] | Value
  [@bs.as 17] | FunctionStart
  [@bs.as 24] | StringStart
  [@bs.as 28] | AttributeName
  [@bs.as 30] | AttributeOperator
  [@bs.as 31] | AttributeValue
  [@bs.as 33] | Condition;

[@bs.deriving jsConverter]
type nodeKind_empty =
  [@bs.as 1] | RuleEnd
  [@bs.as 4] | ParentSelector
  [@bs.as 5] | UniversalSelector
  [@bs.as 6] | CompoundSelectorStart
  [@bs.as 7] | CompoundSelectorEnd
  [@bs.as 8] | SpaceCombinator
  [@bs.as 9] | DoubledChildCombinator
  [@bs.as 10] | ChildCombinator
  [@bs.as 11] | NextSiblingCombinator
  [@bs.as 12] | SubsequentSiblingCombinator
  [@bs.as 15] | CompoundValueStart
  [@bs.as 16] | CompoundValueEnd
  [@bs.as 18] | FunctionEnd
  [@bs.as 25] | StringEnd
  [@bs.as 27] | AttributeSelectorEnd
  [@bs.as 35] | CompoundConditionStart
  [@bs.as 36] | CompoundConditionEnd
  [@bs.as 37] | ConditionGroupStart
  [@bs.as 38] | ConditionGroupEnd;

type node =
  | RuleKindNode(nodeKind_ruleKind, ruleKind)
  | AttributeKindNode(nodeKind_attributeKind, attributeSelectorKind)
  | RefNode(nodeKind_ref, interpolation)
  | StringNode(nodeKind_string, string)
  | Node(nodeKind_empty);

type rawNodePayload;

external intToRawNodePayload: (int) => rawNodePayload = "%identity";
external refToRawNodePayload: (interpolation) => rawNodePayload = "%identity";
external stringToRawNodePayload: (string) => rawNodePayload = "%identity";

let emptyRawNodePayload: [@bs] (int) => (int, rawNodePayload) = [%raw {|
  function (x) { return [x]; }
|}];

/* convert IstfNode.t to JS format */
let nodeToJs = (node: node) : (int, rawNodePayload) => {
  switch (node) {
  | RuleKindNode(kind, x) => (
    nodeKind_ruleKindToJs(kind),
    intToRawNodePayload(ruleKindToJs(x))
  )

  | AttributeKindNode(kind, x) => (
    nodeKind_attributeKindToJs(kind),
    intToRawNodePayload(attributeSelectorKindToJs(x))
  )

  | RefNode(kind, x) => (
    nodeKind_refToJs(kind),
    refToRawNodePayload(x)
  )

  | StringNode(kind, x) => (
    nodeKind_stringToJs(kind),
    stringToRawNodePayload(x)
  )

  | Node(kind) => [@bs] emptyRawNodePayload(nodeKind_emptyToJs(kind))
  }
};

let nodeFromJs = (nodeRaw: (int, rawNodePayload)) : node => {
  let (nodeKind, _) = nodeRaw;

  let tuple = (
    nodeKind_ruleKindFromJs(nodeKind),
    nodeKind_attributeKindFromJs(nodeKind),
    nodeKind_refFromJs(nodeKind),
    nodeKind_stringFromJs(nodeKind),
    nodeKind_emptyFromJs(nodeKind)
  );

  switch (tuple) {
  | (Some(x), _, _, _, _) => RuleKindNode(x, [%raw {| nodeRaw[1] |}])
  | (_, Some(x), _, _, _) => AttributeKindNode(x, [%raw {| nodeRaw[1] |}])
  | (_, _, Some(x), _, _) => RefNode(x, [%raw {| nodeRaw[1] |}])
  | (_, _, _, Some(x), _) => StringNode(x, [%raw {| nodeRaw[1] |}])
  | (_, _, _, _, Some(x)) => Node(x)
  | _ => raise(ConversionError("Serialised node data was not recognised"))
  }
};
