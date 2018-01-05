open IstfNode;

/* Stream type for the ParserStream */
type printerStream = LazyStream.t(string);

let printer = (s: Parser.parserStream) => {
  let indentation = ref(0);

  let stringifyKind = (kind: ruleKind) : string => {
    switch (kind) {
    | StyleRule => "STYLE_RULE"
    | CharsetRule => "CHARSET_RULE"
    | ImportRule => "IMPORT_RULE"
    | MediaRule => "MEDIA_RULE"
    | FontFaceRule => "FONT_FACE_RULE"
    | PageRule => "PAGE_RULE"
    | KeyframesRule => "KEYFRAMES_RULE"
    | KeyframeRule => "KEYFRAME_RULE"
    | MarginRule => "MARGIN_RULE"
    | NamespaceRule => "NAMESPACE_RULE"
    | CounterStyleRule => "COUNTER_STYLE_RULE"
    | SupportsRule => "SUPPORTS_RULE"
    | DocumentRule => "DOCUMENT_RULE"
    | FontFeatureValuesRule => "FONT_FEATURE_VALUES_RULE"
    | ViewportRule => "VIEWPORT_RULE"
    | RegionStyleRule => "REGION_STYLE_RULE"
    }
  };

  let stringifyNode = (node: node) : string => {
    let indentImmediate = ref(indentation^);
    let nodeOutput = switch (node) {
    | RuleKindNode(RuleStart, kind) => {
      indentation := indentation^ + 1;
      "[RULE_START, " ++ stringifyKind(kind) ++ "]"
    }
    | Node(RuleEnd) => {
      indentation := indentation^ - 1;
      indentImmediate := indentImmediate^ - 1;
      "[RULE_END]"
    }
    | StringNode(Selector, str) => "[SELECTOR, '" ++ str ++ "']"
    | Node(ParentSelector) => "[PARENT_SELECTOR]"
    | Node(UniversalSelector) => "[UNIVERSAL_SELECTOR]"
    | Node(CompoundSelectorStart) => {
      indentation := indentation^ + 1;
      "[COMPOUND_SELECTOR_START]"
    }
    | Node(CompoundSelectorEnd) => {
      indentation := indentation^ - 1;
      indentImmediate := indentImmediate^ - 1;
      "[COMPOUND_SELECTOR_END]"
    }
    | Node(SpaceCombinator) => "[SPACE_COMBINATOR]"
    | Node(DoubledChildCombinator) => "[DOUBLED_CHILD_COMBINATOR]"
    | Node(ChildCombinator) => "[CHILD_COMBINATOR]"
    | Node(NextSiblingCombinator) => "[NEXT_SIBLING_COMBINATOR]"
    | Node(SubsequentSiblingCombinator) => "[SUBSEQUENT_SIBLING_COMBINATOR]"
    | StringNode(Property, str) => "[PROPERTY, '" ++ str ++ "']"
    | StringNode(Value, str) => "[VALUE, '" ++ str ++ "']"
    | Node(CompoundValueStart) => {
      indentation := indentation^ + 1;
      "[COMPOUND_VALUE_START]"
    }
    | Node(CompoundValueEnd) => {
      indentation := indentation^ - 1;
      indentImmediate := indentImmediate^ - 1;
      "[COMPOUND_VALUE_END]"
    }
    | StringNode(Condition, str) => "[CONDITION, '" ++ str ++ "']"
    | StringNode(FunctionStart, str) => {
      indentation := indentation^ + 1;
      "[FUNCTION_START, '" ++ str ++ "']"
    }
    | Node(FunctionEnd) => {
      indentation := indentation^ - 1;
      indentImmediate := indentImmediate^ - 1;
      "[FUNCTION_END]"
    }
    | RefNode(SelectorRef, _) => "[SELECTOR_REF, ref]"
    | RefNode(PropertyRef, _) => "[PROPERTY_REF, ref]"
    | RefNode(ValueRef, _) => "[VALUE_REF, ref]"
    | RefNode(PartialRef, _) => "[PARTIAL_REF, ref]"
    | StringNode(StringStart, str) => {
      indentation := indentation^ + 1;
      "[STRING_START, '" ++ str ++ "']"
    }
    | Node(StringEnd) => {
      indentation := indentation^ - 1;
      indentImmediate := indentImmediate^ - 1;
      "[STRING_END]"
    }
    | AttributeKindNode(AttributeSelectorStart, kind) =>
      "[ATTRIBUTE_SELECTOR_START, " ++ string_of_int(attributeSelectorKindToJs(kind)) ++ "]"
    | Node(AttributeSelectorEnd) => "[ATTRIBUTE_SELECTOR_END]"
    | StringNode(AttributeName, str) => "[ATTRIBUTE_NAME, '" ++ str ++ "']"
    | RefNode(AttributeNameRef, _) => "[ATTRIBUTE_NAME_REF, ref]"
    | StringNode(AttributeOperator, str) => "[ATTRIBUTE_OPERATOR, '" ++ str ++ "']"
    | StringNode(AttributeValue, str) => "[ATTRIBUTE_VALUE, '" ++ str ++ "']"
    | RefNode(AttributeValueRef, _) => "[ATTRIBUTE_VALUE_REF, ref]"
    | RefNode(ConditionRef, _) => "[CONDITION_REF, ref]"
    | Node(CompoundConditionStart) => {
      indentation := indentation^ + 1;
      "[COMPOUND_CONDITION_START]"
    }
    | Node(CompoundConditionEnd) => {
      indentation := indentation^ - 1;
      indentImmediate := indentImmediate^ - 1;
      "[COMPOUND_CONDITION_END]"
    }
    | Node(ConditionGroupStart) => {
      indentation := indentation^ - 1;
      indentImmediate := indentImmediate^ - 1;
      "[CONDITION_GROUP_START]"
    }
    | Node(ConditionGroupEnd) => {
      indentation := indentation^ - 1;
      indentImmediate := indentImmediate^ - 1;
      "[CONDITION_GROUP_END]"
    }
    };

    let indent = String.make(indentImmediate^ * 2, ' ');
    indent ++ nodeOutput
  };

  let next: [@bs] (unit => option(string)) = [@bs] (() => {
    switch (LazyStream.next(s)) {
      | Some(node) => Some(stringifyNode(node))
      | None => None
    }
  });

  LazyStream.from(next)
};

/* collects all emissions of a stream in an array */
let printToStr = (s: printerStream) => {
  let rec populate = (acc: string) : string => {
    switch (LazyStream.next(s)) {
    | Some(str) => populate(acc ++ "\n" ++ str)
    | None => acc
    }
  };

  populate("")
};
