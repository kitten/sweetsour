open Common;

/* an error raised by the parser contains a message and a line number */
exception OutputError(string);

type outputNode =
  | EmptyMarker(int)
  | IntMarker(int, int)
  | StringMarker(int, string)
  | RefMarker(int, interpolation);

/* Stream type for the OutputStream */
type outputStream = LazyStream.t(outputNode);

let output = (s: Parser.parserStream) => {
  let serialiseRuleKind = (kind: Parser.ruleKind) : int => Parser.ruleKindToJs(kind);

  let serialiseAttributeSelectorKind = (kind: Parser.attributeSelectorKind) : int =>
    Parser.attributeSelectorKindToJs(kind);

  let serialiseNode = (n: Parser.node) : outputNode => {
    switch (n) {
    | RuleStart(ruleKind) => IntMarker(0, serialiseRuleKind(ruleKind))
    | RuleEnd => EmptyMarker(1)
    | RuleName(str) => StringMarker(2, str)
    | Selector(str) => StringMarker(3, str)
    | ParentSelector => EmptyMarker(4)
    | UniversalSelector => EmptyMarker(5)
    | CompoundSelectorStart => EmptyMarker(6)
    | CompoundSelectorEnd => EmptyMarker(7)
    | SpaceCombinator => EmptyMarker(8)
    | DoubledChildCombinator => EmptyMarker(9)
    | ChildCombinator => EmptyMarker(10)
    | NextSiblingCombinator => EmptyMarker(11)
    | SubsequentSiblingCombinator => EmptyMarker(12)
    | Property(str) => StringMarker(13, str)
    | Value(str) => StringMarker(14, str)
    | CompoundValueStart => EmptyMarker(15)
    | CompoundValueEnd => EmptyMarker(16)
    | Condition(str) => StringMarker(17, str)
    | FunctionStart(str) => StringMarker(18, str)
    | FunctionEnd => EmptyMarker(19)
    | AnimationName(str) => StringMarker(20, str)
    | SelectorRef(x) => RefMarker(21, x)
    | PropertyRef(x) => RefMarker(22, x)
    | ValueRef(x) => RefMarker(23, x)
    | PartialRef(x) => RefMarker(24, x)
    | StringStart(str) => StringMarker(25, str)
    | StringEnd => EmptyMarker(26)
    | AttributeSelectorStart(kind) => IntMarker(27, serialiseAttributeSelectorKind(kind))
    | AttributeSelectorEnd => EmptyMarker(28)
    | AttributeName(str) => StringMarker(29, str)
    | AttributeNameRef(x) => RefMarker(30, x)
    | AttributeOperator(str) => StringMarker(31, str)
    | AttributeValue(str) => StringMarker(32, str)
    | AttributeValueRef(x) => RefMarker(33, x)
    | ConditionRef(x) => RefMarker(34, x)
    | EOF => raise(OutputError("Unexpected Parser node"))
    }
  };

  let next: [@bs] (unit => option(outputNode)) = [@bs] (() => {
    switch (LazyStream.next(s)) {
    | Some(node) => Some(serialiseNode(node))
    | None => None
    }
  });

  LazyStream.from(next)
};
