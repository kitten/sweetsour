open IstfNode;

exception FlattenerError;

type flattenerMode =
  | InitLoop
  | MainLoop
  | RuleStartLoop(ruleKind)
  | StyleRuleStartLoop
  | SelectorLoop
  | ConditionLoop
  | EndLoop;

type state = {
  mutable isOutsideStyleRule: bool,
  mutable selectorListIterator: NestedList.iterator(node),
  mutable selectorListStack: list(NestedList.t(node)),
  mutable ruleKindStack: list(ruleKind),
  mutable mode: flattenerMode
};

/* creates initial selectorList and ruleKind stacks */
let makeInitialRuleStacks = () : (list(NestedList.t(node)), list(ruleKind)) => {
  let firstSelectorList = NestedList.create();
  NestedList.add(Node(ParentSelector), firstSelectorList);
  ([firstSelectorList], [StyleRule])
};

/* returns the first selectorList on the selectorList stack */
let getFirstSelectorList = (stack: list(NestedList.t(node))) : NestedList.t(node) =>
  switch (stack) {
  | [x, ..._] => x
  | [] => raise(FlattenerError)
  };

/* returns whether `n` is a declaration node */
let isDeclarationNode = (n: node) =>
  switch (n) {
    | StringNode(Property | Value | StringStart | FunctionStart, _)
    | RefNode(PropertyRef | ValueRef, _)
    | Node(CompoundValueStart | CompoundValueEnd | StringEnd | FunctionEnd) => true

    | _ => false
  };

/* adds a parent selectorList to a child selectorList, effectively appending selectors as they nest
   NOTE: For now this will leave nested, superfluous compound selector groups, which is fine
   from a stringification standpoint */
let addLowerSelectorList = (
  lowerSelectorList: NestedList.t(node),
  selectorList: NestedList.t(node),
  ~shouldAddSpaceCombinator: bool,
  ~hasCompoundFirst: bool
) => {
  /* indicates that first node (compound start) must be "hopped" over (unshiftSecond) */
  if (hasCompoundFirst) {
    /* add space combinator if no other combinator was before the first selector */
    if (shouldAddSpaceCombinator) NestedList.unshiftSecond(Node(SpaceCombinator), selectorList);

    /* add lower selector list */
    NestedList.unshiftBranchSecond(lowerSelectorList, selectorList);
  } else {
    /* add compound end after first selector node */
    NestedList.unshiftSecond(Node(CompoundSelectorEnd), selectorList);

    /* add space combinator if no other combinator was before the first selector (see above) */
    if (shouldAddSpaceCombinator) NestedList.unshift(Node(SpaceCombinator), selectorList);

    /* add lower selector list */
    NestedList.unshiftBranch(lowerSelectorList, selectorList);
    /* add compound start as first node */
    NestedList.unshift(Node(CompoundSelectorStart), selectorList);
  };
};

let flattener = (s: nodeStream) : nodeStream => {
  let (selectorListStack, ruleKindStack) = makeInitialRuleStacks();

  let state = {
    isOutsideStyleRule: true,
    selectorListIterator: NestedList.createIterator(getFirstSelectorList(selectorListStack)),
    selectorListStack,
    ruleKindStack,
    mode: InitLoop
  };

  let recordSelectors = () : NestedList.t(node) => {
    let lowerSelectorList = getFirstSelectorList(state.selectorListStack);

    let rec record = (
      ~selectorList: NestedList.t(node),
      ~isFirst: bool,
      ~shouldAddSpaceCombinator: bool,
      ~hasCompoundFirst: bool,
      ~seenParentSelector: bool
    ) => {
      switch (LazyStream.peek(s)) {
      | Some(Node(ParentSelector)) => {
        LazyStream.junk(s);
        NestedList.appendBranch(lowerSelectorList, selectorList);
        record(~selectorList, ~isFirst=false, ~shouldAddSpaceCombinator, ~hasCompoundFirst, ~seenParentSelector=true);
      }

      | Some(Node(CompoundSelectorStart) as x) when isFirst === true => {
        LazyStream.junk(s);
        NestedList.add(x, selectorList);
        record(~selectorList, ~isFirst=true, ~shouldAddSpaceCombinator, ~hasCompoundFirst=true, ~seenParentSelector);
      }

      | Some(Node(
          SpaceCombinator | DoubledChildCombinator | ChildCombinator | NextSiblingCombinator | SubsequentSiblingCombinator
        ) as x) when isFirst === true => {
        LazyStream.junk(s);
        NestedList.add(x, selectorList);
        record(~selectorList, ~isFirst=false, ~shouldAddSpaceCombinator=false, ~hasCompoundFirst, ~seenParentSelector);
      }

      | Some((
        StringNode(Selector | AttributeName | AttributeOperator | AttributeValue | StringStart | FunctionStart, _) |
        RefNode(SelectorRef | AttributeNameRef | AttributeValueRef, _) |
        AttributeKindNode(AttributeSelectorStart, _) |
        Node(
          CompoundSelectorStart | CompoundSelectorEnd | StringEnd | FunctionEnd | UniversalSelector | SpaceCombinator |
          DoubledChildCombinator | ChildCombinator | NextSiblingCombinator | SubsequentSiblingCombinator | AttributeSelectorEnd
        )
      ) as x) => {
        LazyStream.junk(s);
        NestedList.add(x, selectorList);
        record(~selectorList, ~isFirst=false, ~shouldAddSpaceCombinator, ~hasCompoundFirst, ~seenParentSelector);
      }

      | _ => {
        if (!seenParentSelector)
          addLowerSelectorList(lowerSelectorList, selectorList, ~shouldAddSpaceCombinator, ~hasCompoundFirst);

        selectorList
      }
      }
    };

    record(
      ~selectorList=NestedList.create(),
      ~isFirst=true,
      ~seenParentSelector=false,
      ~hasCompoundFirst=false,
      ~shouldAddSpaceCombinator=true
    );
  };

  let rec parseRuleEnd = () => {
    switch (state.selectorListStack, state.ruleKindStack) {
    /* for StylRules pop item off of both stacks, emit RuleEnd, and invoke StyleRuleStartLoop */
    | ([_, ...restSelectorListStack], [StyleRule, ...restRuleKindStack]) => {
      state.selectorListStack = restSelectorListStack;
      state.ruleKindStack = restRuleKindStack;

      switch (LazyStream.peek(s)) {
      | Some(Node(RuleEnd)) => {
        LazyStream.junk(s);
        parseRuleEnd()
      }

      | Some(RuleKindNode(RuleStart, StyleRule)) => {
        state.isOutsideStyleRule = true;
        state.mode = MainLoop;
      }

      | Some(_) => state.mode = StyleRuleStartLoop;
      | None => state.mode = EndLoop;
      }
    }

    /* for other rules just pop off ruleKind */
    | (_, [_, ...restRuleKindStack]) => state.ruleKindStack = restRuleKindStack;

    /* emit error when the stacks are empty
       (although the Parser guarantees this to never happen) */
    | _ => raise(FlattenerError)
    }
  };

  let ruleStartLoop = (kind: ruleKind) => {
    state.isOutsideStyleRule = true;
    state.mode = ConditionLoop;
    Some(RuleKindNode(RuleStart, kind))
  };

  let styleRuleStartLoop = () => {
    state.mode = SelectorLoop;
    state.selectorListIterator = NestedList.createIterator(
      getFirstSelectorList(state.selectorListStack));

    Some(RuleKindNode(RuleStart, StyleRule))
  };

  let rec parseStyleRuleStartLoop = () => {
    let prevSelectorListStack = state.selectorListStack;
    let prevRuleKindStack = state.ruleKindStack;

    state.selectorListStack = [recordSelectors(), ...state.selectorListStack];
    state.ruleKindStack = [StyleRule, ...state.ruleKindStack];

    switch (LazyStream.peek(s)) {
    /* when next node is a declaration node, emit rule start and invoke StyleRuleStartLoop */
    | Some(n) when isDeclarationNode(n) => {
      state.mode = StyleRuleStartLoop;

      if (state.isOutsideStyleRule) {
        styleRuleStartLoop()
      } else {
        Some(Node(RuleEnd))
      }
    }

    /* when next node is another RuleStart then invoke self */
    | Some(RuleKindNode(RuleStart, StyleRule)) => {
      LazyStream.junk(s);
      parseStyleRuleStartLoop()
    }

    /* when next node is the RuleEnd immediately, revert both stacks, and invoke mainLoop */
    | Some(Node(RuleEnd)) => {
      LazyStream.junk(s);
      state.selectorListStack = prevSelectorListStack;
      state.ruleKindStack = prevRuleKindStack;
      mainLoop()
    }

    /* when next node is anything else, do nothing and invoke mainLoop */
    | _ => mainLoop()
    }
  }

  and mainLoop = () => {
    switch (LazyStream.next(s)) {
    | Some(node) as x when isDeclarationNode(node) => x

    | Some(RuleKindNode(RuleStart, StyleRule)) => parseStyleRuleStartLoop()

    | Some(RuleKindNode(RuleStart, ruleKind)) => {
      state.mode = RuleStartLoop(ruleKind);
      state.ruleKindStack = [ruleKind, ...state.ruleKindStack];

      if (state.isOutsideStyleRule) {
        ruleStartLoop(ruleKind)
      } else {
        Some(Node(RuleEnd))
      }
    }

    /* detect end of style-rules */
    | Some(Node(RuleEnd)) as x => {
      parseRuleEnd();
      x
    }

    | None => {
      if (state.isOutsideStyleRule) {
        None
      } else {
        state.mode = EndLoop;
        Some(Node(RuleEnd))
      }
    }

    /* all other nodes are unexpected and must raise an error */
    | Some(_) => raise(FlattenerError)
    }
  }

  and selectorLoop = () => {
    switch (NestedList.next(state.selectorListIterator)) {
    | None => {
      state.isOutsideStyleRule = false;
      state.mode = MainLoop;
      mainLoop()
    }
    | Some(_) as x => x
    }
  }

  and conditionLoop = () => {
    switch (LazyStream.peek(s)) {
    | Some(
      RefNode(ConditionRef, _) |
      StringNode(Condition | StringStart | FunctionStart, _) |
      Node(
        CompoundConditionStart | CompoundConditionEnd | ConditionGroupStart |
        ConditionGroupEnd | StringEnd | FunctionEnd
      )
    ) as node => {
      LazyStream.junk(s);
      node
    }

    | Some(node) when isDeclarationNode(node) => {
      state.mode = StyleRuleStartLoop;
      styleRuleStartLoop()
    }

    | Some(RuleKindNode(RuleStart, StyleRule)) => {
      LazyStream.junk(s);
      parseStyleRuleStartLoop()
    }

    | _ => {
      state.mode = MainLoop;
      mainLoop()
    }
    }
  };

  let initLoop = () => {
    switch (LazyStream.peek(s)) {
    | Some(node) when isDeclarationNode(node) => {
      state.mode = StyleRuleStartLoop;
      styleRuleStartLoop()
    }

    | _ => {
      state.mode = MainLoop;
      mainLoop()
    }
    }
  };

  let next: [@bs] (unit => option(node)) = [@bs] () => {
    let res = switch state.mode {
    | InitLoop => initLoop()
    | MainLoop => mainLoop()
    | RuleStartLoop(x) => ruleStartLoop(x)
    | StyleRuleStartLoop => styleRuleStartLoop()
    | SelectorLoop => selectorLoop()
    | ConditionLoop => conditionLoop()
    | EndLoop => None
    };

    res
  };

  LazyStream.from(next)
};
