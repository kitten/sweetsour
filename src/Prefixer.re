let prefixForProp = (prop: string) : option(list(string)) => {
  switch (prop) {
  /* -webkit- prefixes */
  | "text-emphasis-position"
  | "text-emphasis"
  | "text-emphasis-style"
  | "text-emphasis-color"
  | "box-decoration-break"
  | "clip-path"
  | "mask-image"
  | "mask-mode"
  | "mask-repeat"
  | "mask-position"
  | "mask-clip"
  | "mask-origin"
  | "mask-size"
  | "mask-composite"
  | "mask"
  | "mask-border-source"
  | "mask-border-mode"
  | "mask-border-slice"
  | "mask-border-width"
  | "mask-border-outset"
  | "mask-border-repeat"
  | "mask-border"
  | "mask-type"
  | "text-decoration-style"
  | "text-decoration-skip"
  | "text-decoration-line"
  | "text-decoration-color"
  | "filter"
  | "font-feature-settings"
  | "flex"
  | "flex-basis"
  | "flex-direction"
  | "flex-grow"
  | "flex-flow"
  | "flex-shrink"
  | "flex-wrap"
  | "align-content"
  | "align-items"
  | "align-self"
  | "justify-content"
  | "order"
  | "transform"
  | "transform-origin"
  | "transform-origin-x"
  | "transform-origin-y"
  | "backface-visibility"
  | "perspective"
  | "perspective-origin"
  | "transform-style"
  | "transform-origin-z"
  | "animation"
  | "animation-delay"
  | "animation-direction"
  | "animation-fill-mode"
  | "animation-duration"
  | "animation-iteration-count"
  | "animation-name"
  | "animation-play-state"
  | "animation-timing-function"
  | "backdrop-filter"
  | "font-kerning"
  | "shape-image-threshold"
  | "shape-image-margin"
  | "shape-image-outside"
  | "border-image"
  | "border-image-outset"
  | "border-image-repeat"
  | "border-image-slice"
  | "border-image-source"
  | "border-image-width"
  | "transition-delay"
  | "transition-duration"
  | "transition-property"
  | "transition-timing-function" => {
    Some(["-webkit-" ++ prop, prop])
  }

  /* -webkit- & -moz- prefixes */
  | "appearance"
  | "column-count"
  | "column-fill"
  | "column-gap"
  | "column-rule"
  | "column-rule-color"
  | "column-rule-style"
  | "column-rule-width"
  | "columns"
  | "column-span"
  | "column-width" => {
    Some(["-webkit-" ++ prop, "-moz-" ++ prop, prop])
  }

  /* -webkit- & -ms- prefixes */
  | "writing-mode"
  | "scroll-snap-type"
  | "scroll-snap-points-x"
  | "scroll-snap-points-y"
  | "scroll-snap-destination"
  | "scroll-snap-coordinate"
  | "flow-into"
  | "flow-from"
  | "region-fragment"
  | "text-size-adjust" => {
    Some(["-webkit-" ++ prop, "-ms-" ++ prop, prop])
  }

  /* -moz- prefixes */
  | "text-align-last"
  | "tab-size" => {
    Some(["-moz-" ++ prop, prop])
  }

  /* -ms- prefixes */
  | "wrap-flow"
  | "wrap-through"
  | "wrap-margin"
  | "grid-template-columns"
  | "grid-template-rows"
  | "grid-template-areas"
  | "grid-template"
  | "grid-auto-columns"
  | "grid-auto-rows"
  | "grid-auto-flow"
  | "grid"
  | "grid-row-start"
  | "grid-column-start"
  | "grid-row-end"
  | "grid-row"
  | "grid-column"
  | "grid-column-end"
  | "grid-column-gap"
  | "grid-row-gap"
  | "grid-area"
  | "grid-gap" => {
    Some(["-ms-" ++ prop, prop])
  }

  /* -webkit- & -moz- & -ms- prefixes */
  | "hyphens"
  | "user-select"
  | "break-after"
  | "break-before"
  | "break-inside" => {
    Some(["-webkit-" ++ prop, "-moz-" ++ prop, "-ms-" ++ prop, prop])
  }

  | _ => None
  }
};

/* Modes the prefixer can be in, allowing encapsulated and specialised logic */
type parserMode =
  | MainLoop
  | PrefixPropertyLoop
  | BufferLoop;

/* Running state for prefixing */
type state = {
  /* the current mode of the prefixer */
  mutable mode: parserMode,
  /* prefixed properties for the current declaration that is being prefixed */
  mutable prefixedProperties: list(string),
  /* current buffer of nodes */
  mutable nodeBuffer: LinkedList.t(Parser.node),
  /* current pointer to a linked list node */
  mutable bufferPointer: option(LinkedList.elementNode(Parser.node))
};

let prefixer = (s: Parser.parserStream) => {
  let state = {
    mode: MainLoop,
    prefixedProperties: [],
    nodeBuffer: LinkedList.create(),
    bufferPointer: None
  };

  /* collect all value nodes of the current declaration */
  let takeValueNodes = () => {
    let rec explode = (valueNodes: LinkedList.t(Parser.node)) => {
      switch (LazyStream.peek(s)) {
        | Some(Property(_))
        | Some(PropertyRef(_))
        | Some(RuleEnd) => valueNodes

        | Some(node) => {
          LazyStream.junk(s);
          LinkedList.add(node, valueNodes);
          explode(valueNodes)
        }

        | None => valueNodes
      }
    };

    explode(LinkedList.create())
  };

  /* get a prefix for a property and start the PrefixPropertyLoop */
  let rec prefixProperty = (prop: string) : option(Parser.node) => {
    switch (prefixForProp(prop)) {
    | Some(prefixedProperties) => {
      state.mode = PrefixPropertyLoop;
      state.prefixedProperties = prefixedProperties;
      state.nodeBuffer = takeValueNodes();

      prefixPropertyLoop()
    }
    | None => mainLoop()
    }
  }

  /* pass through all nodes until a property is encountered */
  and mainLoop = () : option(Parser.node) => {
    switch (LazyStream.next(s)) {
    | Some(Property(prop)) => prefixProperty(prop)
    | Some(node) => Some(node)
    | None => None
    }
  }

  /* output the next prefixed property and start the buffer loop or switch back to the MainLoop */
  and prefixPropertyLoop = () : option(Parser.node) => {
    switch (state.prefixedProperties) {
      | [prop, ...rest] => {
        state.prefixedProperties = rest;
        state.bufferPointer = state.nodeBuffer.head;
        state.mode = BufferLoop;

        Some(Property(prop))
      }
      | [] => {
        state.mode = MainLoop;
        mainLoop()
      }
    }
  };

  let bufferLoop = () : option(Parser.node) => {
    switch (state.bufferPointer) {
    | Some(element) => {
      state.bufferPointer = element.next;
      Some(element.value)
    }
    | None => prefixPropertyLoop()
    }
  };

  let next: [@bs] (unit => option(Parser.node)) = [@bs] (() => {
    switch state.mode {
    | MainLoop => mainLoop()
    | PrefixPropertyLoop => prefixPropertyLoop()
    | BufferLoop => bufferLoop()
    }
  });

  LazyStream.from(next)
};
