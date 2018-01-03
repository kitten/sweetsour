let prefixForProp = (prop: string) : option(list(string)) => {
  switch (PrefixProperty.prefixForProperty(prop)) {
  | Webkit => Some(["-webkit-" ++ prop, prop])
  | WebkitMoz => Some(["-webkit-" ++ prop, "-moz-" ++ prop, prop])
  | WebkitMs => Some(["-webkit-" ++ prop, "-ms-" ++ prop, prop])
  | Moz => Some(["-moz-" ++ prop, prop])
  | Ms => Some(["-ms-" ++ prop, prop])
  | WebkitMozMs => Some(["-webkit-" ++ prop, "-moz-" ++ prop, "-ms-" ++ prop, prop])
  | NoPrefix => None
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
  let rec prefixProperty = (node: Parser.node, prop: string) : option(Parser.node) => {
    switch (prefixForProp(prop)) {
    | Some(prefixedProperties) => {
      state.mode = PrefixPropertyLoop;
      state.prefixedProperties = prefixedProperties;
      state.nodeBuffer = takeValueNodes();
      prefixPropertyLoop()
    }
    | None => {
      state.mode = MainLoop;
      Some(node)
    }
    }
  }

  /* pass through all nodes until a property is encountered */
  and mainLoop = () : option(Parser.node) => {
    switch (LazyStream.next(s)) {
    | Some(Property(prop) as node) => prefixProperty(node, prop)
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
