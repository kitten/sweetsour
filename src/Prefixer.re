open IstfNode;

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
  mutable nodeBuffer: NestedList.t(node),
  /* current pointer to a linked list node */
  mutable bufferPointer: NestedList.iterator(node)
};

let prefixer = (s: nodeStream) => {
  let nodeBuffer = NestedList.create();
  let state = {
    mode: MainLoop,
    prefixedProperties: [],
    nodeBuffer,
    bufferPointer: NestedList.createIterator(nodeBuffer)
  };

  /* collect all value nodes of the current declaration */
  let takeValueNodes = () => {
    let rec explode = (valueNodes: NestedList.t(node)) => {
      switch (LazyStream.peek(s)) {
        | Some(StringNode(Property, _))
        | Some(RefNode(PropertyRef, _))
        | Some(Node(RuleEnd)) => valueNodes

        | Some(node) => {
          LazyStream.junk(s);
          NestedList.add(node, valueNodes);
          explode(valueNodes)
        }

        | None => valueNodes
      }
    };

    explode(NestedList.create())
  };

  /* get a prefix for a property and start the PrefixPropertyLoop */
  let rec prefixProperty = (node: node, prop: string) : option(node) => {
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
  and mainLoop = () : option(node) => {
    switch (LazyStream.next(s)) {
    | Some(StringNode(Property, prop) as node) => prefixProperty(node, prop)
    | x => x
    }
  }

  /* output the next prefixed property and start the buffer loop or switch back to the MainLoop */
  and prefixPropertyLoop = () : option(node) => {
    switch (state.prefixedProperties) {
      | [prop, ...rest] => {
        state.prefixedProperties = rest;
        state.bufferPointer = NestedList.createIterator(state.nodeBuffer);
        state.mode = BufferLoop;

        Some(StringNode(Property, prop))
      }
      | [] => {
        state.mode = MainLoop;
        mainLoop()
      }
    }
  };

  let bufferLoop = () : option(node) =>
    switch (NestedList.next(state.bufferPointer)) {
    | Some(_) as x => x
    | None => prefixPropertyLoop()
    };

  let next: [@bs] (unit => option(node)) = [@bs] (() => {
    switch state.mode {
    | MainLoop => mainLoop()
    | PrefixPropertyLoop => prefixPropertyLoop()
    | BufferLoop => bufferLoop()
    }
  });

  LazyStream.from(next)
};
