type prefix =
  | Webkit
  | Moz
  | Ms
  | WebkitMoz
  | WebkitMs
  | All
  | None;

let prefixForProp = (prop: string) : prefix => {
  switch (prop) {
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
  | "transition-timing-function" => Webkit

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
  | "column-width" => WebkitMoz

  | "writing-mode"
  | "scroll-snap-type"
  | "scroll-snap-points-x"
  | "scroll-snap-points-y"
  | "scroll-snap-destination"
  | "scroll-snap-coordinate"
  | "flow-into"
  | "flow-from"
  | "region-fragment"
  | "text-size-adjust" => WebkitMs

  | "text-align-last"
  | "tab-size" => Moz

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
  | "grid-gap" => Ms

  | "hyphens"
  | "user-select"
  | "break-after"
  | "break-before"
  | "break-inside" => All

  | _ => None
  }
};

let prefixer = (s: Parser.parserStream) => {
  let prefixedStreams: ref(list(BufferStream.t(Parser.node))) = ref([]);

  let prefixSliceStream = (prop: string, s: LazyStream.t(Parser.node)) => {
    let buffer = BufferStream.from(s);
    BufferStream.put(Parser.Property(prop), buffer);
    buffer
  };

  let isEndOfDeclaration = (n: Parser.node) => {
    switch (n) {
      | Property(_) => true
      | PropertyRef(_) => true
      | RuleEnd => true
      | _ => false
    }
  };

  let prefix = (prop: string) => {
    let prefixMode = prefixForProp(prop);

    prefixedStreams := if (prefixMode === None) {
      []
    } else {
      let slice = SliceStream.from(s, [@bs] ((x) => isEndOfDeclaration(x)));

      switch (prefixMode) {
      | Webkit => [
        prefixSliceStream("-webkit-" ++ prop, [@bs] slice()),
        prefixSliceStream("" ++ prop, [@bs] slice())
      ]
      | Moz => [
        prefixSliceStream("-moz-" ++ prop, [@bs] slice()),
        prefixSliceStream("" ++ prop, [@bs] slice())
      ]
      | Ms => [
        prefixSliceStream("-ms-" ++ prop, [@bs] slice()),
        prefixSliceStream("" ++ prop, [@bs] slice())
      ]
      | WebkitMoz => [
        prefixSliceStream("-webkit-" ++ prop, [@bs] slice()),
        prefixSliceStream("-moz-" ++ prop, [@bs] slice()),
        prefixSliceStream("" ++ prop, [@bs] slice())
      ]
      | WebkitMs => [
        prefixSliceStream("-webkit-" ++ prop, [@bs] slice()),
        prefixSliceStream("-ms-" ++ prop, [@bs] slice()),
        prefixSliceStream("" ++ prop, [@bs] slice())
      ]
      | _ => [
        prefixSliceStream("-webkit-" ++ prop, [@bs] slice()),
        prefixSliceStream("-moz-" ++ prop, [@bs] slice()),
        prefixSliceStream("-ms-" ++ prop, [@bs] slice()),
        prefixSliceStream("" ++ prop, [@bs] slice())
      ]
      }
    }
  };

  let rec nextItem = () => {
    switch (prefixedStreams^) {
    | [ps, ...rest] => {
      switch (BufferStream.next(ps)) {
        | Some(x) => Some(x)
        | None => {
          prefixedStreams := rest;
          nextItem()
        }
      }
    }

    | [] => {
      switch (LazyStream.next(s)) {
      | Some(Property(prop)) => {
        prefix(prop);
        nextItem()
      }
      | Some(x) => Some(x)
      | None => None
      }
    }
    }
  };

  let next: [@bs] (unit => option(Parser.node)) = [@bs] (() => nextItem());

  LazyStream.from(next)
};
