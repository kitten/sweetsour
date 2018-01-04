# Status

## Parser

The following CSS features of the spec are implemented and parsed correctly
into ISTF:

> ~struck through items~ will be **temporarily unsupported** for the next release

- [x] Rules
  - [x] Style Rules
- [ ] At-Rules
  - [ ] ~At-Rule Statements~
    - [ ] ~Charset Rules~
    - [ ] ~Import Rules~
    - [ ] ~Namespace Rules~
  - [x] Nested, Conditional At-Rules
    - [x] Media Rules
    - [x] Supports Rules
    - [x] Document Rules
  - [x] Regular, Nested At-Rules
    - [x] FontFace Rules
    - [ ] ~Page Rules~
    - [x] Keyframe(s) Rules
    - [ ] ~Margin Rules~
    - [x] Counter Style Rules
    - [ ] ~Font Feature Value Rules~
    - [x] Viewport Rules
    - [ ] ~Region Style Rules~
- [x] Selectors
  - [x] Parent Selectors
  - [x] Universal Selectors
  - [x] Selector Combinators
    - [x] Space Combinator
    - [x] (Doubled) Child Combinator
    - [x] Next Sibling Combinator
    - [x] Subsequent Sibling Combinator
  - [x] Compound Selectors & Chaining (`,`)
  - [x] Pseudo Selectors (classes)
  - [x] Pseudo Selector Functions
  - [x] Strings
  - [x] Attribute selectors
    - [x] Attribute name shorthand
    - [x] Attribute case sensitivity
    - [x] Attribute operators
    - [x] Attribute string values
- [x] Declarations
  - [x] Properties
  - [x] Values & Compound values
  - [x] Functions
  - [x] Strings
- [x] Interpolations
  - [x] Value interpolations
  - [x] Property interpolations
  - [x] Selector interpolations
  - [x] Value String interpolations
  - [x] Attribute Selector interpolations
  - [x] Partial interpolation (heuristic)
