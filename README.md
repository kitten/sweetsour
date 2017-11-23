<p align="center">
  <img alt="Sweetsour" src="/docs/logo.png?raw=true" width="200px" />
</p>
<h1 align="center">Sweetsour</h1>
<p align="center">
  A template string CSS parser and pipeline adhering to the ISTF spec 🍭
</p>

<br />

This is a reference implementation of a CSS (-in-JS) parser, accepting both CSS
strings and interpolations, and outputting ISTF, according to the current
[ISTF specification](https://github.com/cssinjs/istf-spec).

## Status

**Work in Progress: Not ready for any use!**

Currently work is ongoing to build: **The parser (~60%)**

- [ ] Parser
- [ ] Transform tool with plugin support
- [ ] Reference Babel plugin
- [ ] Flattener for nested CSS (plugin)
- [ ] Static prefixer (plugin)

## Motivation

The project aims to build a common and reusuable pipeline for processing CSS, including
interleaved interpolations, for CSS-in-JS libraries; This involves processing CSS-in-JS code,
turning it into ISTF, and to provide a pipeline to then transform this code during either
the build time (preprocessing), or runtime (postprocessing).

This will enable a couple of things. First of all, ISTF is easy to parse and can be targeted
by plugins, which means that language-level features can easily be implemented while not breaking
interpolations.

Second, all CSS is parsed, which means that the parser will raise an error, if something's wrong
with the code that's been written.

And lastly, ISTF is built to be very efficient and is, unlike ASTs, supposed to be processed during
the runtime, which basically means it's supposed to be fast and can be stringified very efficiently
back to CSS.

This makes CSS (-in-JS) more accessible for transformations and new features, and should make
the runtime a lot faster, since stringification is optimised and all transformations only have
to be run once.

[**More details about the motivation behind the project can be found in the Motivation doc**](./docs/motivation.md)

## Goals

The goals of Sweetsour include:

- Providing a common CSS-in-JS pipeline
- Enabling stable and fast preprocessing of CSS during buildtime
- Creating a parser able to parse CSS including interpolations
- Allowing optional runtime-only support i.e. using Sweetsour in the browser

As opposed to other project which are fairly similar, like for instance PostCSS,
ISTF allows the parser to leave markers for interpolations (typically JS / Babel AST)
right inside of the rest of the CSS nodes.
The format also breaks CSS down even further than PostCSS, providing tokens for each
language feature like at rules, functions, values, attribute selectors, and more.
This is achieved by splitting the input up into individual characters and interleaving
them with the interpolation input.

The project will provide a couple of core parts:

- Parser
- Transform tool accepting plugins
- Static Prefixer (plugin)
- Flattener for nested CSS (plugin)
- Stringifier turning ISTF back into CSS

