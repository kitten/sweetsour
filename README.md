<p align="center">
  <img alt="Sweetsour" src="/docs/logo.png?raw=true" width="200px" />
</p>
<h1 align="center">Sweetsour</h1>
<p align="center">
  A CSS-in-JS parser and pipeline adhering to the ISTF spec 🍭
</p>

<br />

This is a reference implementation of a CSS-in-JS parser, outputting ISTF, according
to the current [ISTF specification](https://github.com/cssinjs/istf-spec).

## Status

**Work in Progress: Not ready for any use!**

Currently work is ongoing to build: **The parser (~60%)**

- [ ] Parser
- [ ] Transform tool with plugin support
- [ ] Reference Babel plugin
- [ ] Flattener for nested CSS (plugin)
- [ ] Static prefixer (plugin)

## Goals

The project aims to build a common and reusuable pipeline for processing CSS, including
interleaved interpolations, for CSS-in-JS libraries; This involves processing CSS-in-JS code,
turning it into ISTF, and to provide a pipeline to then transform this code during either
the build time (preprocessing), or runtime (postprocessing).

The goals of ISTF and Sweetsour include:

- Providing a common CSS-in-JS pipeline
- Enabling stable and fast preprocessing of CSS during buildtime
- Creating a parser able to parse CSS including interpolations
- Allowing optional runtime-only support i.e. using Sweetsour in the browser

As opposed to other project which are fairly similar, like for instance PostCSS,
the IST format allows us to leave markers for interpolations (typically JS / Babel AST)
right inside it. The format will also break CSS down even further, not only to allow
interpolations to be injected, but also for transformations to be come easier.

The project will provide a couple of core parts:

- Parser
- Transform tool accepting plugins
- Static Prefixer (plugin)
- Flattener for nested CSS (plugin)
- Stringifier turning ISTF back into CSS

