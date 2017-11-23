# Caveats

The parser is mostly written to be very predictable, so it adheres to all CSS specs, while still being
a little lenient where some typos are unambiguous while still adhering to the CSS syntax.

However, some cases are out of its reach to handle correctly; Either because of performance implications,
simplifications, or not to cause bundle size bloat. Those few cases are listed here.

## Property Refs are not prefixed

```js
css`
  appearance: none; /* this will be prefixed */
  ${() => 'appearance'}: none; /* this won't be prefixed */
`
```

In the above case, `appearance` is a CSS property that should be prefixed; However, since one of the goals
of Sweetsour is to preprocess CSS, we cannot prefix `appearance` when it's in a "ref". This is because until
we're stringifying ISTF, all interpolations/refs won't be evaluated. The stringifier doesn't contain any
logic to prefix your CSS properties.

This is to save size on the client, and while this might lead to some unexpected cases where you won't
receive the output you expected, this is to keep things small and fast; A clear compromise :bomb:

This might change in the future, if we decide to bundle **some** prefixers automagically. But since this
is quite hard, Sweetsour won't prefix property refs for now.
