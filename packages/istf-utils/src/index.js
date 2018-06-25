const isIdentifier = ([kind]) => (
  kind === 0 ||
  kind === 26
);

const isRef = ([kind]) => (
  kind === 20 ||
  kind === 21 ||
  kind === 22 ||
  kind === 23 ||
  kind === 29 ||
  kind === 32 ||
  kind === 34
);

const isString = ([kind]) => (
  kind === 3 ||
  kind === 13 ||
  kind === 14 ||
  kind === 17 ||
  kind === 24 ||
  kind === 28 ||
  kind === 30 ||
  kind === 31 ||
  kind === 33
);

const isEmpty = ([kind]) => (
  kind === 1 ||
  kind === 4 ||
  kind === 5 ||
  kind === 6 ||
  kind === 7 ||
  kind === 8 ||
  kind === 9 ||
  kind === 10 ||
  kind === 11 ||
  kind === 12 ||
  kind === 15 ||
  kind === 16 ||
  kind === 18 ||
  kind === 25 ||
  kind === 27 ||
  kind === 35 ||
  kind === 36 ||
  kind === 37 ||
  kind === 38
);

module.exports = {
  isIdentifier,
  isRef,
  isString,
  isEmpty
};
