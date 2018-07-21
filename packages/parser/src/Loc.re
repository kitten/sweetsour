type position = {
  row: int,
  offset: int
};

type t = {
  start: position,
  _end: position
};

let makePos = (): position => {
  row: 1,
  offset: 0
};

let advanceRow = (p: position): position => {
  row: p.row + 1,
  offset: 0
};

let advanceOffset = (p: position): position => {
  row: p.row,
  offset: p.offset + 1
};

let make = (start: position, _end: position) => {
  start,
  _end
};
