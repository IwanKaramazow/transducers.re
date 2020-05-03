module Naive: {
  let xmap: ('a => 'b, ('c, 'b) => 'd, 'c, 'a) => 'd;
  let xfilter: ('a => bool, ('b, 'a) => 'b, 'b, 'a) => 'b;
  let transduce:
    (~xform: ('a, 'b, 'c) => 'b, ~f: 'a, ~init: 'b, list('c)) => 'b;
  let compose: ('a => 'b, 'c => 'a, 'c) => 'b;
  let (>>>): ('a => 'b, 'b => 'c, 'a) => 'c;
  let conj: (list('a), 'a) => list('a);
  let xlist:
    (
      ~xform: ((list('a), 'a) => list('a), list('b), 'c) => list('b),
      ~init: list('b)=?,
      list('c)
    ) =>
    list('b);
};
let inc: int => int;
let unwrap_optional: option(int) => int;
let filter_numbers: option('a) => bool;
let maybe_numbers: list(option(int));
let xf:
  ((list(int), int) => list(int), list(int), option(int)) => list(int);
let only_incremented_numbers: list(int);
type reducingFn('r, 'a) =
  | Reducer(('r, 'a) => 'r): reducingFn('r, 'a);
type transducer('a, 'b) = {
  t: 'r. reducingFn('r, 'b) => reducingFn('r, 'a),
};
