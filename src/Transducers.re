module Naive = {
  type transduce('a, 'b, 'c) =
    (~xform: ('a, 'b, 'c) => 'b, ~f: 'a, ~init: 'b, list('c)) => 'b;
  type xmap('a, 'b, 'c, 'd) = ('a => 'b, ('c, 'b) => 'd, 'c, 'a) => 'd;
  type xfilter('a, 'b, 'bool) = ('a => bool, ('b, 'a) => 'b, 'b, 'a) => 'b;

  type compose('a, 'b, 'c) = ('a => 'b, 'c => 'a, 'c) => 'b;
  type conj('a) = (list('a), 'a) => list('a);
  type xlist('a, 'b, 'c) =
    (
      ~xform: ((list('a), 'a) => list('a), list('b), 'c) => list('b),
      ~init: list('b)=?,
      list('c)
    ) =>
    list('b);
  let xmap = (f, xf, r, a) => xf(r, f(a));
  let xfilter = (predicate, xf, r, a) => predicate(a) ? xf(r, a) : r;
  let transduce = (~xform, ~f, ~init, coll) =>
    List.fold_left(xform(f), init, coll); /* end of the library, helper functions below */
  let compose:compose('a, 'b, 'c) = (f, g) => {
    let c = x => f(g(x));
    c;
  };
  let (>>>) = (g, f) => compose(f, g);
  let conj:conj('a) = (xs, x) => xs @ [x];
  let xlist:xlist('a, 'b, 'c)  = (~xform, ~init=[], coll) =>
    List.fold_left(xform(conj), init, coll);
     /* example:
       transduce(xmap((x) => x + 1), conj, [], [1, 2, 3]); ---> [2, 3, 4]
              
       or updated?
       
      transduce(~xform=xmap(x => x + 1), [], [1, 2, 3]);
     */
};

open Naive;

let inc = x => x + 1;

let unwrap_optional = x =>
  switch (x) {
  | Some(n) => n
  | None => 0
  };

let filter_numbers = x =>
  switch (x) {
  | Some(_) => true
  | None => false
  };

let maybe_numbers = [Some(1), None, Some(2), None];

let xf = xmap(inc) >>> xmap(unwrap_optional) >>> xfilter(filter_numbers);

let only_incremented_numbers = xlist(~xform=xf, maybe_numbers); /*
    only_incremented_numbers : list int = [2, 3]
 */

type reducingFn('r, 'a) =
  | Reducer(('r, 'a) => 'r): reducingFn('r, 'a);

type transducer('a, 'b) = {
  t: 'r. reducingFn('r, 'b) => reducingFn('r, 'a),
};
