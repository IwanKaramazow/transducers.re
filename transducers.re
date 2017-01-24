let module Naive = {
  let xmap f xf r a => xf r (f a);
  let xfilter predicate xf r a => predicate a ? xf r a : r;
  let transduce xform::xform f::f init::init coll => List.fold_left (xform f) init coll;
  /* end of the library, helper functions below */
  let compose f g => {
    let c x => f (g x);
    c
  };
  let (>>>) g f => compose f g;
  let conj xs x => xs @ [x];
  let xlist xform::xform init::init=[] coll => List.fold_left (xform conj) init coll;
  /* example:
       transduce (xmap (fun x => x + 1)) conj [] [1, 2, 3]; ---> [2, 3, 4]
     */
};

open Naive;

let inc x => x + 1;

let unwrap_optional x =>
  switch x {
  | Some n => n
  /* | None => 0 */
  };

let filter_numbers x =>
  switch x {
  | Some _ => true
  | None => false
  };

let maybe_numbers = [Some 1, None, Some 2, None];

let xf = xmap inc >>> xmap unwrap_optional >>> xfilter filter_numbers;

let only_incremented_numbers = xlist xf maybe_numbers;

/*
    only_incremented_numbers : list int = [2, 3]
 */
type reducingFn 'r 'a =
  | Reducer ('r => 'a => 'r) :reducingFn 'r 'a;

type transducer 'a 'b = {t: 'r .reducingFn 'r 'b => reducingFn 'r 'a};
