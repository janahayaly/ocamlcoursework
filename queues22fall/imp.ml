
;; open Assert

(* Recall the `transform` and `fold` functions from hw03. They are provided
   here for you to use throughout the homework as needed. *)

let rec transform (f: 'a -> 'b) (l: 'a list) : 'b list =
  begin match l with
  | [] -> []
  | x :: xs -> f x :: transform f xs
  end

let rec fold (combine: 'a -> 'b -> 'b) (base: 'b) (l: 'a list) : 'b =
  begin match l with
  | [] -> base
  | x :: xs -> combine x (fold combine base xs)
  end

(******************************************************************************)
(* Problem 1: Options  ********************************************************)
(******************************************************************************)

(* OCaml provides a generic 'a option type, which allows an algorithm to
   indicate that it was unable to come up with a useful value to return.

      type 'a option = None | Some of 'a

   For example, consider this version of `assoc` for lists:

      let rec assoc : (key: 'k) (l : ('k * 'v) list) : 'v =
        begin match l with
        | [] -> failwith "Not_found"
        | (k2, v) :: tl -> if key = k2 then v else assoc key tl
        end

   Here the return type 'v is sort of a lie, isn't it?  If the key isn't
   found in the list, `assoc` hits a failwith instead of returning an
   actual value.

   Use options to re-implement `assoc` with a more truthful type: *)
let rec assoc (k: 'k) (l: ('k * 'v) list) : 'v option =
  begin match l with
  | [] -> None
  | (k2, v) :: tl -> if k = k2 then Some v else assoc(k)(tl)
  end

let test () : bool =
  assoc 1 [(1, 2)] = Some 2
;; run_test "assoc key exists" test

let test () : bool =
  assoc 3 [(1, 2)] = None
;; run_test "assoc key doesn't exist" test

let test () : bool =
  assoc 3 [(1, 2); (3, 4); (5, 6)] = Some 4
;; run_test "assoc key exists multiple list" test

let test () : bool =
  assoc 6 [(1, 2); (3, 4); (5, 6)] = None
;; run_test "assoc key doesn't exist multiple list" test


(* Write a function that converts an "optional optional value" into just
   an optional value.  (If you are confused by this description, just
   let the types be your guide. There is basically only one way to do
   it!) *)
let join_option (x: 'a option option) : 'a option =
  begin match x with
  | None -> None
  | Some None -> None
  | Some(Some n) -> Some n
  end

let test () : bool =
  join_option (Some (Some 3)) = Some 3
;; run_test "join_option Some option" test

let test () : bool =
  join_option (Some (Some 6)) = Some 6
;; run_test "join_option Some option 2" test

let test () : bool =
  join_option (Some (None)) = None
;; run_test "join_option Some None option" test

let test () : bool =
  join_option (None) = None
;; run_test "join_option None option" test


(* Write a function that takes a list of optional values and returns a
   list containing all of the values that are present (i.e., stripping
   off the `Some`s and dropping the `None`s). *)
let rec cat_option (l: 'a option list) : 'a list =
  begin match l with
  | [] -> []
  | None :: tl -> cat_option(tl)
  | Some n :: tl -> n :: cat_option(tl)
  end

let test () : bool =
  cat_option [Some 1; None; Some 2; Some 0; None; None] = [1;2;0]
;; run_test "cat_option list contains Some and None options" test

let test () : bool =
  cat_option [None; None; None; None] = []
;; run_test "cat_option list contains all None options" test

let test () : bool =
  cat_option [None; Some 3; Some 5; Some 0; None; None; Some 8] = [3;5;0;8]
;; run_test "cat_option list contains Some and None options 2" test

let test () : bool =
  cat_option [] = []
;; run_test "cat_option empty list" test


(* Write a function that transforms a list with a function f and returns a list
   containing all of the values that are "present" after the transformation. *)
let rec partial_transform (f: 'a -> 'b option) (l: 'a list) : 'b list =
 begin match l with
 | [] -> []
 | hd :: tl -> 
  let res = f(hd) in
    begin match res with
    | None -> partial_transform (f)(tl)
    | Some n -> n :: partial_transform (f)(tl)
    end
  end

let test () : bool =
  let f = fun x -> if x > 0 then Some (x * x) else None in
  partial_transform f [0; -1; 2; -3] = [4]
;; run_test "partial_transform positive squaring" test

let test () : bool =
  let f = fun x -> if x < 0 then Some (2 * x) else None in
  partial_transform f [0; -1; 2; -3] = [-2; -6]
;; run_test "partial_transform doubling negatives" test

let test () : bool =
  let f = fun x -> Some (x * x * x) in
  partial_transform f [1; -1; 0] = [1; -1; 0]
;; run_test "partial_transform cubing" test

let test () : bool =
  let f = fun x -> None in
  partial_transform f [0; -1; 2; -3] = []
;; run_test "partial_transform all none" test


(******************************************************************************)
(* Problem 2: Mutability and Aliasing *****************************************)
(******************************************************************************)

(* Implement a function `iter` that calls a side-effecting function on each
   element of a list. Do not use any List library functions. *)
let rec iter (f: 'a -> unit) (l: 'a list) : unit =
  begin match l with
  | [] -> ()
  | hd :: tl -> f(hd);
  iter(f)(tl)
  end

(* In order to work with mutability, we will define a record with
   one mutable field called count. *)
type state = {mutable count: int}

(* Recall that we can now create a new variable of type state by using
   curly braces and giving a value for the count field as follows:

      let counter : state = {count = 0}

   This would create a counter for type state with its mutable field set
   to 0. *)

(* Here's a test for `iter`, using it to increment the value of count
   in a list of state types *)

let test () : bool =
  let l = [{count = 1}; {count = 2}; {count = 3}] in
  iter (fun r -> r.count <- 1 + r.count) l;
  l = [{count = 2}; {count = 3}; {count = 4}]
;; run_test "iter non-empty list" test

let test () : bool =
  let l = [{count = 1}; {count = 2}; {count = 3}] in
  iter (fun r -> r.count <- r.count * r.count) l;
  l = [{count = 1}; {count = 4}; {count = 9}]
;; run_test "iter non-empty list, squaring function" test

let test () : bool =
  let l = [{count = 3}] in
  iter (fun r -> r.count <- r.count) l;
  l = [{count = 3}]
;; run_test "iter singleton list" test


(* Now let's explore records in more detail, starting with the
   basics:

   Record types are like tuples with named fields. The type
   is written as a list of <id> : <type> pairs between {} brackets,
   meaning records can have more than one field.

   For example, if we wanted our state type to store an integer and
   a boolean, we could create it as follows:

     type state = { mutable count : int; mutable switch : boolean }

   To get the value of a field stored in a record `c`, use
   `c.field_name` (for example, `c.count` or `c.switch`). To update
   a value in `c` to some new value `x`, use `c.field_name <- x`
   (for example, `c.count <- x` where x is an integer or
   `c.switch <- y` where y is a boolean).

   The '=' operator is used to check for structural equality between
   refs, whereas '==' is used to check for "reference equality." *)


(* Write a function to increment the contents of a state record
   containing a single int field called count. The increment function
   should return the old value of the count field. *)
let state_incr (r : state) : int =
let old = r.count in
r.count <- old + 1;
old

let test () : bool =
  let r = { count = 0 } in
  state_incr r = 0 && state_incr r = 1 && r.count = 2
;; run_test "state_incr incrementing twice" test

let test () : bool =
  let r = { count = 0 } in
  state_incr r = 0 && state_incr r = 1 && state_incr r = 2 && r.count = 3
;; run_test "state_incr incrementing three times" test

let test () : bool =
  let r = { count = 0 } in
  state_incr r = 0 && r.count = 1
;; run_test "state_incr incrementing once" test


(* Write a function to swap the contents of two state record cells.
   Note that swap returns unit, and the "<-" operator will return unit.
   You can also return unit directly with "()". *)
let swap (r1: state) (r2: state) : unit =
  let store = r1.count in
  r1.count <- r2.count;
  r2.count <- store
  

let test () : bool =
  let r1 = { count = 5 } in
  let r2 = { count = 6 } in
  let _ = swap r1 r2 in
  (6, 5) = (r1.count, r2.count)
;; run_test "swap records with different contents" test

let test () : bool =
  let r1 = { count = 2 } in
  let r2 = { count = 0 } in
  let _ = swap r1 r2 in
  (0, 2) = (r1.count, r2.count)
;; run_test "swap records with different contents" test

let test () : bool =
  let r1 = { count = 4 } in
  let r2 = { count = 8 } in
  let _ = swap r1 r2 in
  (8, 4) = (r1.count, r2.count)
;; run_test "swap records with different contents" test


(* One tricky issue with mutable state is the possibilty of
   *aliasing* between mutable bindings. *)

(* Write a function that determines if two record cells are aliased by
   using the reference equality operator (==). *)
let states_aliased (r1: state) (r2: state) : bool =
  r1 == r2

let test () : bool =
  let r1 = { count = 5 } in
  let r2 = { count = 5 } in
  let b = states_aliased r1 r2 in
  (false, true, true) = (b, r1.count = 5, r2.count = 5)
;; run_test "states_aliased different records with same value not aliased" test

let test () : bool =
  let r1 = { count = 5 } in
  let r2 = r1 in
  let b = states_aliased r1 r2 in
  (true, true, true) = (b, r1.count = 5, r2.count = 5)
;; run_test "states_aliased aliased records are aliased" test

let test () : bool =
  let r1 = { count = 5 } in
  let r2 = r1 in
  let r1 = { count = 6 } in
  let b = states_aliased r1 r2 in
  (false, true, true) = (b, r1.count = 6, r2.count = 5)
;; run_test "states_aliased r1 is shadowed" test

let test () : bool =
  let r1 = { count = 5 } in
  let r2 = r1 in
  r1.count <-  6;
  let b = states_aliased r1 r2 in
  (true, true, true) = (b, r1.count = 6, r2.count = 6)
;; run_test "states_aliased r1 is replaced" test


(***** KUDOS Problem (states_aliased_kudos) *********)

(* Write another function to test whether two state records are aliased
   without using reference equality (i.e. do NOT use OCaml's '==' or
   '!=' operators for this problem). At the end of the function, both
   records *must* be in the same state they started in!  
let states_aliased_kudos (r1: state) (r2: state) : bool =
  failwith "states_aliased_kudos: unimplemented"

let test () : bool =
  let r1 = { count = 5 } in
  let r2 = { count = 5 } in
  let b = states_aliased_kudos r1 r2 in
  (false, true, true) = (b, r1.count = 5, r2.count = 5)
;; run_test "states_aliased_kudos dif. records with same value
are not aliased" test

let test () : bool =
  let r1 = { count = 5 } in
  let r2 = r1 in
  let b = states_aliased_kudos r1 r2 in
  (true, true, true) = (b, r1.count = 5, r2.count = 5)
;; run_test "states_aliased_kudos aliased records are aliased" test*)


(******************************************************************************)
(* Problem 3: Equality and Aliases ********************************************)
(******************************************************************************)

(* Write a function that, given a value x and a list of values,
   determines whether any of the elements in the list is an alias of
   x. (HINT: use reference equality!)

   Do NOT use the 'states_aliased' function to help with this problem.

*)
let rec contains_alias (x: 'a) (l: 'a list) : bool =
  begin match l with
  | [] -> false
  | hd :: tl -> hd == x || contains_alias(x)(tl)
  end

let test () : bool =
  let r1 = { count = 5 } in
  let r2 = { count = 5 } in
  not (contains_alias r1 [r2])
;; run_test "contains_alias singleton no alias" test

let test () : bool =
  let r1 = { count = 5 } in
  let r2 = { count = 5 } in
  contains_alias r1 [r2; r1]
;; run_test "contains_alias multi-element list alias exists" test

let test () : bool =
  let r1 = { count = 5 } in
  let r2 = { count = 6 } in
  contains_alias r2 [r2; r1]
;; run_test "contains_alias multi-element list alias exists 2" test

let test () : bool =
  let r1 = { count = 5 } in
  let r2 = { count = 5 } in
  let r3 = r1 in
  contains_alias r3 [r2; r1]
;; run_test "contains_alias multi-element list alias exists named diff" test

(* Let's briefly review equality...  Two values are *structurally*
   equal when the values they point to on the heap *look* the same.  -
   Two values are *reference* equal when they point to the *same*
   value on the heap

   Complete the test result below with a list of boolean values in
   such a way that the test passes. Make sure you understand why each
   comparison returns true or false before continuing to the next part
   of the homework assignment.  (Hint: If you get confused, draw an
   ASM diagram!) *)
let equality_test_results () : bool list =
  [true;
  true;
  true;
  false;
  true;
  false;
  true;
  true;
  true;
  false;
  true;
  false;
  true;
  true;
  true;
  false]

let test () : bool =
  let r = { count = 5 } in
  let o = Some r in
  [ r = r;
    r == r;
    r = { count = 5 };
    r == { count = 5 };
    { count = 5 } = { count = 5 };
    { count = 5 } == { count = 5 };
    r.count = r.count;
    r.count == r.count;
    Some r = Some r;
    Some r == Some r;
    Some r = o;
    Some r == o;
    o = o;
    o == o;
    contains_alias o [o];
    contains_alias (Some r) [Some r]
  ] = equality_test_results ()
;; run_test "interactions between == and options" test


(* Now write a function that determines whether a given value x is
   aliased with one of a list of optional values. *)
let rec contains_alias_option (x: 'a) (l: 'a option list) : bool =
  begin match l with
  | [] -> false
  | hd :: tl -> 
    begin match hd with
    | None -> contains_alias_option (x)(tl)
    | Some n -> n == x || contains_alias_option(x)(tl)
    end
  end

let test () : bool =
  let r = { count = 5 } in
  let o1 = Some r in
  contains_alias_option r [o1]
;; run_test "contains_option_alias singleton contains alias" test

let test () : bool =
  let r = { count = 5 } in
  let o2 = Some { count = 5 } in
  not (contains_alias_option r [o2])
;; run_test "contains_option_alias singleton does not contain alias" test

let test () : bool =
  let r = { count = 5 } in
  let o2 = Some { count = 5 } in
  not (contains_alias_option r [o2; None; Some { count = 9 }])
;; run_test "contains_option_alias multiple element does not contain alias" test

let test () : bool =
  let r = { count = 5 } in
  let o2 = Some r in
  contains_alias_option r [o2; None; Some { count = 9 }]
;; run_test "contains_option_alias multiple element contains alias" test