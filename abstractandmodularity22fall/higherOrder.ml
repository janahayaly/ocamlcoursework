;; open Assert

(* NOTE: Throughout this assignment, you may NOT use any library functions (in
   particular, any functions defined in the `List` module), unless the
   instructions specifically say to.  You will lose points if you do this. *)

(******************************************************************************)
(* PROBLEM 1: GENERIC TYPES                                                   *)
(******************************************************************************)

(* Recall that "generic types" in OCaml can be used to help abstract out
   frequently used programming patterns. Here are two functions that determine
   whether an element is part of a list. *)

let rec int_member (i: int) (l: int list) : bool =
  begin match l with
  | [] -> false
  | x :: xs -> i = x || int_member i xs
  end

let rec string_member (s: string) (l: string list) : bool =
  begin match l with
  | [] -> false
  | x :: xs -> s = x || string_member s xs
  end

(* The pattern here is pretty obvious. Note that it doesn't depend on any
   type-specific details (because the `=` operator is defined for all types).
   Let's extract the pattern into a generic function that works for ANY type
   of list. The `'a` is known as a "type variable", and allows any concrete
   type to be substituted for it everywhere in the function. Complete the
   following implementation of a generic 'member' function: *)

let rec member (x: 'a) (l: 'a list) : bool =
  begin match l with
  | [] -> false
  | hd :: tl -> hd = x || member(x)(tl)
  end
(* Notice that we can use values of any type in our test cases, but we don't
   need to test with every single type. If this function works properly for
   ints, for example, then it will work just the same for all other types. *)

let test () : bool =
  not (member false [])
;; run_test "member: empty list" test

let test () : bool =
  member 1 [2; 4; 1; 5]
;; run_test "member: element is found" test

let test () : bool =
  not (member 'q' ['a'; 'b'; 'c'])
;; run_test "member: element not found" test

let test () : bool =
  (member "hi" ["bye"; "how are you"; "hi"])
;; run_test "member: string element found" test

(* Now let's try something a bit more challenging. We first introduce the
   notion of an "association list", which is a list of pairs of keys and
   "associated" values. An example association list is below. *)

let assoc_list: (int * string) list =
  [(1100, "Java"); (1200, "OCaml"); (1210, "Java"); (2400, "C")]

(* Define a generic function `assoc` that, given a list of tuples of the form
   (key, value) and a particular key, finds its associated value. If the key
   is not present, the function should signal an error using `failwith`.  If
   there are multiple instances of a key, it should return the value
   associated with the first one. *)

let rec assoc (key: 'k) (l: ('k * 'v) list) : 'v =
  begin match l with 
  | [] -> failwith "key not found"
  | (k, v) :: tl -> if k = key then v else assoc(key)(tl)
  end

let test () : bool =
  assoc 1200 assoc_list = "OCaml"
;; run_test "assoc: key found" test

let test () : bool =
  assoc 42 assoc_list = "should fail"
;; run_failing_test "assoc: key not found" test

let test () : bool =
  assoc 7 [] = "should fail"
;; run_failing_test "assoc: empty list" test

let test () : bool =
  assoc 2400 assoc_list = "C"
;; run_test "assoc: key found for C" test

(******************************************************************************)
(* PROBLEM 2: ANONYMOUS & HIGHER-ORDER FUNCTIONS                              *)
(******************************************************************************)

(* For the remaining functions, we strongly advise that you add any test cases
   that you deem necessary to ensure the functions are correctly
   implemented. *)

(* Recall the `transform` function from lecture. It is a "higher-order
   function" because it takes another function as an argument (and applies it
   to every element of a list). *)

let rec transform (f: 'a -> 'b) (l: 'a list) : 'b list =
  begin match l with
  | [] -> []
  | x :: xs -> f x :: transform f xs
  end

(* We can use `transform` to define other functions. Write a function that
   capitalizes every string in a list of strings. You can use the function
   `String.uppercase_ascii` (of type `string -> string`) in your
   implementation.  Do not add the `rec` keyword to this definition. *)

let capitalize (l: string list) : string list =
  transform(String.uppercase_ascii)(l)

let test () : bool =
  capitalize ["a"; "list"; "of"; "words"] = ["A"; "LIST"; "OF"; "WORDS"]
;; run_test "capitalize: non-empty list works" test

let test () : bool =
  capitalize ["supercalifragilistic"] = ["SUPERCALIFRAGILISTIC"]
;; run_test "capitalize: singleton list" test

let test () : bool =
  capitalize [] = []
;; run_test "capitalize: empty list" test

(* Next, recall the higher-order function `fold`, which collapses a list into
   a single result value. *)

let rec fold (combine: 'a -> 'b -> 'b) (base: 'b) (l: 'a list) : 'b =
  begin match l with
  | [] -> base
  | x :: xs -> combine x (fold combine base xs)
  end

(* Here's an example use of `fold` to sum an integer list. Notice that
   the `x` is the element at the head of `l`, and `acc` is the
   "accumulated" result from calls to `fold` on the remainder of the list.
   The parameter `x` will be of the same type as the elements in the list, and
   `acc` (as well as the base argument) will be of the same type as the result
   of the `fold`. *)

let sum (l: int list) : int =
  fold (fun x acc -> x + acc) 0 l

(* Try your hand at implementing the `list_length` function using `fold`.  (Do
   not add `rec` to the declaration of `list_length`.) *)

let list_length (l: 'a list) : int =
  fold (fun x acc -> 1 + acc) 0 l

let test () : bool =
  list_length ["a"; "list"; "of"; "words"] = 4
;; run_test "list_length: non-empty list" test

let test () : bool =
  list_length [] = 0
;; run_test "list_length: empty list" test

(* Now, use `fold` to write a `concat` function that takes a list of lists and
   "flattens" it into a single list containing all the elements of the lists,
   in order.  It is okay to use the built-in `@` function, which appends two
   lists.  (When passing `@` as an argument to another function, you need to
   enclose it in parens to avoid confusing the parser.)  Do not add `rec` to
   the declaration of `concat`. *)

let concat (l: 'a list list) : 'a list =
  fold (fun x acc -> (x @ acc)) [] l

let test () : bool =
  concat [[]] = []
;; run_test "concat: list of empty list returns empty" test

let test () : bool =
  concat [[1; 2]; [3]; [4; 5; 6]] = [1; 2; 3; 4; 5; 6]
;; run_test "concat: list containing more than two lists" test

let test () : bool =
  concat [[4; 5; 6]] = [4; 5; 6]
;; run_test "concat: list containing one list" test

(* Use `fold` to write a function that reverses a generic list. Again,
   you may use the `@` function to append two lists. Do not add the
   `rec` keyword. *)

let reverse (l: 'a list) : 'a list =
  fold (fun x acc -> (acc @ [x])) [] l

let test () : bool =
  reverse ["you"; "are"; "how"; "hi"] = ["hi"; "how"; "are"; "you"]
;; run_test "reverse: list containing multiple elements" test

let test () : bool =
  reverse ["are"; "you"; "good"] = ["good"; "you"; "are"]
;; run_test "reverse: list containing multiple elements 2" test

let test () : bool =
  reverse ["hi"] = ["hi"]
;; run_test "reverse: singleton list" test

let test () : bool =
  reverse [] = []
;; run_test "reverse: empty list" test

(* We can use `fold` to write many other higher-order functions.

   Two common higher-order functions are `for_all` and `exists`. They both
   operate on lists of elements, applying a "predicate" (i.e., a testing
   function) that returns true or false for each element of that list.

   Use a single call to `fold` to implement each of these functions. Your
   solution in each case should be only one line long, and you may not add the
   `rec` keyword to the function declarations. (Hint: think carefully about
   why the base cases of the two functions are different.) *)

(* The `for_all` function returns true only if every element in the list `l`
   satisfies the predicate (i.e., if `pred x` is true for all `x` in `l`). *)

let for_all (pred: 'a -> bool) (l: 'a list) : bool =
  fold (fun x acc -> pred x && acc) true l

let test () : bool =
  for_all (fun x -> x > 0) []
;; run_test "for_all: empty list" test

let test () : bool =
  not (for_all (fun x -> x > 0) [1; 2; -5; 3])
;; run_test "for_all: multiple elements; returns false" test

let test () : bool =
  for_all (fun x -> x > 0) [1; 2; 5; 3]
;; run_test "for_all: multiple elements; returns true" test

let test () : bool =
  not (for_all (fun x -> x < 0) [1])
;; run_test "for_all: singlton; returns false" test

(* The `exists` function returns true only if there is some element in the
   list `l` that satisfies the predicate (i.e., `pred x` is true for at least
   one `x` in `l`). *)

let exists (pred: 'a -> bool) (l: 'a list) : bool =
  fold (fun x acc -> pred x || acc) false l

let test () : bool =
  not (exists (fun x -> x > 0) [])
;; run_test "exists: empty list" test

let test () : bool =
  exists (fun x -> x > 0) [1; 2; -5; 3]
;; run_test "exists: multiple elements; returns true" test

let test () : bool =
  not (exists (fun x -> x < 0) [1; 2; 5; 3])
;; run_test "exists: multiple elements; returns false" test

let test () : bool =
  exists (fun x -> x > 0) [2]
;; run_test "exists: singleton; returns true" test


(* The last higher-order function we'll explore in this assignment is
   `filter`, which uses a predicate function to filter out elements that don't
   satisfy a condition `pred`. *)

let rec filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  begin match l with
  | [] -> []
  | x :: xs ->
     let rest = filter pred xs in
     if pred x then x :: rest else rest
  end

(* Rewrite `filter` using `fold` so that it passes the tests below. *)

let ho_filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  fold (fun x acc -> if pred x then x :: acc else acc ) [] l

let test () : bool =
  ho_filter (fun x -> x > 0) [1; 2; -5; 3] = [1; 2; 3]
;; run_test "ho_filter: multiple elements; some are filtered" test

let test () : bool =
  ho_filter (fun _ -> false) ["a"; "b"; "c"] = []
;; run_test "ho_filter: multiple elements; all are filtered" test

let test () : bool =
  ho_filter (fun x -> x = "hi") ["bye"; "hi"; "sup"] = ["hi"]
;; run_test "ho_filter: multiple elements; one is unfiltered" test

let test () : bool =
  ho_filter (fun x -> x < 1) [] = []
;; run_test "ho_filter: empty list" test