(******************************************************************************)
(* PROBLEM 5: WRITING TEST CASES                                              *)
(******************************************************************************)

(* The module `SetTest` defined below is a reuseable component that we'll use
   to test other modules that conform to the `SET` interface. When `SetTest`
   is instantiated with a particular set implementation, it will run all of
   its test cases against the set type defined in that implementation.  This
   means that the _same_ tests can be used for both the OrderedListSet and
   BSTSet implementations -- this makes sense because all implementations of
   `SET` should behave the same!

   Read through the module, then write your test cases in the space provided
   below. Make sure NOT to test for structural equality with sets.  Instead,
   use the equals function specified in the interface.  Your TAs will be
   grading the completeness of your tests. *)

module SetTest (SetImpl: SetInterface.SET) = struct
  ;; open SetImpl

  (* We first redefine the `run_test` and `run_failing_test` functions so that
     they prepend the name of the set we're testing to the test description. *)

  let run_test desc = Assert.run_test (debug_name ^ ": " ^ desc)
  let run_failing_test desc = Assert.run_failing_test (debug_name ^ ": " ^ desc)

  ;; print_endline ("\n--- Running tests for " ^ debug_name ^ " ---")

  (* Here are a couple of test cases to help get you started... *)

  let test () : bool =
    is_empty empty
  ;; run_test "is_empty: call on empty returns true" test

  (* Note that some tests in this test module (such as the one below) may not
     pass until all the functions they depend on are implemented. For
     instance, the test below will fail for sets whose `set_of_list` function
     is not yet implemented (even if `is_empty` is correct).  This is fine:
     the goal here is just to record all the tests that we expect will pass
     when we get around to implementing everything later. *)

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    not (is_empty s)
  ;; run_test "is_empty: non-empty set returns false" test


(* Now, it's your turn! Make sure to comprehensively test all the other
   functions defined in the `SET` interface. It will probably be helpful to
   have the file `setInterface.ml` open as you work.  Your tests should stress
   the abstract properties of what it means to be a set, as well as the
   relationships among the operations provided by the SET interface.

   One thing to be careful of: your tests should not use `=` to compare sets:
   use the `equals` function instead.

   We strongly advise you to write tests for the functions in the order they
   appear in the interface. Write tests for all of the functions here before
   you start implementing. After the tests are written, you should be able to
   implement the functions one at a time in the same order and see your tests
   incrementally pass.

   Your TAs will be manually grading the completeness of your test cases. *)

  (* ---------- Write your own test cases below. ---------- *)

(*NOTE: ALL TESTS TEST SET OF LIST*)
(*list of set tests*)

let test () : bool =
    let s = set_of_list [1; 2; 3] in
    list_of_set(s) = [1; 2; 3]
  ;; run_test "list_of_set: in order, back to list" test

  let test () : bool =
    let s = set_of_list [1] in
    list_of_set(s) = [1]
  ;; run_test "list_of_set: singleton list" test

    let test () : bool =
    let s = set_of_list [] in
    list_of_set(s) = []
  ;; run_test "list_of_set: empty list" test

  let test () : bool =
    let s = set_of_list [1; 2; 2; 3; 4] in
    list_of_set(s) = [1; 2; 3; 4]
  ;; run_test "list_of_set: duplicates, ascending order" test

  let test () : bool =
    let s = set_of_list [1; 5; 2; 6; 3] in
    list_of_set(s) = [1; 2; 3; 5; 6]
  ;; run_test "list_of_set: out of order" test

  let test () : bool =
    let s = set_of_list [1; 5; 2; 6; 6; 1; 3] in
    list_of_set(s) = [1; 2; 3; 5; 6]
  ;; run_test "list_of_set: out of order, with duplicates" test

(*add tests*)

let test () : bool =
    let s = set_of_list [] in
    let r = add(4)(s) in
    list_of_set(r) = [4]
  ;; run_test "add: to empty" test

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    let r = add(4)(s) in
    list_of_set(r) = [1; 2; 3; 4]
  ;; run_test "add: in order" test

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    let r = add(2)(s) in
    list_of_set(r) = [1; 2; 3]
  ;; run_test "add: in order, with repeat" test

  let test () : bool =
    let s = set_of_list [1] in
    let r = add(4)(s) in
    list_of_set(r) = [1; 4]
  ;; run_test "add: singleton list, no repeat" test

    let test () : bool =
    let s = set_of_list [1] in
    let r = add(1)(s) in
    list_of_set(r) = [1]
  ;; run_test "add: singleton list, repeat" test

  let test () : bool =
    let s = set_of_list [1; 5; 2; 6; 6; 1; 3] in
    let r = add(6)(s) in
    list_of_set(r) = [1; 2; 3; 5; 6]
  ;; run_test "add: out of order, with repeat" test

  let test () : bool =
    let s = set_of_list [1; 5; 2; 6; 6; 1; 3] in
    let r = add(7)(s) in
    list_of_set(r) = [1; 2; 3; 5; 6; 7]
  ;; run_test "add: out of order, without repeat" test

  (*remove tests*)

   let test () : bool =
    let s = set_of_list [1; 2; 3] in
    let r = remove(1)(s) in
    list_of_set(r) = [2; 3]
  ;; run_test "remove: last value" test

   let test () : bool =
    let s = set_of_list [1; 2; 3] in
    let r = remove(2)(s) in
    list_of_set(r) = [1; 3]
  ;; run_test "remove: middle value" test

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    let r = remove(1)(s) in
    list_of_set(r) = [2; 3]
  ;; run_test "remove: first value" test

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    let r = remove(4)(s) in
    list_of_set(r) = [1; 2; 3]
  ;; run_test "remove: value not found" test

  let test () : bool =
    let s = set_of_list [1] in
    let r = remove(1)(s) in
    list_of_set(r) = []
  ;; run_test "remove: singleton list" test

    let test () : bool =
    let s = set_of_list [] in
    let r = remove(1)(s) in
    list_of_set(r) = []
  ;; run_test "remove: from empty" test

(*member tests*)

   let test () : bool =
    let s = set_of_list [1; 2; 3] in
    member(2)(s)
  ;; run_test "member: value is present" test

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    not (member(4)(s))
  ;; run_test "member: value not found" test

    let test () : bool =
    let s = set_of_list [] in
    not (member(2)(s))
  ;; run_test "member: empty list" test

(*size tests*)

   let test () : bool =
    let s = set_of_list [1; 2; 3] in
    size(s) = 3
  ;; run_test "size: 3 values" test

  let test () : bool =
    let s = set_of_list [2; 3] in
    size(s) = 2
  ;; run_test "size: 2 values" test

   let test () : bool =
    let s = set_of_list [4] in
    size(s) = 1
  ;; run_test "size: singleton" test


  let test () : bool =
    let s = set_of_list [] in
    size(s) = 0
  ;; run_test "size: empty" test

(*equals tests*)

let test () : bool =
    let s = set_of_list [] in
    let r = set_of_list [] in
    equals(r)(s)
  ;; run_test "equals: two empty" test

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    let r = set_of_list [] in
    not (equals(r)(s))
  ;; run_test "equals: one empty, one non empty" test

  let test () : bool =
    let s = set_of_list [1; 2; 4; 3] in
    let r = set_of_list [1; 2; 3; 4] in
    equals(r)(s)
  ;; run_test "equals: same, out of order" test

  let test () : bool =
    let s = set_of_list [1; 2; 5] in
    let r = set_of_list [1; 2; 5; 6] in
    not (equals(r)(s))
  ;; run_test "equals: not same, in order" test

    let test () : bool =
    let s = set_of_list [1] in
    let r = set_of_list [] in
    not (equals(r)(s))
  ;; run_test "equals: singleton list, unequal" test

  (* ---------- Write your own test cases above. ---------- *)

end

(* The rest of the file instantiates the above tests so they are
   executed for both OrderedListSet and BSTSet.  Don't modify anything
   below this comment. *)

module TestOrderedListSet = SetTest(ListSet.OrderedListSet)
;; print_newline ()

module TestBSTSet = SetTest(TreeSet.BSTSet)
;; print_newline ()
