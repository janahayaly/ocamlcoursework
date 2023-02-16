(******************************************************************************)
(* Problem 8: Deques **********************************************************)
(******************************************************************************)

;; open Assert
;; open Imp

(* A deque (pronounced 'deck') is like a queue, but with the ability
   to efficiently insert and delete at both ends. That is, deques are
   "double-ended queues".

   Your task in this problem is to implement deques using doubly
   linked lists of nodes in the heap--i.e., each node will have
   pointers to both the next AND the previous nodes.

   Hint: When working out how to implement these functions, you will
   find it VERY HELPFUL to work things out on paper using
   "box and pointer" diagrams in the style of the Abstract Stack
   Machine. *)

(* A deque is represented here like a linked queue but with links in
   both directions.

   A dqnode contains a value and references to the next AND PREVIOUS
   nodes in the structure. *)

type 'a dqnode = {
  v: 'a;
  mutable next: 'a dqnode option;
  mutable prev: 'a dqnode option;
}

type 'a deque = {
  mutable head: 'a dqnode option;
  mutable tail: 'a dqnode option;
}


(**********************************************)
(****** DEQUE implementation INVARIANT ********)

(* INVARIANT: either...

   The deque is empty, and the head and tail are both None, or

   the deque is non-empty, and
   - head = Some n1 and tail = Some n2, where
      (a) n2 is reachable from n1 by following 'next' pointers
      (b) n2.next = None   (there is no element after the tail)

      (c) n1 is reachable from n2 by following 'prev' pointers
      (d) n1.prev = None   (there is no element before the head)

   - for every node n in the deque:
      (e) if n.next = Some m then
            m.prev = Some n

      (f) if n.prev = Some m then
            m.next = Some n *)

(* The next few functions check whether a given deque satisfies the
   deque invariants. You do not have to write the code for these, but
   do take some time to read over them and understand how they
   work. You will find yourself writing functions with similar
   structure below. *)

(* Helper function to check for aliasing of dqnode lists *)

let alias (x: 'a dqnode) : 'a dqnode list -> bool =
  List.fold_left (fun acc h -> x == h || acc) false

(* Helper function for 'valid' that checks deque invariants "from head
   to tail."  The tail of the deque is returned if traversal is
   successful. *)
let check_to_tail (n: 'a dqnode) : 'a dqnode option =
  let rec loop (curr: 'a dqnode) (seen: 'a dqnode list) : 'a dqnode option =
    begin match curr.next with
    | None -> Some curr
    | Some m ->
       begin match m.prev with
       | None -> None
       | Some mp ->
          if mp != curr || alias curr seen
          then None
          else loop m (curr :: seen)
       end
    end
  in loop n []

(* Helper function for 'valid' that checks deque invariants "from
   tail to head."  The head of the deque is returned if traversal is
   successful. *)
let check_to_head (n: 'a dqnode) : 'a dqnode option =
  let rec loop (curr: 'a dqnode) (seen: 'a dqnode list) : 'a dqnode option =
    begin match curr.prev with
    | None -> Some curr
    | Some m ->
        begin match m.next with
        | None -> None
        | Some mn ->
            if mn != curr || alias curr seen
            then None
            else loop m (curr :: seen)
        end
    end
  in loop n []

(* Function that checks whether a given deque is valid. The function
   performs a check of invariants in both directions down the deque,
   and then checks if the head and tail of the given deque match those
   returned by check_to_tail and check_to_head. *)
let valid (d: 'a deque) : bool =
  begin match d.head, d.tail with
  | None, None -> true
  | Some h, Some t ->
      begin match check_to_tail h, check_to_head t with
      | Some n2, Some n1 -> n2 == t && n1 == h
      | _, _ -> false
      end
  | _, _ -> false
  end


(*****************************************)
(****** DEQUE examples and cycles ********)

(* Write a function that returns an empty deque. *)
let create () : 'a deque =
  { head = None; tail = None }

(* We've done this one for you. Do not change it! *)
let is_empty (d: 'a deque) : bool =
  d.head = None

(* This function directly builds a *valid* deque of length 1. *)
let deque_1 () : int deque =
  let n = { v = 1; prev = None; next = None } in
  { head = Some n; tail = Some n }

(* Write a function that builds a *valid* deque of length 2. *)
let deque_12 () : int deque =
  let m = { v = 1; prev = None; next = None} in
  let n = { v = 2; prev = None; next = None} in
  m.next <- Some n;
  n.prev <- Some m;
  let q = { head = Some m; tail = Some n} in 
  q


(* Note that if you draw the result of the ASM for a correct deque_12,
   you'll get a heap with a *cycle* -- i.e., by following a
   combination of next or prev pointers you can get back to where you
   started. Deques are different from queues in that valid deques with
   length >= 2 will ALWAYS have cycles, whereas ordinary queues with
   cycles are invalid. *)



(* Write down an *invalid* deque that does not contain any cycles.  *)
let invalid_acyclic () : int deque =
  { head = Some { v = 1; prev = None; next = None}; 
  tail = Some { v = 2; prev = None; next = None} }

(* Of course, not all deques with cycles are valid. Write down an invalid
   deque that does contain a cycle. *)
let cycle () : int deque =
  let m = { v = 1; prev = None; next = None} in
  let n = { v = 2; prev = None; next = None} in
  m.next <- Some n;
  n.next <- Some m;
  let q = { head = Some m; tail = Some n} in 
  q

(* Here are two functions that help get values from the deque without
   mutating it. We use these helper functions for testing purposes. *)
let peek_head (q: 'a deque) : 'a =
  if not (valid q) then failwith "peek_head: given invalid deque";
  begin match q.head with
  | None -> failwith "peek_head called on empty deque"
  | Some hd -> hd.v
  end

let peek_tail (q: 'a deque) : 'a =
  if not (valid q) then failwith "peek_tail: given invalid deque";
  begin match q.tail with
  | None -> failwith "peek_tail called on empty deque"
  | Some tl -> tl.v
  end

(**********************************************)
(****** DEQUE implementation exercises ********)

(* NOTE: add all your tests for the following deque functions to
   dequeTest.ml.  Look for specified sections to write certain
   tests. *)

(* Write a function to return a list of elements in the deque, ordered
   from the head to the tail.

   This function should create the list in a single iteration through
   the deque with no additional processing. This means you will get
   full credit only if you implement to_list using tail recursion. Do
   not use or reimplement the List library functions, @, or
   reverse (List.rev).

   HINT: Why are those List operations needed for singly-linked queues? *)
let to_list (q: 'a deque) : 'a list =
  if not (valid q) then failwith "to_list: given invalid deque";
  let rec loop (no:'a dqnode option) (acc: 'a list) : 'a list =
      begin match no with
        | None -> []
        | Some n -> n.v :: loop n.next (acc)
      end
    in
    loop q.head []

(* Write two functions, insert_head and insert_tail, for adding elements to
   the head and tail of a deque, respectively.

   You may assume that the deque input satisfies the deque invariants.

   Just as with queues, you should update the deque in place, and make sure
   to maintain the invariants. *)
let insert_head (x: 'a) (q: 'a deque) : unit =
  if not (valid q) then failwith "insert_head: given invalid deque";
  let m = {v = x; next = None; prev = None} in
  begin match q.head with 
  | None -> q.head <- Some m; q.tail <- Some m
  | Some n -> n.prev <- Some m; m.next <- Some n; q.head <- Some m
  end

let insert_tail (x: 'a) (q: 'a deque) : unit =
  if not (valid q) then failwith "insert_tail: given invalid deque";
  let m = {v = x; next = None; prev = None} in
  begin match q.tail with 
  | None -> q.head <- Some m; q.tail <- Some m
  | Some n -> n.next <- Some m; m.prev <- Some n; q.tail <- Some m
  end


(* Remember the "Understanding the Problem" part of the design process
   -- you want to ask yourself "What are the relevant concepts in the
   informal description?  How do the concepts relate to each other?"
   Before writing the next four functions, take a look at what
   operations these functions might have in common.

   All of these functions can be written individually without a helper
   function, but you can write a helper function in less than 10 lines
   that you can reuse for all four. This helper function is optional,
   but if you find yourself repeating the same process over and over,
   you may want to consider writing it!  *)


(* Write two functions, remove_head and remove_tail, that remove and
   then return the first and last elements of a deque, respectively.

   These functions should fail if the deque is empty.

   Again, you should update the deque in place, and be sure to
   maintain the deque invariants.

   Your TAs will be manually grading your test cases for these two
   functions.  Make sure you've written them in dequeTest.ml! *)
let remove_head (q: 'a deque) : 'a =
  if not (valid q) then failwith "remove_head: given invalid deque";
  begin match q.head with
  | None -> failwith "deque is empty"
  | Some n ->  
    let oldhead = n.v in
    begin match n.next with
    | None -> q.head <- None; q.tail <- None; oldhead
    | Some m -> q.head <- Some m; m.prev <- None; n.next <- None; oldhead
    end
  end

let remove_tail (q: 'a deque) : 'a =
  if not (valid q) then failwith "remove_tail: given invalid deque";
  begin match q.tail with
  | None -> failwith "deque is empty"
  | Some n ->  
    let oldtail = n.v in
    begin match n.prev with
    | None -> q.head <- None; q.tail <- None; oldtail
    | Some m -> q.tail <- Some m; m.next <- None; n.prev <- None; oldtail
    end
  end

(*DELETES ARE PRODUCING INVALID QUEUES AFTER THEY REMOVE SMTH*)
(* Write a function that deletes the last (nearest the tail)
   occurrence of a given element from a deque.

   You will get full credit only if you implement delete_last using
   tail recursion.

   Your TAs will be manually grading your test cases for this
   function. Remember to add tests to the part marked in dequeTest.ml
   before completing this problem. Use structural equality here
   because the element "v" doesn't have to be reference equal to the
   element you want to delete. *)
let delete_last (v: 'a) (q: 'a deque) : unit =
  if not (valid q) then failwith "delete_last: given invalid deque";
  let rec loop (curr:'a dqnode option) (next:'a dqnode option) : unit =
      begin match curr with
        | None -> ()
        | Some n -> if n.v = v then (
          begin match n.prev with
            | None -> 
            begin match n.next with
              | None -> q.head <- None; q.tail <- None
              | Some x -> q.head <- Some x; x.prev <- None; n.next <- None
            end
            | Some m -> 
            begin match n.next with
              | None -> q.tail <- Some m; m.next <- None; n.prev <- None
              | Some x -> m.next <- Some x; x.prev <- Some m
            end
          end)
        else loop n.prev curr
      end
    in
    loop q.tail None

(* Write a function that deletes the first (nearest the head)
   occurrence of a given element from a deque.

   You will get full credit only if you implement delete_first using
   tail recursion.

   Your TAs will be manually grading your test cases for this
   function. Remember to add tests to the part marked in dequeTest.ml
   before completing this problem. Use structural equality. *)
let delete_first (v: 'a) (q: 'a deque) : unit =
  if not (valid q) then failwith "delete_first: given invalid deque";
  let rec loop (curr:'a dqnode option) (prev:'a dqnode option) : unit =
      begin match curr with
        | None -> ()
        | Some n -> if n.v = v then
        begin match n.next with
          | None -> 
            begin match n.prev with
              | None -> q.head <- None; q.tail <- None
              | Some x -> q.tail <- Some x; x.next <- None; n.prev <- None;
            end
          | Some m -> 
            begin match n.prev with
              | None -> q.head <- Some m; m.prev <- None; n.next <- None
              | Some x -> x.next <- Some m; m.prev <- Some x
            end
        end
        else loop n.next curr
      end
    in
    loop q.head None

(* Write a function to mutate the given deque in-place, so that the
   order of the element is reversed.

   In-place means that, after reversal, the deque uses the same qnodes
   as before it started.

   It is possible to satisfy the tests for this problem without doing
   an in-place reversal (for example, by removing all of the elements,
   storing them in a list, reversing the list and then inserting the
   elements back). However, this is not the solution we are looking
   for and you will lose points from your style grade if you submit
   it.

   You will get full credit only if you implement reverse using tail
   call recursion.

   Your TAs will be manually grading your test cases for this
   problem. Make sure you've written them in dequeTest.ml! *)

let reverse (q: 'a deque) : unit =
  if not (valid q) then failwith "reverse: given invalid deque";
  let rec loop (curr:'a dqnode option) : unit =
      begin match curr with
        | None -> let head = q.head in
                  let tail = q.tail in
                  q.tail <- head; q.head <- tail;
        | Some n -> let next = n.next in
                    let prev = n.prev in
                    n.prev <- next; n.next <- prev; loop next
      end
    in
    loop q.head

(****************************************************************)
(**** Deque kudos problem (ho_valid) ****************************)
(****************************************************************)

(* Our definitions of check_to_tail and check_to_head above share a
   lot of code.  Write a higher-order function that will allow you to
   write both check_to_tail and check_to_head by calling this
   function. Make sure your new implementation behaves like the old
   one.

   Note: The required functionality will provide you with the ability
   to do the loop in both directions. You could also try making the
   functionality of the loop a parameter of the function like fold
   does for lists, although we won't be testing for that. *)

let ho_check (n: 'a dqnode) (one_way: 'a dqnode -> 'a dqnode option)
             (other_way: 'a dqnode -> 'a dqnode option) : 'a dqnode option =
  let rec loop (curr: 'a dqnode) (seen: 'a dqnode list) : 'a dqnode option =
    failwith "ho_check unimplemented"
  in loop n []

(* Next, implement ho_check_to_tail using ho_check. Add tests to make
   sure ho_check_to_tail works exactly like check_to_tail. *)
let ho_check_to_tail (n: 'a dqnode) : 'a dqnode option =
  failwith "ho_check_to_tail unimplemented"

(* Now, do the same thing for check_to_head. *)
let ho_check_to_head (n: 'a dqnode) : 'a dqnode option =
  failwith "ho_check_to_tail unimplemented"

(* Finally we provide ho_valid, a variant of valid that uses the ho_
   versions of the checks. *)
let ho_valid (d: 'a deque) : bool =
  begin match d.head, d.tail with
  | None, None -> true
  | Some h, Some t ->
      begin match ho_check_to_tail h, ho_check_to_head t with
      | Some n2, Some n1 -> n2 == t && n1 == h
      | _, _ -> false
      end
  | _, _ -> false
  end
