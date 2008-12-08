(******************************************************************************)
(*      Copyright (c) 2008, Sebastien Mondet                                  *)
(*                                                                            *)
(*      Permission is hereby granted, free of charge, to any person           *)
(*      obtaining a copy of this software and associated documentation        *)
(*      files (the "Software"), to deal in the Software without               *)
(*      restriction, including without limitation the rights to use,          *)
(*      copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*      copies of the Software, and to permit persons to whom the             *)
(*      Software is furnished to do so, subject to the following              *)
(*      conditions:                                                           *)
(*                                                                            *)
(*      The above copyright notice and this permission notice shall be        *)
(*      included in all copies or substantial portions of the Software.       *)
(*                                                                            *)
(*      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*      OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*      HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*      FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*      OTHER DEALINGS IN THE SOFTWARE.                                       *)
(******************************************************************************)

(**
An imperative circular list with many {e O(1)} operations
@author Sebastien Mondet (sebmdt.googlepages.com)
*)

(**
Features:
- no [Obj.magic], no [==] and no [!=] (except in the [Unsafe] submodule)
- many {e O(1)} operations: [length], [add_head], [add_last], [see_head], [see_last], [take_head], [append]...
- memory usage overhead (compared to OCaml list): 7 values (i.e. 7 x (4 or 8) bytes)

Warning:
- Do not use structural equality on lists ([(=)] or [compare]) it may infinitely loop


*)

(** {3 The basic types and exceptions } *)

type 'a iList
(** The imperative circular list type *)

type ('a, 'b) return = [ `other of 'b | `value of 'a ]
(** The returned type of non-unit functions *)

val get_return_value : ('a, 'b) return -> 'a
(** Get the return value or raise a Failure exception *)

exception Error of [ `empty | `index_out_of_bounds ]
(** The exception raised by *_exn functions *)


(** {3 'Empty' Functions} *)

val empty: unit -> 'a iList
(** Create a new empty list *)

val is_empty: 'a iList -> bool
(** Check the list for emptyness *)

val to_empty: 'a iList -> unit
(** Make the list empty *)

(** {3 Basic {e O(1)} Functions} *)

val length: 'a iList -> int
(** Get the length of the list ({e O(1)}) *)

val add_head : 'a iList -> 'a -> unit
(** Add an element at the head of the list ({e O(1)}) *)

val add_last : 'a iList -> 'a -> unit
(** Add an element at the end of the list ({e O(1)}) *)

val see_head : 'a iList -> ('a, [ `empty ]) return
(** Get the element at the head of the list ({e O(1)}) *)

val see_head_exn : 'a iList -> 'a
(** Same as [see_head] but can raise [Error `empty] *)

val see_last : 'a iList -> ('a, [ `empty ]) return
(** Get the element at the end of the list ({e O(1)}) *)

val see_last_exn : 'a iList -> 'a
(** Same as [see_last] but can raise [Error `empty] *)

val take_head : 'a iList -> ('a, [ `empty ]) return
(** Remove (and get) the element at the head of the list ({e O(1)}) *)

val take_head_exn : 'a iList -> 'a
(** Same as [take_head] but can raise [Error `empty] *)

val append: 'a iList -> 'a iList -> unit
(** [append l1 l2] Adds the content of l2 at the end of l1 and empties l2 ({e O(1)}) *)

(** {3 N-th element accessors} *)

val get_nth : 'a iList -> int -> ('a, [`index_out_of_bounds]) return
(** Get the n-th element of the list ({e O(n)}) *)

val get_nth_exn : 'a iList -> int -> 'a
(** Same as [get_nth] but can raise [(Error `index_out_of_bounds)]  *)

val set_nth : 'a iList -> int -> 'a -> (unit, [`index_out_of_bounds ]) return
(** Modify the n-th element of the list ({e O(n)}) *)

val set_nth_exn : 'a iList -> int -> 'a -> unit
(** Same as [set_nth] but can raise [(Error `index_out_of_bounds)] *)


(** {3 Other Functions} *)

val iter : 'a iList -> f:('a -> unit) -> unit
(** The "classic" iteration function *)

val transform : 'a iList -> f:('a -> 'a) -> unit
(** Iter and transform the list contents *)

val map : 'a iList -> f:('a -> 'b) -> 'b iList
(** Create a new list with a function *)

val copy : 'a iList -> 'a iList
(** Copy the list ({e O(length l)}) *)

val equals: ?cmp:('a -> 'a -> bool) -> 'a iList -> 'a iList -> bool
(** Test the equality between two lists (worst case = "true": {e O(length l)})) *)

val map_of_list :'a list ->  f:('a -> 'b) -> 'b iList
(** [map_of_list l f] Applies f to all elements of l creating a new iList *)

val map_to_list_rev : 'a iList -> f:('a -> 'b) -> 'b list
(** [map_to_list_rev l f] Applies f to all elements of l creating a new List.t
 (inversed order) *)

val map_to_list : 'a iList -> f:('a -> 'b) -> 'b list
(** [map_to_list l f] Applies f to all elements of l creating a new List.t
(there's a call to [List.rev] => {e O(2n)}) *)

val copy_rev : 'a iList -> 'a iList
(** Builds a new iList in reverse order *)

val reverse : 'a iList -> unit
(** Imperatively reverses its operand ({e O(n)}) *)

val of_list : 'a list -> 'a iList
(** Build an iList from a list *)

val to_list : 'a iList -> 'a list
(** Render an iList to a list (uses [List.rev] => {e O(2n)}) *)

(** {3 Removing elements} *)

val remove_nth: 'a iList -> int -> ('a, [`index_out_of_bounds ]) return
(** Removes and returns the n-th element of the list *)

val remove_if: 'a iList -> f:('a -> bool) -> unit
(** Removes the first element that satisfies [f e = true] *)

val filter: 'a iList -> f:('a -> bool) -> unit
(** Removes all the elements that do not satisfy [f e = true] *)

val remove_while: 'a iList -> f:('a -> bool) -> unit
(** Removes all the elements that satisfy [f e = true] *)

(** {3 Searching functions} *)

val find : 'a iList -> f:('a -> bool) -> 'a option
(** [(find l f)] finds the first element of the list satisfying f
(returns [None] if not found) *)

val find_all : 'a iList -> f:('a -> bool) -> 'a iList
(** Returns a new iList containing all elements which satisfy [f] *)

(** {3 The 'Unsafe' submodule} *)


(** Unsafe functions (but which can be faster) *)
module Unsafe : sig
    
    val convert_to_list : 'a iList -> 'a list
    (** Convert to a classic list, and empty the original
     (it uses [Obj.magic] but it's {e O(1)}) *)

end


(** {3 Semantic renaming modules} *)

(** They provide some [*_exn] functions with names:
[create], [push], [pop], [peek], [is_empty] *)

(** The Stack ([push] is [add_head]) *)
module LIFO :
  sig
    val create : unit -> 'a iList
    val push : 'a iList -> 'a -> unit
    val pop : 'a iList -> 'a
    val peek : 'a iList -> 'a
    val is_empty : 'a iList -> bool
  end

(** The Queue ([push] is [add_last]) *)
module FIFO :
  sig
    val create : unit -> 'a iList
    val push : 'a iList -> 'a -> unit
    val pop : 'a iList -> 'a
    val peek : 'a iList -> 'a
    val is_empty : 'a iList -> bool
  end

