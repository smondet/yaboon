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
This module implements a buffer (string) accessible with "bit" precision
*)

(**
General features:
- Each bit occupies one bit in memory
- Bound checking is "exception based" and if an exception other than [(BitBuffer.Error _)] is raised, it means that you have found a bug
- Read/Write operations have been optimized for speed (by writing as much bytes "at once" as possible)
- Usage of [Int64] operations has been minimized for [write_int64] and [read_int64]
- No [Obj.magic], no [==] and no [!=]
- Can be optimized with [-unsafe] with same behaviour (all underlying string operations are based on [s.[i] <- '\x42'] and all tests succeed with [-unsafe] compilation)

*)

(** {3 Quick Access Module} *)

(** A module that may be useful for [open BitBuffer.Op in] *)
module Op: sig

    val ioc : char -> int
    (** int_of_char replication *)

    val coi : int -> char
    (** char_of_int replication *)

    val iob : bool -> int
    (** int of bool (0 of false, <> 0 of true, usually 1) *)
    
    val boi : int -> bool
    (** bool of int (C-like convention) *)

end


(** {3 The basic types and exceptions } *)


type buffer
(** The binary buffer *)

type error = [
    | `bit_index_out_of_bounds
    | `invalid_int_size
    | `invalid_buffer_size
]
(** The reasons of errors *)

val string_of_error : error -> string
(** String representing the [error] *)

exception Error of error
(** The exception raised when, for example, accessing the buffer out of its bounds *)

(** {3 Basic Functions} *)

val create: ?with_val:bool -> int -> buffer
(** Create a new buffer of a given length (in bits) filled with [with_val] (default: 0/false) *)

val copy: buffer -> buffer
(** Creates a fresh copy of a buffer *)

val length : buffer -> int
(** The number of bits in the buffer *)

val unsafe_get_string : buffer -> string
(** Get the underlying string (maybe not maintained in the future, if
    implementation changes) *)

val of_string : string -> buffer
(** Make a buffer from a string (it's length will be [8 * (String.length s)]) *)

val is_byte_array : buffer -> bool
(** Returns true if the buffer is a string (i.e. [length mod 8 = 0]) *)

val to_string : ?complete_with:bool -> buffer -> string
(** Convert the buffer to a string, if the buffer length is not a
multiple of 8 the string will be completed with [complete_with] (default: 0)
*)

val get_bit : buffer -> int -> bool
(** Get the i-th bit of the buffer *)

val set_bit : buffer -> int -> bool -> unit
(** Set the i-th bit of the buffer *)

val write_byte : buffer -> int -> char -> unit
(** Write a byte (8 bits) starting from a given position *)

val write_int : buffer -> index:int -> size:int -> int -> unit
(** Write the [size] lowest bits of the int, starting from [index] 
(example: in
[let b = of_string "\x00\x00" in write_int b ~index:3 ~size:7 0b101_1011],
[b] will become: [0b00010110_11000000])
*)

val write_int64 : buffer -> index:int -> size:int -> int64 -> unit
(** Same as [write_int] but with an [int64] *)

val read_byte : buffer -> int -> char
(** Read a byte (8 bits) from given index *)

val read_int : buffer -> index:int -> size:int -> int
(** Read an integer of given [size], starting from [index] *)

val read_int64 : buffer -> index:int -> size:int -> int64
(** Same as [read_int] but with an [int64] *)

val write_buffer :
  dst:buffer -> dst_index:int -> src:buffer -> src_index:int -> size:int -> unit
(** [write_buffer ~dst ~dst_index ~src ~src_index ~size] writes [size] bits
from [src] starting at [src_index] in buffer [dst] starting at [dst_index] *)

val concat : ?size:int -> buffer list -> buffer
(** Create a buffer from a buffer list,
if [size] is given then
[concat] creates a buffer of that size and write as much as possible (ending with zeros),
if not [concat] uses [List.fold_left] to compute the exact size  *)

val append : buffer -> buffer -> unit
(** [append b1 b2] appends b2 at the end of b1 *)

val sub : buffer -> index:int -> size:int -> buffer
(** Creates a new buffer from a part of its operand (like [String.sub]) *)
