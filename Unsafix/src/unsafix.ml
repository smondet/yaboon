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
Access to unsafe and non-portable features of Unix  *)


(** {3 Sockets} *)

(**
The socket creation (no verification: can return an invalid file descriptor)
*)
external socket:
    domain:int -> sock_type:int -> protocol:int -> Unix.file_descr
    = "unsafix_socket"


(** Same as [socket] but returns [None] if call failed *)
let socket_opt ~domain ~sock_type ~protocol = 
    let s = socket ~domain ~sock_type ~protocol in
    if (Obj.magic s:int) = -1 then None else Some s


(** Set a socket option of type [int] *)
external setsockopt_int:
    Unix.file_descr -> level:int -> optname:int -> optval:int -> int
    = "unsafix_setsockopt_int"

(** Get a socket option of type [int] *)
external getsockopt_int:
    Unix.file_descr -> level:int -> optname:int -> int
    = "unsafix_getsockopt_int"


(** The return type of [getsockopt_str] *)
type string_sockopt = [
    | `ok of string
    | `error of int
    | `optlength_overflow
]


(** Retrieve a socket option of variable length as a [string] *)
external getsockopt_str:
    Unix.file_descr -> level:int -> optname:int -> optlength:int -> string_sockopt
    = "unsafix_getsockopt_str"

(** {3 Nanosleep} *)

(** Return type of [nanosleep] and [nanosleep_float] *)
type nanosleep_status = [
    | `ok
    | `error_EFAULT
    | `error_EINVAL
    | `interrupted of int * int
]

(** High resolution [sleep]. Call [nanosleep seconds nanoseconds], if the call
is interrupted (e.g. by a signal), it will return [`interrupted (secs,nanos)]
with the remaining time. *)
external nanosleep:
    int -> int -> nanosleep_status
    = "unsafix_nanosleep"

(** Same as [nanosleep] but with a [float] argument *)
let nanosleep_float f = (
    let s, ns = int_of_float f, int_of_float ((f -. (floor f)) *. 10e8) in
    (nanosleep s ns)
)

(** {3 Errno} *)

(** Get the current value of the unix global variable [errno] *)
external current_errno: unit -> Unix.error = "unsafix_current_errno"


