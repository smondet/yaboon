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
(*

Here are some performance measurements

cp bitBuffer.ml bitBufferOpen.ml
ocamlopt   bitBufferOpen.ml write_contest.ml
time ./a.out


*)

open BitBufferOpen
open BitBufferOpen.Op


let write_byte buf index v = (
    Internal.check_bounds_exn buf index;
    Internal.check_bounds_exn buf (index + charsize - 1);
    let ofset_in_byte = index mod charsize in
    if ofset_in_byte = 0 then (
        buf.b_string.[index / charsize] <- v;
    ) else (
        (* setting the first semi-byte *)
        Internal.set_low_bits_of_char
            buf.b_string (index / charsize) ofset_in_byte v;
        (* setting the second part *)
        Internal.set_high_bits_of_char
            buf.b_string ((index / charsize) + 1) ofset_in_byte v;
    );
)
let write_byte_greedy buf index v = (
    Internal.check_bounds_exn buf index;
    Internal.check_bounds_exn buf (index + charsize - 1);
    let vi = ioc v in
    for i = 0 to 7 do
        let mask = 0b1000_0000 lsl i in
        set_bit buf (index + i) (mask land vi = mask);
    done;
)




let () =
    let b = of_string "AAAAA" in
    for i = 0 to 100000000 do
        (* write_byte b 2 '\x42'; *)
        write_byte_greedy b 2 '\x42';
    done;
    

(*
RESULTS:

    optimized normal write_byte:
real	0m3.153s
user	0m3.156s
sys	0m0.000s

    write_byte_greedy:
real	0m19.411s
user	0m19.349s
sys	0m0.056s



 *)



