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
ocamlc -pp "camlp4 -parser OCaml SimpleTestParser.cmo -printer Auto" \
test_TestParser.ml -o e_test

*)
let pr = Printf.printf

let () = (
    pr "let's test the tester\n";
    TEST ASSERT (
        pr "This is a succesful test\n";
        true
    );
    TEST ASSERT (
        pr "This is a failful test\n";
        false
    );
    TEST NAME "Named test !!" ASSERT (
        pr "This is another failful test\n";
        false
    );
    TEST NAME "Exception raised (Test OK)" 
    EXCEPTION (Invalid_argument "index out of bounds")
    IN (let b = [|4; 5|] in b.(2));

    TEST NAME "Exception not raised (Test KO)" 
    EXCEPTION (Invalid_argument "index out of bounds")
    IN (let b = [|4; 5|] in b.(1));

    TEST EXCEPTION (Invalid_argument "index out of bounds")
    IN (let b = [|4; 5|] in b.(2));

    TEST EXCEPTION (Invalid_argument "index out of bounds")
    IN (let b = [|4; 5|] in b.(1));

)
