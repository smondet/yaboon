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

This syntax extension provides a (very) simple unit testing framework.
(see test_TestParser.ml to learn usage)


ocamlc -I +camlp4 camlp4lib.cma -pp camlp4oof -c SimpleTestParser.ml

*)
open Camlp4.PreCast
open Syntax

EXTEND Gram
    expr: LEVEL "top"
    [ [
        "TEST" ; "ASSERT" ; the_test = expr  ->
        (* <:expr< $uid:m$.iter (fun $lid:v$ -> $seq$) $e$ >> *)
        let (file, line, c, d, e, f, g, h) = Loc.to_tuple _loc in
        (* Printf.eprintf "(* some information *)\n"; *)
        <:expr<
            let () =
                let res = $the_test$ in 
                let pr =  Printf.printf in
                if not res then (
                    pr "[%s:%d] Assertion test failed\n" $`str:file$ $`int:line$ ;
                );
            in ()
        >>
        | "TEST"; "NAME" ; name = expr ; "ASSERT"; the_test = expr  ->
        (* <:expr< $uid:m$.iter (fun $lid:v$ -> $seq$) $e$ >> *)
        let (file, line, c, d, e, f, g, h) = Loc.to_tuple _loc in
        <:expr<
            let () =
                let res = $the_test$ in 
                let pr =  Printf.printf in
                if not res then (
                    pr "[%s:%d] Assertion test \"%s\" failed\n" $`str:file$ $`int:line$ $name$;
                );
            in ()
        >>
        | "TEST"; "NAME" ; name = expr ; "EXCEPTION" ; the_exn = expr ;
             "IN"; the_test = expr  ->
        (* <:expr< $uid:m$.iter (fun $lid:v$ -> $seq$) $e$ >> *)
        let (file, line, c, d, e, f, g, h) = Loc.to_tuple _loc in
        <:expr<
            let () =
                (* let res = $the_test$ in  *)
                let f () = $the_test$ in
                let success = ref false in
                begin try
                    ignore (f ());
                with
                | exn when exn = $the_exn$ ->
                        success := true;
                end;
                let pr =  Printf.printf in
                if not !success then (
                    pr "[%s:%d] Exception test \"%s\" failed\n" $`str:file$ $`int:line$ $name$;
                );
            in ()
        >>
        | "TEST";  "EXCEPTION" ; the_exn = expr ;
             "IN"; the_test = expr  ->
        (* <:expr< $uid:m$.iter (fun $lid:v$ -> $seq$) $e$ >> *)
        let (file, line, c, d, e, f, g, h) = Loc.to_tuple _loc in
        <:expr<
            let () =
                (* let res = $the_test$ in  *)
                let f () = $the_test$ in
                let success = ref false in
                begin try
                    ignore (f ());
                with
                | exn when exn = $the_exn$ ->
                        success := true;
                end;
                let pr =  Printf.printf in
                if not !success then (
                    pr "[%s:%d] Exception test failed\n" $`str:file$ $`int:line$;
                );
            in ()
        >>
    ] ]
    ;
END

