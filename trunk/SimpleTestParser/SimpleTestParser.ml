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

