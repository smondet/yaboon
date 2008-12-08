
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
