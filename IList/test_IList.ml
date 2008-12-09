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

To compile the test:

You need the SimpleTestParser camlp4 macro:

TEST_PARSER=../SimpleTestParser/SimpleTestParser.cmo

ocamlc -pp "camlp4o $TEST_PARSER" iList.mli iList.ml test_IList.ml

 
 *)

let p = Printf.printf
let spr = Printf.sprintf


let pli l = (
    p "[";
    IList.iter l ~f:(p "%d; ");
    p "]\n";
)

open IList

let () = (

    p "Starting tests\n";
    let l = empty () in
    TEST ASSERT ((is_empty l) && ((length l) = 0));
    TEST ASSERT (see_head l = `other `empty);
    TEST ASSERT (see_last l = `other `empty);
    add_head l "head 1";
    TEST ASSERT (not (is_empty l) && ((length l) = 1));
    TEST ASSERT (see_head l = `value "head 1");
    TEST ASSERT (see_last l = `value "head 1");
    add_last l "last 1";
    TEST ASSERT (not (is_empty l) && ((length l) = 2));
    TEST ASSERT (see_head l = `value "head 1");
    TEST ASSERT (see_last l = `value "last 1");
    add_last l "last 2";
    TEST ASSERT (not (is_empty l) && ((length l) = 3));
    TEST ASSERT (see_head l = `value "head 1");
    TEST ASSERT (see_last l = `value "last 2");
    let head1 = take_head l in
    TEST ASSERT (head1 = `value "head 1");
    TEST ASSERT (not (is_empty l) && ((length l) = 2));
    TEST ASSERT (see_head l = `value "last 1");
    TEST ASSERT (see_last l = `value "last 2");
    let head2 = take_head l in
    TEST ASSERT (head2 = `value "last 1");
    TEST ASSERT (not (is_empty l) && ((length l) = 1));
    TEST ASSERT (see_head l = `value "last 2");
    TEST ASSERT (see_last l = `value "last 2");
    let head3 = take_head l in
    TEST ASSERT (head3 = `value "last 2");
    TEST ASSERT ((is_empty l) && ((length l) = 0));
    TEST ASSERT (see_head l = `other `empty);
    TEST ASSERT (see_last l = `other `empty);
    (* l is now empty *)
    let a_four = [| "oan"; "to"; "thri" ;"fore" |] in
    Array.iter (fun s -> add_last l s) a_four;
    TEST ASSERT (not (is_empty l) && ((length l) = 4));
    TEST ASSERT (see_head l = `value "oan");
    TEST ASSERT (see_last l = `value "fore");
    let i = ref 0 in
    iter l ~f:(fun s ->
        TEST NAME (spr "iter:%d" !i) ASSERT (s = a_four.(!i));
        incr i;
    );
    transform l ~f:String.uppercase;
    let a_FOUR = Array.map String.uppercase a_four in
    let i = ref 0 in
    iter l (fun s ->
        TEST NAME (spr "transform:%d" !i) ASSERT (s = a_FOUR.(!i));
        incr i;
    );
    let () =
        let ll = empty () in
        let succes = ref true in 
        iter ll ~f:(fun e -> succes := false);
        TEST ASSERT (!succes);
        succes := true;
        transform ll ~f:(fun e -> succes := false; e - 1);
        TEST ASSERT (!succes);
    in

    (* get_return_value *)
    let () =
        let ll = empty () in
        let success = ref false in
        begin try
            let s = 
                get_return_value (take_head ll);
            in
            p "Should not be there !! (%d)\n" s;
        with
        | Failure "not a `value" ->
                success := true;
        end;
        TEST ASSERT (!success);
        add_last ll 42;
        TEST ASSERT (42 = get_return_value (see_head ll));
    in

    (* the *_nth* values *)
    TEST ASSERT (get_nth l 1 = `value a_FOUR.(1));
    TEST ASSERT (get_nth l 4 = `other `index_out_of_bounds);
    TEST ASSERT (set_nth l 2 "three" = `value ());
    TEST ASSERT (get_nth l 2 = `value "three");
    TEST ASSERT (set_nth l 4 "faive" = `other `index_out_of_bounds);
    set_nth_exn l 2 "THREE";
    TEST ASSERT (get_nth_exn l 2 = "THREE");

    TEST EXCEPTION (Error `index_out_of_bounds) IN (set_nth_exn l 4 "FIVE");
    TEST EXCEPTION (Error `index_out_of_bounds) IN (get_nth_exn l 4);


    (* the map and copy functions: *)
    let () = 
        let ll = empty () in
        TEST ASSERT (is_empty (map ll ~f:(fun a -> 0)));
        TEST ASSERT (is_empty (copy ll));
        let etalon =  [0; 1; 2; 3; 4; 5; 6] in
        List.iter (fun i -> add_last ll i) etalon;
        let lm = map ll (fun e -> e + 10) in
        let lc = copy ll in
        List.iter (fun i ->
            TEST NAME (spr "iter:%d" i) ASSERT (`value (i + 10) = get_nth lm i);
            TEST NAME (spr "iter:%d" i) ASSERT (`value i = get_nth lc i);
        ) etalon;
    in

    (* The to_empty function *)
    TEST ASSERT (not (is_empty l));
    to_empty l;
    TEST ASSERT (is_empty l);
    add_head l "head 1";
    TEST ASSERT (not (is_empty l) && ((length l) = 1));
    TEST ASSERT (see_head l = `value "head 1");
    TEST ASSERT (see_last l = `value "head 1");
    add_last l "last 1";
    TEST ASSERT (not (is_empty l) && ((length l) = 2));
    TEST ASSERT (see_head l = `value "head 1");
    TEST ASSERT (see_last l = `value "last 1");

    (* function equals *)
    let test_equals cmp = 
        let l1 = empty () in
        let l2 = empty () in
        TEST ASSERT (equals ?cmp l1 l2);

        let etalon1 = [0; 1; 2; 3] in
        List.iter (add_last l1) etalon1;
        TEST ASSERT (not (equals ?cmp l1 l2));
        List.iter (add_last l2) etalon1;
        TEST ASSERT (equals ?cmp l1 l2);

        to_empty l2;
        let etalon2 = [0; 1; 2; 3; 4] in
        List.iter (add_last l2) etalon2;
        TEST ASSERT (not (equals ?cmp l1 l2));

        to_empty l2;
        let etalon3 = [0; 1; 3; 3] in
        List.iter (add_last l2) etalon3;
        TEST ASSERT (not (equals ?cmp l1 l2));
        
    in
    test_equals None;
    test_equals (Some (fun a b ->
        (* p "%d %d\n" a b; *)
        a + 100 = b + 100
    ));

    (* The map_* functions *)
    let () =
        let id = fun e -> e in
        TEST ASSERT (is_empty (map_of_list [] id));
        TEST ASSERT ((map_to_list (empty ()) ~f:id) = []);
        TEST ASSERT ((map_to_list_rev (empty ()) id) = []);
        let etalon = [1; 2; 3; 4; 5;] in
        let ll = map_of_list etalon (fun a -> a + 2) in
        TEST ASSERT ((map_to_list ll (fun a -> a - 2)) = etalon);
        TEST ASSERT ((map_to_list_rev ll ~f:(fun a -> a - 2)) = List.rev etalon);
    in

    (* The copy_rev and rev functions *)
    let () =
        TEST ASSERT (let l = empty () in equals (copy_rev l) l); 
        TEST ASSERT (let l = empty () in reverse l; is_empty l); 
        let etalon = [1; 2; 3; 4; 5;] in
        let l = of_list etalon in
        let ll = copy_rev l in
        let lll =  of_list (List.rev etalon) in
        TEST ASSERT (equals ll lll);
        reverse l;
        TEST ASSERT ((to_list l) = (List.rev etalon));
        reverse ll;
        reverse lll;
        TEST ASSERT (equals ll lll);
    in

    (* Test the append function *)
    let () =
        let le = empty () in
        append le (empty ());
        TEST ASSERT (is_empty le);
        let l1 = of_list [1; 2; 3; ] in
        let l2 = of_list [5; 6; 7; ] in
        append l1 l2;
        TEST ASSERT (is_empty l2);
        TEST ASSERT ((to_list l1) = [1; 2; 3; 5; 6; 7; ]);
        append l1 l2;
        TEST ASSERT (is_empty l2);
        TEST ASSERT ((to_list l1) = [1; 2; 3; 5; 6; 7; ]);
        append l2 l1;
        TEST ASSERT (is_empty l1);
        TEST ASSERT ((to_list l2) = [1; 2; 3; 5; 6; 7; ]);
        (* Some random crash course: *)
        add_head l1 42;
        add_last l1 43;
        TEST ASSERT ((to_list l1) = [42; 43]);
        TEST ASSERT ((take_head l2) = `value 1);
        TEST ASSERT ((take_head l2) = `value 2);
        TEST ASSERT ((take_head l2) = `value 3);
        reverse l2;
        append l2 (copy_rev l1);
        TEST ASSERT ((to_list l2) = [7;6;5;43;42]);
    in

    (* remove_nth *)
    let test_remove_nth l = 
        TEST ASSERT ((remove_nth l 0)  = (`other `index_out_of_bounds));
        TEST ASSERT ((remove_nth l 1)  = (`other `index_out_of_bounds));
        TEST ASSERT ((remove_nth l (-1)) = (`other `index_out_of_bounds));
        append l (of_list [1 ;2 ;3 ;5]);
        TEST ASSERT ((remove_nth l 0)  = `value 1);
        TEST ASSERT (to_list l = [2;3;5]);

        TEST ASSERT ((remove_nth l 2)  = `value 5);
        TEST ASSERT (to_list l = [2;3]);

        TEST ASSERT ((remove_nth l 10)  = (`other `index_out_of_bounds));
        TEST ASSERT ((remove_nth l (-1)) = (`other `index_out_of_bounds));

        TEST ASSERT ((remove_nth l 1)  = `value 3);
        TEST ASSERT (to_list l = [2;]);

        TEST ASSERT ((remove_nth l 0)  = `value 2);
        TEST ASSERT (to_list l = []);
    in
    let l = empty () in test_remove_nth l; test_remove_nth l;

    let () (* some _exn *) =
        let l = of_list [1 ;2 ;3] in
        TEST ASSERT (take_head_exn l = 1);
        TEST EXCEPTION (Error `empty) IN (take_head_exn (empty ()));

        TEST ASSERT (see_head_exn l = 2);
        TEST ASSERT (see_last_exn l = 3);
        TEST EXCEPTION (Error `empty) IN (see_head_exn (empty ()));
        TEST EXCEPTION (Error `empty) IN (see_last_exn (empty ()));
    in

    (* remove_if *)
    let () = (
        let l = empty () in
        remove_if ~f:(fun _ -> true) l;
        TEST ASSERT (is_empty l);
        remove_if ~f:(fun _ -> false) l;
        TEST ASSERT (is_empty l);

        let l = of_list [1] in
        remove_if ~f:(fun _ -> false) l;
        TEST ASSERT (length l = 1 && see_head l = `value 1);
        remove_if ~f:(fun _ -> true) l;
        TEST ASSERT (is_empty l);

        let l = of_list [1; 2; 3; 4; 5; 6; 7; 8;] in
        remove_if ~f:(fun _ -> false) l;
        TEST ASSERT (length l = 8 && see_head l = `value 1);
        remove_if ~f:(fun _ -> true) l;
        TEST ASSERT (length l = 7 && see_head l = `value 2);
        remove_if ~f:(function 4 -> true | _ -> false) l;
        TEST ASSERT (length l = 6 && to_list l = [2; 3; 5; 6; 7; 8;]);
        remove_if ~f:(function 8 -> true | _ -> false) l;
        TEST ASSERT (length l = 5 && to_list l = [2; 3; 5; 6; 7;]);
        remove_if ~f:(function 6 | 7 -> true | _ -> false) l;
        TEST ASSERT (length l = 4 && to_list l = [2; 3; 5; 7;]);

    ) in


    (* filter *)
    let () = (
        let l = empty () in
        filter ~f:(fun _ -> true) l;
        TEST ASSERT (is_empty l);
        filter ~f:(fun _ -> false) l;
        TEST ASSERT (is_empty l);

        let l = of_list [1] in
        filter ~f:(fun _ -> true) l;
        TEST ASSERT (length l = 1 && see_head l = `value 1);
        filter ~f:(fun _ -> false) l;
        TEST ASSERT (is_empty l);

        let l = of_list [1; 2; 3; 4; 5; 6; 7; 8;] in
        filter ~f:(fun _ -> true) l;
        TEST ASSERT (length l = 8 && see_head l = `value 1);
        filter ~f:(fun _ -> false) l;
        TEST ASSERT (is_empty l);

        let l = of_list [1; 2; 3; 4; 5; 6; 7; 8;] in
        filter ~f:(function 4 -> false | _ -> true) l;
        TEST ASSERT (to_list l = [1; 2; 3; 5; 6; 7; 8;]);
        filter ~f:(function 8 -> false | _ -> true) l;
        TEST ASSERT (length l = 6 && to_list l = [1; 2; 3; 5; 6; 7;]);
        filter ~f:(function 6 | 7 -> false | _ -> true) l;
        TEST ASSERT (length l = 4 && to_list l = [1; 2; 3; 5;]);
        filter ~f:(function 1 | 2 -> false | _ -> true) l;
        TEST ASSERT (length l = 2 && to_list l = [3; 5;]);

        let l = of_list [12;1;4555;2;3;48;4;5;6;78;7;456;8;459;9] in
        filter l ~f:((>) 10);
        TEST ASSERT (length l = 9 && to_list l = [1;2;3;4;5;6;7;8;9]);

    ) in

    (* remove_while *)
    let () = (
        let l = empty () in
        remove_while ~f:(fun _ -> false) l;
        TEST ASSERT (is_empty l);
        remove_while ~f:(fun _ -> true) l;
        TEST ASSERT (is_empty l);

        let l = of_list [1] in
        remove_while ~f:(fun _ -> false) l;
        TEST ASSERT (length l = 1 && see_head l = `value 1);
        remove_while ~f:(fun _ -> true) l;
        TEST ASSERT (is_empty l);

        let l = of_list [1; 2; 3; 4; 5; 6; 7; 8;] in
        remove_while ~f:(fun _ -> false) l;
        TEST ASSERT (length l = 8 && see_head l = `value 1);
        remove_while ~f:(fun _ -> true) l;
        TEST ASSERT (is_empty l);

        let l = of_list [1; 2; 3; 4; 5; 6; 7; 8;] in
        remove_while ~f:(function 4 -> true | _ -> false) l;
        TEST ASSERT (to_list l = [1; 2; 3; 5; 6; 7; 8;]);
        remove_while ~f:(function 8 -> true | _ -> false) l;
        TEST ASSERT (length l = 6 && to_list l = [1; 2; 3; 5; 6; 7;]);
        remove_while ~f:(function 6 | 7 -> true | _ -> false) l;
        TEST ASSERT (length l = 4 && to_list l = [1; 2; 3; 5;]);
        remove_while ~f:(function 1 | 2 -> true | _ -> false) l;
        TEST ASSERT (length l = 2 && to_list l = [3; 5;]);

        let l = of_list [12;1;4555;2;3;48;4;5;6;78;7;456;8;459;9] in
        remove_while l ~f:((<) 10);
        TEST ASSERT (length l = 9 && to_list l = [1;2;3;4;5;6;7;8;9]);

    ) in

    
    (* Unsafe.to_list *)
    let () = (
        let l = empty () in
        TEST ASSERT (Unsafe.convert_to_list l = []);
        TEST ASSERT (is_empty l);
        let l = of_list [1] in
        TEST ASSERT (Unsafe.convert_to_list l = [1]);
        TEST ASSERT (is_empty l);
        let l = of_list [1;2;3;42;51] in
        TEST ASSERT (Unsafe.convert_to_list l = [1;2;3;42;51]);
        TEST ASSERT (is_empty l);
    ) in

    (* find *)
    let () = (
        let l = empty () in
        TEST ASSERT (find l ~f:(fun _ -> true) = None);
        TEST ASSERT (find l ~f:(fun _ -> false) = None);
        let l = of_list [1;2;3;4] in
        TEST ASSERT (find l ~f:(fun _ -> true) = Some 1);
        TEST ASSERT (find l ~f:(fun _ -> false) = None);

        TEST ASSERT (find l ~f:(function 2 -> true | _ -> false) = Some 2);
        TEST ASSERT (find l ~f:(function 4 -> true | _ -> false) = Some 4);

        TEST ASSERT (Unsafe.convert_to_list l = [1;2;3;4]);

    ) in

    (* find_all *)
    let () = (
        let l = empty () in
        TEST ASSERT (is_empty (find_all l ~f:(fun _ -> true)));
        TEST ASSERT (is_empty (find_all l ~f:(fun _ -> false)));
        let l = of_list [1;2;3;4] in
        TEST ASSERT (equals (find_all l ~f:(fun _ -> true)) l);
        TEST ASSERT (equals (find_all l ~f:(fun _ -> false)) (empty ()));

        TEST ASSERT (equals (find_all l ~f:(function 2 -> true | _ -> false)) (of_list [2]));
        TEST ASSERT (equals (find_all l ~f:(function 4 -> true | _ -> false)) (of_list [4]));

        TEST ASSERT (equals (find_all l ~f:(function 2 | 3 -> true | _ -> false)) (of_list [2;3]));
        TEST ASSERT (equals (find_all l ~f:(function 1 | 4 -> true | _ -> false)) (of_list [1;4]));

        TEST ASSERT (Unsafe.convert_to_list l = [1;2;3;4]);

    ) in


    p "End of tests\n";

)
