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
TODO list:
    x remove_if -> removes once
    x filter -> removes all non-matching
    x remove_while -> the (not) of filter
    - try to factorize some code (remove/filter/...)
    - fold_left/right
    x find
    x find_all
    Exception versions:
        x set_nth
        x get_nth
        x take_head
        x see_last
        x see_head
    Submodules:
        x LIFO: create, push, pop, peek, is_empty (just change the names of functions)
        x FIFO: create, push, pop, peek, is_empty (just change the names of functions)
        - Unsafe: with Obj.magic to decrease complexity
            x convert_to_list

 *)

(* For debug: *)
let p = Printf.printf

type 'a node = {
    mutable n_val: 'a;
    mutable n_nxt: 'a node;
}

type 'a iList = {
    mutable i_fst: 'a node option;
    mutable i_lst: 'a node option;
    mutable i_lth: int;
}

exception Error of [ `empty | `index_out_of_bounds ]

type ('a, 'b) return = [ `value of 'a | `other of 'b ]

let get_return_value = function
    | `value v -> v
    | _ -> raise (Failure "not a `value")



let empty () = { i_fst = None; i_lst = None ; i_lth = 0}
    
let is_empty l = l.i_fst = None
    
let length l = l.i_lth

let to_empty l = (
    l.i_fst <- None;
    l.i_lst <- None;
    l.i_lth <- 0;
)

module Internal = struct

    exception ExitLoop

    (** Get option (Should only be used when <> None *)
    let get_option_exn = function
        | None -> failwith "It's a bug in iList implementation"
        | Some v -> v

    let get_option_or_empty =
        function None -> `other `empty | Some v -> `value v

    let get_option_val_or_empty =
        function None -> `other `empty | Some v -> `value v.n_val

    (** Makes a one-node list from empty list l *)
    let make_one l e = (
        let rec node = { n_val = e ; n_nxt = node; } in
        l.i_fst <- Some node;
        l.i_lst <- Some node;
        l.i_lth <- 1;
    )

    let incr_length l = l.i_lth <- l.i_lth + 1
    let decr_length l = l.i_lth <- l.i_lth - 1

    let copy_node n = {
        n_val = n.n_val;
        n_nxt = n.n_nxt;
    }

    let get_nth_node l index = (
        if not (0 <= index && index < l.i_lth) then (
            `other `index_out_of_bounds
        ) else (
            let node = ref (get_option_exn l.i_fst) in
            (* here the list can't be empty *)
            for i = 0 to index - 1 do
                node := (!node).n_nxt;
            done;
            `value !node
        )
    )
    let get_nth_node_exn l index = (
        if not (0 <= index && index < l.i_lth) then (
            raise (Failure "Bug in get_nth_node_exn usage");
        );
        let node = ref (get_option_exn l.i_fst) in
        (* here the list can't be empty *)
        for i = 0 to index - 1 do
            node := (!node).n_nxt;
        done;
        !node
    )

    let iter_while_2_exn l1 l2 f = (
        if l1.i_lth <> l2.i_lth then (
            raise (Failure "Bug: called iter_2_exn with different sizes");
        );
        if not (is_empty l2) then (
            let node1 = ref (get_option_exn l1.i_fst) in
            let node2 = ref (get_option_exn l2.i_fst) in
            let continue = ref true in
            let count = ref 0 in
            while !continue do
                (* for i = 0 to l1.i_lth - 1 do *)
                continue :=
                    (f (!node1).n_val (!node2).n_val)
                    && (!count <> (l1.i_lth - 1));
                incr count;
                node1 := (!node1).n_nxt;
                node2 := (!node2).n_nxt;
            done;
        );
    )


end


let add_head l e = (
    if is_empty l then (
        Internal.make_one l e;
    ) else (
        let node =
            { n_val = e ; n_nxt = Internal.get_option_exn l.i_fst; } in
        l.i_fst <- Some node;
        (Internal.get_option_exn l.i_lst).n_nxt <- node;
        Internal.incr_length l;
    );
)

let add_last l e = (
    if is_empty l then (
        Internal.make_one l e;
    ) else (
        let node =
            { n_val = e ; n_nxt = Internal.get_option_exn l.i_fst; } in
        (Internal.get_option_exn l.i_lst).n_nxt <- node;
        l.i_lst <- Some node;
        Internal.incr_length l;
    );
)

let see_head l = (Internal.get_option_val_or_empty l.i_fst)
let see_last l = (Internal.get_option_val_or_empty l.i_lst)
let see_head_exn l = (match l.i_fst with Some v -> v.n_val | None -> raise (Error `empty))
let see_last_exn l = (match l.i_lst with Some v -> v.n_val | None -> raise (Error `empty))

let take_head l = (
    match Internal.get_option_or_empty l.i_fst with
    | `value first_node ->
            if l.i_lth = 1 then (
                l.i_fst  <- None;
                l.i_lst   <- None ;
                l.i_lth <- 0;
            ) else (
                l.i_fst <- Some first_node.n_nxt;
                (Internal.get_option_exn l.i_lst).n_nxt <- first_node.n_nxt;
                Internal.decr_length l;
            );
            `value first_node.n_val
    | `other `empty as other -> other
)
let take_head_exn l = (
    match l.i_fst with
    | Some first_node ->
            if l.i_lth = 1 then (
                l.i_fst  <- None;
                l.i_lst   <- None ;
                l.i_lth <- 0;
            ) else (
                l.i_fst <- Some first_node.n_nxt;
                (Internal.get_option_exn l.i_lst).n_nxt <- first_node.n_nxt;
                Internal.decr_length l;
            );
            first_node.n_val
    | None -> raise (Error `empty)
)


(* Some other implementation tests are in test_list.ml
 * this one seems the best as it is efficient and safe
 * (no '==' or '!=')
 *)
let iter l ~f = ( (* Does not rely on == or != *)
    if not (is_empty l) then (
        let node = ref (Internal.get_option_exn l.i_fst) in
        for i = 0 to l.i_lth - 1 do
            let () = f (!node).n_val in
            node := (!node).n_nxt;
        done;
    );
)

let transform l ~f = (
    if not (is_empty l) then (
        let node = ref (Internal.get_option_exn l.i_fst) in
        for i = 0 to l.i_lth - 1 do
            (!node).n_val <- f (!node).n_val;
            node := (!node).n_nxt;
        done;
    );
)

let get_nth l i = (
    match Internal.get_nth_node l i with
    | `value node -> `value node.n_val
    | `other other -> `other other
)

let get_nth_exn l i = (
    match Internal.get_nth_node l i with
    | `value node -> node.n_val
    | `other other -> raise (Error other)
)

let set_nth l i v = (
    match Internal.get_nth_node l i with
    | `value node -> node.n_val <- v; `value ()
    | `other other -> `other other
)

let set_nth_exn l i v = (
    match Internal.get_nth_node l i with
    | `value node ->
            node.n_val <- v; 
    | `other `index_out_of_bounds ->
            raise (Error `index_out_of_bounds)
)

let map l ~f = (
    let new_list = empty () in
    iter l (
        fun elt ->
            add_last new_list (f elt);
    );
    new_list
)

let copy l = map l (fun e -> e)

let equals ?(cmp=(=)) l1 l2 = (
    if (length l1) <> (length l2) then (
        false
    ) else (
        let ret = ref true in
        Internal.iter_while_2_exn l1 l2 (fun a b ->
            if not (cmp a b ) then (
                ret := false;
            );
            !ret
        );
        !ret
    )
)

let map_of_list l ~f = (
    let ll = empty () in
    List.iter (fun e -> add_last ll (f e)) l;
    ll
)
let map_to_list_rev l ~f = (
    let res = ref [] in
    iter l ~f:(fun e -> res := (f e) :: !res);
    !res
)
let map_to_list l ~f = List.rev (map_to_list_rev l ~f)


let copy_rev l = (
    let res = empty () in
    iter l (fun e ->
        add_head res e;
    );
    res
)

let reverse l = (
    let cp = copy_rev l in
    l.i_fst <- cp.i_fst;
    l.i_lst <- cp.i_lst;
    l.i_lth <- cp.i_lth;
)

let of_list l = map_of_list l (fun e -> e)
let to_list l = map_to_list l (fun e -> e)

let append l1 l2 =  (
    match is_empty l1, is_empty l2 with
    | true, false ->
            l1.i_fst <- l2.i_fst;
            l1.i_lst <- l2.i_lst;
            l1.i_lth <- l2.i_lth;
            to_empty l2;
    | _, true ->
            ()
    | _, _ ->
            let l1_fst_node = Internal.get_option_exn l1.i_fst in
            let l1_lst_node = Internal.get_option_exn l1.i_lst in
            let l2_fst_node = Internal.get_option_exn l2.i_fst in
            let l2_lst_node =  Internal.get_option_exn l2.i_lst in
            l1_lst_node.n_nxt <- l2_fst_node;
            l2_lst_node.n_nxt <- l1_fst_node;
            l1.i_lth <- l1.i_lth + l2.i_lth;
            l1.i_lst <- Some l2_lst_node;
            to_empty l2;
)


let remove_nth l i = (
    if not (0 <= i && i < l.i_lth) then (
        `other `index_out_of_bounds
    ) else (
        match i, l.i_lth with
        | 0, 1 ->
                let only_node = Internal.get_option_exn l.i_fst in
                to_empty l;
                `value only_node.n_val
        | 0, _ ->
                let fst_node = Internal.get_option_exn l.i_fst in
                let lst_node = Internal.get_option_exn l.i_lst in
                l.i_fst <- Some fst_node.n_nxt;
                lst_node.n_nxt <- fst_node.n_nxt;
                Internal.decr_length l;
                `value fst_node.n_val
        (* | n, lg when n < lg - 1 -> *)
                (* let just_before_node = Internal.get_nth_node l (n - 1) in *)
                (* let to_remove = just_before_node.n_nxt in *)
                (* just_before_node.n_nxt <- to_remove.n_nxt; *)
                (* `value to_remove.n_val *)
        | n, lg  ->
                let just_before_node = Internal.get_nth_node_exn l (n - 1) in
                let to_remove = just_before_node.n_nxt in
                if n = l.i_lth - 1 then (
                    l.i_lst <- Some just_before_node;
                );
                just_before_node.n_nxt <- to_remove.n_nxt;
                Internal.decr_length l;
                `value to_remove.n_val

    )
)


let remove_if l ~f = (
    match l.i_lth with
    | 0 -> ()
    | 1 -> 
            let only_node = Internal.get_option_exn l.i_fst in
            if f only_node.n_val then (
                to_empty l;
            );
    | lgth ->
            let fst_node = Internal.get_option_exn l.i_fst in
            let lst_node = Internal.get_option_exn l.i_lst in
            if f fst_node.n_val then (
                l.i_fst <- Some fst_node.n_nxt;
                lst_node.n_nxt <- fst_node.n_nxt;
                Internal.decr_length l;
            ) else (
                let node = ref fst_node in
                let nth = ref 0 in
                while (not (f !node.n_nxt.n_val)) && (!nth < lgth - 1) do
                    node := (!node).n_nxt;
                    incr nth;
                done;
                if (!nth < lgth - 1) then (
                    let to_remove = (!node).n_nxt in
                    if !nth = l.i_lth - 2 then (
                        l.i_lst <- Some !node;
                    );
                    (!node).n_nxt <- to_remove.n_nxt;
                    Internal.decr_length l;
                );
            );
)

let generic_remove_while l ~f ~yes_or_no = (

    let one_before_the_last = ref None in
    if l.i_lth > 1 then (
        (* Head Elements (when we remove the head we have a new head...) *)
        let fst_node = Internal.get_option_exn l.i_fst in
        let lst_node = Internal.get_option_exn l.i_lst in
        let node = ref fst_node in
        while (yes_or_no (f !node.n_val)) && l.i_lth > 1 do
            Internal.decr_length l;
            node := !node.n_nxt;
        done;
        l.i_fst <- Some !node;
        lst_node.n_nxt <- !node;

        (* Non-head and Non-end elements: *)
        let index = ref 0 in
        let lgth = l.i_lth in
        while !index < lgth - 2 do
            if yes_or_no (f !node.n_nxt.n_val) then (
                (* remove the following one *)
                let to_remove = (!node).n_nxt in
                (!node).n_nxt <- to_remove.n_nxt;
                Internal.decr_length l;
            );
            node := (!node).n_nxt;
            incr index;
        done;
        one_before_the_last := Some !node;
        (* now !index is lgth - 2
           => there's still at least one element in the list *)
    );

    (* Only eventual last element to check/remove
       3 cases: length = 0, 1 or more
    *)
    begin match l.i_lth with
    | 0 -> ()
    | 1 ->
            let only_node = Internal.get_option_exn l.i_fst in
            if yes_or_no (f only_node.n_val) then (
                to_empty l;
            );
    | _ ->
            let last_node = Internal.get_option_exn l.i_lst in
            if yes_or_no (f last_node.n_val) then (
                let fst_node = Internal.get_option_exn l.i_fst in
                let before = Internal.get_option_exn !one_before_the_last in
                before.n_nxt <- fst_node;
                l.i_lst <- Some before;
                Internal.decr_length l;
            );
    end;
)

let filter l ~f = generic_remove_while l ~f ~yes_or_no:(not)

let remove_while l ~f = generic_remove_while l ~f ~yes_or_no:(fun (x:bool) -> x)

let find l ~f = (
    let res = ref None in
    begin try
        iter l ~f:(fun a -> 
            if f a then (
                res := Some a;
                raise Internal.ExitLoop;
            );
        );
    with Internal.ExitLoop -> ()
    end;
    !res
)

let find_all l ~f = (
    let res = empty () in
    iter l ~f:(fun a ->
        if f a then (
            add_last res a;
        );
    );
    res
)

module Unsafe = struct

    let convert_to_list (l:'a iList) = (
        if is_empty l then (
            []
        ) else (
            let the_list = (Obj.magic (Internal.get_option_exn l.i_fst): 'a list) in
            (Internal.get_option_exn l.i_lst).n_nxt <- Obj.magic [];
            (* As the previous write invalidates our iList, we must empty it 
               insure some coherency...
            *)
            to_empty l;
            the_list

        )
    )

end

module LIFO = struct
    let create = empty
    let push = add_head
    let pop = take_head_exn
    let peek = see_head_exn
    let is_empty = is_empty
end

module FIFO = struct
    let create = empty
    let push = add_last
    let pop = take_head_exn
    let peek = see_head_exn
    let is_empty = is_empty
end

