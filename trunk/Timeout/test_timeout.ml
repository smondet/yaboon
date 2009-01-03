module S = Sys
module U = Unix
module P = Pervasives 
module M = Mutex
module R = Random
module TM = Timeout.Manager
module TC = Timeout.Container

exception Too_fast
let get_t = U.gettimeofday 
let pp = Printf.printf   

let check_not_interrupted nber_sec_to_wait = 
  let now = get_t () in
    U.sleep nber_sec_to_wait;
    P.abs (nber_sec_to_wait - (int_of_float (P.abs_float ((get_t ()) -. now)) ) ) < 1

let test_to_mgr unit = 
  let ps = Printf.sprintf in
  let (nber_elt_in_list,max_to_val) = 
    if Array.length S.argv != 3 then 
      raise (Failure (ps "Usage:%s <nber_simult_rand_timeouts> <max_to_val>" S.argv.(0)))
    else (int_of_string S.argv.(1),float_of_string S.argv.(2))
  in
  let t_mgr = TM.create () in


  let interrupt_me () = U.sleep P.max_int in
  let dring_prev = ref [] in
  let dring_mut = M.create () in

  let create_to_and_append_to_prev to_val = 
    let now = get_t () in
    let res = TM.create_timeout t_mgr to_val in
      M.lock dring_mut;
      dring_prev:=List.sort (fun x y -> let ((_,x_dt),(_,y_dt)) = (x,y) in P.compare x_dt y_dt) 
	((res,to_val +. now) :: !dring_prev);
      M.unlock dring_mut;
      res
  in
    
  let rec create_random_list res remaining = 
    if remaining = 0 then res
    else create_random_list ((P.max (R.float max_to_val) TM.granularity) :: res) (remaining - 1)
  in
  
  let delete_to_and_remove_from_prev t_id = 
    M.lock dring_mut;
    TM.cancel_timeout t_mgr t_id;
    dring_prev:=List.filter (fun x -> let (x_id,_) = x in x_id <> t_id) !dring_prev;
    M.unlock dring_mut
  in
    
  let f t_in_sig_handler t_id = 
    if not (M.try_lock dring_mut) then raise Too_fast;
    ignore(match !dring_prev with
	     | [] -> raise (Failure "Unexpected timeout");
	     | hd :: tail ->
		 let (exp_t_id,exp_t) = hd in
		 let diff = P.abs_float ((get_t ()) -. exp_t) in
		   if (t_id <> exp_t_id) then (M.unlock dring_mut; failwith("[ERROR] Got non expected t_id"));
		   if (diff > TM.granularity) then 
		     pp "a timeout drang at a diff of %f when wanting %f\n" diff TM.granularity
		       (*else pp "t_id:%d drang at an acceptable time\n" t_id*);
		   dring_prev:=tail);
    M.unlock dring_mut
  in
    TM.set_handler t_mgr f;
    R.self_init ();
    (*let print_dring_prev t_id time =  pp "t_id:%d should dring at %f\n" t_id time in*)
    ignore(create_to_and_append_to_prev 1.0); interrupt_me ();
    (* ok now we check with elements in different order *)
    let test_list =  create_random_list [] nber_elt_in_list in
      List.iter (fun x -> ignore(create_to_and_append_to_prev x)) test_list;
      List.iter (fun x -> interrupt_me ();flush stdout) test_list;
      let new_t_list = create_random_list [] nber_elt_in_list  in
      let t_ids = List.map (fun x -> create_to_and_append_to_prev x) new_t_list in
      let idx = ref 0 in
	List.iter (fun x -> if !idx = 0 then (delete_to_and_remove_from_prev x); idx:=1 - !idx) t_ids;
	idx:=1;
	List.iter (fun x -> if !idx = 0 then (interrupt_me ();flush stdout); idx:=1 - !idx) t_ids;
	let nber_sec_to_wait = P.max 1 ((int_of_float max_to_val) * nber_elt_in_list) in
	  assert(check_not_interrupted nber_sec_to_wait);
	  pp "all the test for manager succeeded\n"
	  
let test_to_container () = 
  let tm = TM.create () in
  let _ = TM.set_handler tm (fun x y -> ()) in
  let my_cont = TC.create tm in
  let tid_1 = TC.create_timeout my_cont 2.0 in
    assert(TC.is_one_of_my_timeouts my_cont tid_1);
    assert(not (TC.is_one_of_my_timeouts my_cont tid_1));
    (* this is because it's not used like that *)
    TM.cancel_timeout tm tid_1;
    assert(check_not_interrupted 2);
    let tid_2 = TC.create_timeout my_cont 2.0 in
      TC.cancel_timeout my_cont tid_2;
      assert(not (TC.is_one_of_my_timeouts my_cont tid_2));
      assert(check_not_interrupted 2);
      pp "all the test for container succeeded\n"
    
let _ = 
  test_to_mgr ();
  test_to_container ()
    
  
      
		  
    
    
