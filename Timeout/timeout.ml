module P = Pervasives
module M = Mutex
module U = Unix
module S = Sys

let debug = false
let print_debug = if debug then Printf.printf "[DEBUG_TO_MGR] %s\n" else (fun x -> ())

module Manager = struct
  type t_fd = int
  type t_elt = {
    id: t_fd;
    trigger_date: float;
  }
  type t_val = float
  type t = {
    mutable timeouts:(t_fd * float) list;
    mut:M.t;
    mutable curr_idx:int;
    mutable handler_set:bool;
  }
  let granularity = 0.01 
  exception Too_fast
    
  (** @return a timeout manager *)  
  let create () = 
    { 
      timeouts = [];
      mut = M.create ();
      curr_idx = 0;
      handler_set = false;
    }

  let printed_lock mut s = 
    Printf.printf "[TOLOCK] entering critical section (%s)\n" s;
    flush stdout;
    M.lock mut;
    Printf.printf "[TOLOCK] in critical section (%s)\n" s;
    flush stdout

  let printed_unlock mut s = 
    Printf.printf "[TOLOCK] leaving critical section (%s)\n" s;
    flush stdout;
    M.unlock mut


  let extract_trigger_time x =  let (_,res) = x in res 
  let extract_timeout_id x = let (res,_) = x in res
  let set_real_timer = U.handle_unix_error (U.setitimer U.ITIMER_REAL)
    
  (* non mutexed should be called inside a M.Lock M.unlock *)
  let sort t = 
    ignore(t.timeouts <- List.sort (fun x y -> P.compare (extract_trigger_time x) (extract_trigger_time y))
	     t.timeouts)
      
  (* non mutexed should be called inside a M.Lock M.unlock *)
  let update_timer t = 
    let ps = Printf.sprintf in
    let get_time elt = P.max ((extract_trigger_time elt) -. (U.gettimeofday ())) P.min_float (* in case of hurry *) in
      print_debug "updating timer";
      ignore(match t.timeouts with
	       | [] -> (print_debug "cancelling timer"; set_real_timer {U.it_interval = 0.0; U.it_value = 0.0 })
	       | hd :: tail ->
		   let first_time =  get_time hd in
		     match tail with
		       | [] -> (print_debug "setting real timer with one val"; 
				set_real_timer {U.it_value = first_time ; U.it_interval = 0.0 })
		       | hd2 :: tail -> (
			   let second_time = get_time hd2 in
			     print_debug (ps "setting real time with two val:first:%f second:%f" first_time second_time);
			     set_real_timer {U.it_value = first_time ; U.it_interval = second_time }))
	
  (* non mutexed should be called inside a M.Lock M.unlock *)
  let get_first_second_t_id t = 
    match t.timeouts with
      | [] -> (None,None)
      | hd :: tail ->
	  let first = extract_timeout_id hd in
	    match tail with 
	      | [] -> (Some first,None)
	      | hd2 :: tail2 -> (Some first,Some (extract_timeout_id hd2))

  let cancel_timeout_internal t t_id = 
    let first_second_b = get_first_second_t_id t in
      t.timeouts <- List.filter (fun x -> (extract_timeout_id x) <> t_id) t.timeouts;
      let first_second_a = get_first_second_t_id t in
	if first_second_b <> first_second_a then update_timer t
		  
  (** @param t timeout manager
      @param t_id descriptor of the timeout to cancel *)
  let cancel_timeout t t_id = 
    (*printed_lock t.mut "cancel_timeout";*)
    if not (M.try_lock t.mut) then raise Too_fast;
    cancel_timeout_internal t t_id;
    (*printed_unlock t.mut "cancel_timeout"*)
    M.unlock t.mut
	
  let delete_timeout t t_id = cancel_timeout t t_id
  
  let delete_timeout_internal t t_id = cancel_timeout_internal t t_id
    
  let internal_triggered_fd t = 
    let id = 
      match t.timeouts with
	| [] -> failwith("[ERROR] Asking which timeout has been triggerd when there should not have been anyone")
	| (id,time) :: tail -> 
	    if (time > U.gettimeofday ()) then failwith("[ERROR] timeout triggered earlier than expected");
	    id
    in
      delete_timeout_internal t id;
      id
    
  (** @param t manager.
      @return fd that triggered the signal *)
  let triggered_fd t = 
    (*printed_lock t.mut "triggered_fd";*)
    if not (M.try_lock t.mut) then raise Too_fast;
    try (
      let id = internal_triggered_fd t in
	M.unlock t.mut;
	id
    )
    with Failure s -> (M.unlock t.mut; raise (Failure s))
  
  type t_in_sig_handler = t
  let t_in_sig_equal a b = P.compare a b = 0
    
  let are_the_same t t_sig = 
    P.compare t t_sig = 0
    
  (** set_handler t f, sets f as the handler that will be called by the signal handler.
      @param t timeout manager 
      @param f a function that will be called during the sig handling. *)
  let set_handler t f = 
    let s_hdler alarm = 
      if alarm = S.sigalrm then
	if not (M.try_lock t.mut) then raise Too_fast
	else (
	  let t_id = internal_triggered_fd t in 
	    f t t_id; 
	    M.unlock t.mut)
      else ()
    in
      S.set_signal S.sigalrm (S.Signal_handle s_hdler);
      t.handler_set <- true
	
  let stop_everything (t:t) = ignore(set_real_timer {U.it_interval = 0.0; U.it_value = 0.0 })

  let create_timeout_internal t t_val = 
    assert(t.handler_set);
    let now = U.gettimeofday () in
    let res = t.curr_idx in
      (*let t_val = P.max t_val granularity in*)
    let new_val = (res,now +. t_val) in
      t.curr_idx <- P.succ t.curr_idx;
      let first_second_b = get_first_second_t_id t in
	t.timeouts <- new_val :: t.timeouts;
	sort t;
	let first_second_a = get_first_second_t_id t in
	  if first_second_b <> first_second_a then update_timer t;
	  res
  (** @param t the timeout manager
      @param t_val the value in seconds of when the timeout should expire
      @return  a new timeout descriptor *)
  let create_timeout t t_val = 
    (*printed_lock t.mut "create_timeout";*)
    if not (M.try_lock t.mut) then raise Too_fast;
    let res = create_timeout_internal t t_val in
      (*printed_unlock t.mut "create_timeout";*)
      M.unlock t.mut;
      res
	
      
  (** function to create a timeout in a sig handler 
      @param t_sig the timeout manager in the sig handler (see set_handler)
      @param t_val value of the timeout in seconds
      @return a timeout descriptor
  *)
  let create_timeout_when_handling_to t_sig t_val = create_timeout_internal t_sig t_val
    
  (** as for create timeout but for cancelling *)
  let cancel_timeout_when_handling_to t_sig t_val = cancel_timeout_internal t_sig t_val
    
  
end
module Container = struct
  type t = {
    mutable pendings: Manager.t_fd list;
    mgr: Manager.t;
  }
  
  let create m = { pendings = []; mgr = m; }
  
  let add_timeout t t_fd = t.pendings <-  t_fd :: t.pendings
  let create_timeout t t_val  = 
    let res = Manager.create_timeout t.mgr t_val in
      add_timeout t res;
      res
  
  let create_timeout_when_handling_to t t_sig t_val = 
    assert(Manager.are_the_same t.mgr t_sig);
    let res = Manager.create_timeout_when_handling_to t_sig t_val in
      add_timeout t res;
      res
    
  let get_mgr t = t.mgr 
  
  let is_one_of_my_timeouts t t_id = 
    let (diff_l,same_id) = List.partition (fun x -> x <> t_id) t.pendings in
      t.pendings <- diff_l;
      (List.length same_id = 1) 

  let remove_timeout t t_id = 
    t.pendings <- List.filter (fun x -> x <> t_id) t.pendings
  
  let cancel_timeout t t_id = 
    Manager.cancel_timeout t.mgr t_id;
    remove_timeout t t_id

  let cancel_timeout_when_handling_to t t_sig t_fd = 
    assert(Manager.are_the_same t.mgr t_sig);
    Manager.cancel_timeout_when_handling_to t_sig t_fd;
    remove_timeout t t_fd
  
  let delete t = List.iter (fun x -> cancel_timeout t x) t.pendings
  let delete_in_timeout_handling t t_sig = List.iter (fun x -> cancel_timeout_when_handling_to t t_sig x) t.pendings
end
