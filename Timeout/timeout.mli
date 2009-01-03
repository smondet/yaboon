(** A module to manage timeouts. Expired timeouts will be handled
    by a handler called by the Sys.sigalrm signal sent
    by the Unix.ITIMER_REAL timer.
    @author: Ion Alberdi
*)

module Manager: sig
  (** timeout descriptors *)
  type t_fd 
  
  (** timeout values in seconds *)
  type t_val = float
      
  (** type of a timeout manager *)
  type t 
    
  (** type of a timeout manager inside the sig handler.
      The data necessary to handle timeouts, i.e their creation, deletion, 
	is mutexed so that if a modification is interrupted by a sig handler, 
	the sig handler could not modify (an exception is raised if so) and make it incoherent. 
	Therefore there are two different views of the managers, 
	one in the normal schedule, the other one during the sig handler, 
	and each one asserts the coherence of the timeouts data.
    *)
  type t_in_sig_handler
    
  (** exception raised if the sig handler is called during a 
      timeout creation or cancellation *)
  exception Too_fast
    

  (** the granularity of the timeouts in seconds *)
  val granularity: float
    
  (** @return a timeout manager *)  
  val create: unit -> t
  
  (** @param t the timeout manager
      @param t_val the value in seconds of when the timeout should expire
      @return  a new timeout descriptor that will expire in t_val seconds *)
  val create_timeout: t -> t_val -> t_fd 
    
  (** cancels the timeout t_fd.
      @param t timeout manager
      @param t_id descriptor of the timeout to cancel *)
  val cancel_timeout: t -> t_fd -> unit
    
  (** creates a timeout in a sig handler.
      @param t_sig the timeout manager in the sig handler (see set_handler)
      @param t_val value of the timeout in seconds
      @return a timeout descriptor *)
  val create_timeout_when_handling_to: t_in_sig_handler -> t_val -> t_fd
  
  (** as for create timeout but for cancelling *)  
  val cancel_timeout_when_handling_to: t_in_sig_handler -> t_fd -> unit
    
  (** @return the boolean the two managers are the same *)
  val t_in_sig_equal: t_in_sig_handler -> t_in_sig_handler -> bool
    
  (** @return the boolean the two views of the timeout manager are the same *)
  val are_the_same: t -> t_in_sig_handler -> bool    
    
  (** set_handler t f, sets f as the handler that will be called by the signal handler.
      the handler has two parameters: 
      - t_in_sig_handler: the sig handler view of the timeout manager that triggered the timeout.
      - t_fd: the timeout descriptor that triggered the timeout.
      @param t timeout manager 
      @param f a function that will be called during the sig handling. *)
  val set_handler: t -> (t_in_sig_handler -> t_fd -> unit) -> unit
    
  (** cancels all the timeouts and stops the Unix.ITIMER_REAL timer *)
  val stop_everything: t -> unit

end

module Container: sig
  (** type of the container *)
  type t
    
  (** @return a new container *)
  val create: Manager.t -> t
    
  (** @return new timeout that will expire in t_val seconds *)
  val create_timeout: t -> Manager.t_val -> Manager.t_fd
    
  (** the same as for managers *)
  val create_timeout_when_handling_to: t -> Manager.t_in_sig_handler -> Manager.t_val -> Manager.t_fd
    
  (** is_one_of_my_timeouts t id returns the boolean 
      "id is one of the timeouts in t",
      and if so removes it from the container.
      @return id is one of the timeouts in t.
  *)
  val is_one_of_my_timeouts: t -> Manager.t_fd -> bool
  val cancel_timeout: t -> Manager.t_fd -> unit
  val cancel_timeout_when_handling_to: t -> Manager.t_in_sig_handler -> Manager.t_fd -> unit
    
  (** delete the container and cancel all the related pending timeouts *)
  val delete: t -> unit
  
  (** the same but in sig handling *)
  val delete_in_timeout_handling: t -> Manager.t_in_sig_handler -> unit

end
