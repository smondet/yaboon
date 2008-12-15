

let pr = Printf.printf


(* PF_INET in /usr/include/linux/socket.h *)
let d_PF_INET = 2

(* PF_INET6 in /usr/include/linux/socket.h *)
let d_PF_INET6 = 10

(* SOCK_STREAM in /usr/include/sys/socket.h *)
let d_SOCK_STREAM = 1

(* SOL_DCCP in /usr/include/linux/socket.h *)
let d_SOL_DCCP = 269

(* SO_REUSEADDR in /usr/include/asm-i386/socket.h *)
let d_SO_REUSEADDR = 2

(* IPPROTO_TCP in /usr/include/netinet/in.h *)
let d_IPPROTO_TCP = 6
(* TCP_NODELAY in /usr/include/linux/tcp.h *)
let d_TCP_NODELAY = 1
(* TCP_MAXSEG in /usr/include/linux/tcp.h *)
let d_TCP_MAXSEG = 2



let () = (
    let s = Unsafix.socket ~domain:0 ~sock_type:0 ~protocol:0 in
    pr "s: %d\n" (Obj.magic s: int);
    let t = 
        Unsafix.socket
            ~domain:d_PF_INET6
            ~sock_type:d_SOCK_STREAM
            ~protocol:0
    in
    pr "t: %d\n" (Obj.magic t: int);

    let err_msg () =  (Unix.error_message (Unsafix.current_errno ())) in

    let getsockopt_str_test s level optname optlength = 
        let gos1 =
            Unsafix.getsockopt_str  s ~level ~optname ~optlength in
        pr "GetSockOptString (s, %d, %d, %d) gives " level optname optlength;
        let ics s i = Printf.sprintf "%02x" (int_of_char s.[i]) in
        begin match gos1 with
        | `ok s -> pr ":%s%s%s%s:\n" (ics s 0) (ics s 1) (ics s 2) (ics s 3) 
        | `error i -> pr "ERROR: %d\n" i
        | `optlength_overflow -> pr "\"Length given is too big\"\n"
        end;
    in
    let test_sockopt str level optname v = (
        pr "Testing **%s**\n" str;
        let rg1 = 
            Unsafix.getsockopt_int t ~level ~optname in
        pr "GetSockOpt (%s) -> %d\n" str rg1;
        pr "    (error_message: %s)\n" (err_msg ());
        let r = 
            Unsafix.setsockopt_int t ~level ~optname ~optval:v in
        pr "Setsockopt (%s) -> %d\n" str r;
        if r <> 0 then (
            pr "  error_message: %s\n" (err_msg ());
        );
        let rg2 = 
            Unsafix.getsockopt_int t ~level ~optname in
        pr "GetSockOpt (%s) -> %d\n" str rg2;
        if rg2 <> v then (
            pr "   error_message: %s\n" (err_msg ());
        );
    ) in
    test_sockopt "NODELAY, 1" d_IPPROTO_TCP d_TCP_NODELAY 1;
    test_sockopt "MAXSEG, 1001" d_IPPROTO_TCP d_TCP_MAXSEG 1001;
    
    getsockopt_str_test t d_IPPROTO_TCP d_TCP_MAXSEG 4;
    
    let wrong_socket =
        Unsafix.socket 
            ~domain:d_PF_INET6 ~sock_type:56 ~protocol:123
    in
    pr "GetSockOpt: %d\n" (
        Unsafix.getsockopt_int t ~level:d_IPPROTO_TCP
            ~optname:d_SO_REUSEADDR);   
    pr "error_message: %s\n" (Unix.error_message (Unsafix.current_errno ()));
    pr "wrong_socket: %d\n" (Obj.magic wrong_socket: int);
    pr "error_message: %s\n" (Unix.error_message (Unsafix.current_errno ()));
    pr "GetSockOptString ERRORS:\n";
    getsockopt_str_test t d_IPPROTO_TCP 424242 4;
    getsockopt_str_test t d_IPPROTO_TCP 424242 400;
)

