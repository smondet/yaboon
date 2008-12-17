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

let pr = Printf.printf
let spr = Printf.sprintf


(* PF_INET in /usr/include/linux/socket.h *)
let d_PF_INET = 2
(* SOCK_STREAM in /usr/include/sys/socket.h *)
let d_SOCK_STREAM = 1
(* SOL_DCCP in /usr/include/linux/socket.h *)
let d_SOL_DCCP = 269
(* IPPROTO_TCP in /usr/include/netinet/in.h *)
let d_IPPROTO_TCP = 6
(* SOL_DCCP in /usr/include/linux/socket.h *)
let d_SOL_DCCP = 269
(* TCP_MAXSEG in /usr/include/linux/tcp.h *)
let d_TCP_MAXSEG = 2
(* SO_REUSEADDR in /usr/include/asm-i386/socket.h *)
let d_SO_REUSEADDR = 2


let nd_SOCK_DCCP = 6
let nd_IPPROTO_DCCP = 33


let server () = (
    let socket =
        match
            (Unsafix.socket_opt
                ~domain:d_PF_INET ~sock_type:nd_SOCK_DCCP
                ~protocol:nd_IPPROTO_DCCP)
        with
        | None -> failwith "Didn't accept DCCP ?"
        | Some s -> s
    in

    let port, qlen = 2233, 6 in
    let sockadr = Unix.ADDR_INET (Unix.inet_addr_any , port) in
    Unix.bind socket sockadr;
    Unix.listen socket qlen;

    let client_socket, _ = Unix.accept socket in
    let succeded = ref false in

    let data = String.make 3000 '\x42' in
    let data_size = 1350 in

    while not !succeded do
        begin try
            let status = Unix.send client_socket data 0 data_size [] in
            if status < 0 then failwith "status < 0 in DCCP send";
            succeded := true;
        with
        Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
        end;
    done;

)

let read_buf file_desc ?(finally=fun (a:int) -> ()) size = (
    let buf = String.create size in
    let read_bytes = ref 0 in
    begin try
        while !read_bytes < size do
            let read =
                Unix.read file_desc buf !read_bytes (size - !read_bytes) in
            if (read = 0) then (failwith "Read 0 bytes !!!");
            read_bytes := !read_bytes + read;
        done;
        with e -> finally !read_bytes ; raise e;
    end;
    buf
)


let client ?(adr="127.0.0.1") ?(port=2233) () = (
    pr "Client pausing 2 seconds...\n%!";
    Unix.sleep 2;

    let inet_addr_of_string str = (
        let ll = Unix.gethostbyname str in
        let aa = ll.Unix.h_addr_list in
        aa.(0)
    ) in
    let serv_adr =  Unix.ADDR_INET (inet_addr_of_string adr, port) in
    let to_server_socket =
        match
            (Unsafix.socket_opt
                ~domain:d_PF_INET ~sock_type:nd_SOCK_DCCP
                ~protocol:nd_IPPROTO_DCCP)
        with
        | None -> failwith "Didn't accept DCCP ?"
        | Some s -> s
    in

    Unix.connect to_server_socket serv_adr;

    let buf = read_buf to_server_socket 1350 in

    pr "Got message from server: \"%s...\"\n" (String.sub buf 0 8);

)

let () = (
    let pid = Unix.fork () in
    if pid = 0 then (
        client ();
    ) else (
        pr "Client is: %d\n%!" pid;
        server ();
    );

)

