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

let string_returned = function
    | `ok -> "`ok"
    | `error_EFAULT -> "`error_EFAULT"
    | `error_EINVAL -> "`error_EINVAL"
    | `interrupted (s, n) -> Printf.sprintf "`interrupted %d, %d" s n

let pr = Printf.printf

let debugging = not true

let test_ret r v = (
    if debugging then (
        pr "ret: %s  exp:%s\n%!" (string_returned r) (string_returned v);
    );
    assert (r = v);
)

let () = (

    let normal1 = Unsafix.nanosleep 1 5_000_000 in
    test_ret normal1 `ok;
    let invalid_arg1 = Unsafix.nanosleep 1 1_000_000_000 in
    test_ret invalid_arg1 `error_EINVAL;
    let invalid_arg2 = Unsafix.nanosleep (-10) 1_000_000 in
    test_ret invalid_arg2 `error_EINVAL;


    pr "===== Forked test =====\n%!";
    let pid = Unix.fork () in
    if pid = 0 then (
        Sys.set_signal Sys.sigusr1
            (* (Sys.Signal_handle (fun _ -> pr "Signaled !!!\n%!")); *)
            (Sys.Signal_handle (fun _ -> ()));
        let s = 10 in
        pr "%f: Child runs nanosleep(%d secs)\n%!" (Unix.gettimeofday ()) s;
        let interupted1 = Unsafix.nanosleep s 0 in
        begin match interupted1 with
        | `interrupted (s, n) ->
                pr "%f: Child runs nanosleep(%d, %d)\n%!"
                    (Unix.gettimeofday ()) s n;
                let normal3 = Unsafix.nanosleep s n in
                test_ret normal3 `ok;
        | _ ->
                pr "%f: ERROR child gets: %s\n%!"
                    (Unix.gettimeofday ()) (string_returned interupted1);
        end;
        pr "%f: End for the Child\n%!" (Unix.gettimeofday ());
    ) else (
        let s = 2 in
        pr "Parent (%d) runs nanosleep(%d secs)\n%!" (Unix.getpid ()) s;
        let normal2 = Unsafix.nanosleep s 0 in
        pr "and father gets: %s\n" (string_returned normal2);
        Unix.kill pid Sys.sigusr1;
        ignore (Unix.waitpid [] pid);
    );

)
