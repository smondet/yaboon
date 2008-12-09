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

You need the SimpleTestParser  camlp4 macro:

TEST_PARSER=../SimpleTestParser/SimpleTestParser.cmo

ocamlc -pp "camlp4o $TEST_PARSER " bitBuffer.mli bitBuffer.ml test_BitBuffer.ml


 
 *)

let p = Printf.printf
let spr = Printf.sprintf

open BitBuffer
open BitBuffer.Op

let prhex s outchan = (
    for i = 0 to String.length s - 1 do
        Printf.fprintf outchan "%02x" (int_of_char s.[i]);
    done;
)
let prbin s outchan = (
    let b = of_string s in 
    for i = 0 to String.length s - 1 do
        for j = 0 to 7 do
            Printf.fprintf outchan "%d" (iob (get_bit b ((8*i)+j)));
        done;
        Printf.fprintf outchan ":";
    done;

)


let tests () = (

    p "Starting BitBuffer tests\n";

    (* create(s), length *)
    let () =
        let b0 = create 0 in
        let b1 = create ~with_val:true 0 in
        let b2 = create 42 in
        let b3 = create ~with_val:true 42 in
        TEST ASSERT (length b0 = 0);
        TEST ASSERT (length b1 = 0);
        TEST ASSERT (length b2 = 42);
        TEST ASSERT (length b3 = 42);

        TEST EXCEPTION (Error `invalid_buffer_size) IN (create (-1));
        TEST EXCEPTION (Error `invalid_buffer_size) IN (create (8*Sys.max_string_length + 1));
    in

    (* of/to_string *)
    let () = 
        let b = of_string "ABBA" in
        TEST ASSERT (to_string b = "ABBA");
        TEST ASSERT (length b = (4 * 8));
        let b = of_string "" in
        TEST ASSERT (length b = 0);
        TEST ASSERT (to_string b = "");
        let b = create ~with_val:true 16 in
        TEST ASSERT (length b = 16);
        TEST ASSERT (to_string b = "\xFF\xFF");
        let b = create ~with_val:true 17 in
        TEST ASSERT (length b = 17);
        TEST ASSERT (to_string ~complete_with:true b = "\xFF\xFF\xFF");

        let s = "ABBA" in
        let b = of_string s in
        s.[0] <- 'B';
        TEST ASSERT (to_string b = "ABBA");

    in
    (* unsafe_get_string *)
    let () = 
        let b = of_string "ABBA" in
        TEST ASSERT (unsafe_get_string b = "ABBA");
    in
    (* is_byte_array *)
    let () =
        let b = of_string "ABBA" in
        TEST ASSERT (is_byte_array b);
    in

    (* get/set bit *)
    let () = 
        let etalon = "\xf0\xf0\xff\x00" in
        let b = of_string etalon in
        TEST ASSERT (to_string b = etalon);
        (* get_bit *)
        TEST ASSERT ((iob (get_bit b 0)) = 1);
        TEST ASSERT ((iob (get_bit b 1)) = 1);
        TEST ASSERT ((iob (get_bit b 2)) = 1);
        TEST ASSERT ((iob (get_bit b 3)) = 1);
        TEST ASSERT ((iob (get_bit b 4)) = 0);
        TEST ASSERT ((iob (get_bit b 5)) = 0);
        TEST ASSERT ((iob (get_bit b 6)) = 0);
        TEST ASSERT ((iob (get_bit b 7)) = 0);
        TEST ASSERT ((iob (get_bit b 8)) = 1);
        TEST ASSERT ((iob (get_bit b 9)) = 1);
        TEST ASSERT ((iob (get_bit b 10)) = 1);
        TEST ASSERT ((iob (get_bit b 11)) = 1);
        TEST ASSERT ((iob (get_bit b 12)) = 0);
        TEST ASSERT ((iob (get_bit b 13)) = 0);
        TEST ASSERT ((iob (get_bit b 14)) = 0);
        TEST ASSERT ((iob (get_bit b 15)) = 0);
        TEST ASSERT ((iob (get_bit b 16)) = 1);
        TEST ASSERT ((iob (get_bit b 17)) = 1);
        TEST ASSERT ((iob (get_bit b 18)) = 1);
        TEST ASSERT ((iob (get_bit b 19)) = 1);
        TEST ASSERT ((iob (get_bit b 20)) = 1);
        TEST ASSERT ((iob (get_bit b 21)) = 1);
        TEST ASSERT ((iob (get_bit b 22)) = 1);
        TEST ASSERT ((iob (get_bit b 23)) = 1);
        TEST ASSERT ((iob (get_bit b 24)) = 0);
        TEST ASSERT ((iob (get_bit b 25)) = 0);
        TEST ASSERT ((iob (get_bit b 26)) = 0);
        TEST ASSERT ((iob (get_bit b 27)) = 0);
        TEST ASSERT ((iob (get_bit b 28)) = 0);
        TEST ASSERT ((iob (get_bit b 29)) = 0);
        TEST ASSERT ((iob (get_bit b 30)) = 0);
        TEST ASSERT ((iob (get_bit b 31)) = 0);

        (* set_bit *)
        set_bit b  0 (boi 0);
        set_bit b  1 (boi 1);
        set_bit b  2 (boi 0);
        set_bit b  3 (boi 0);
        set_bit b  4 (boi 0);
        set_bit b  5 (boi 0);
        set_bit b  6 (boi 1);
        set_bit b  7 (boi 0);
        set_bit b  8 (boi 1);
        set_bit b  9 (boi 1);
        set_bit b 10 (boi 1);
        set_bit b 11 (boi 1);
        set_bit b 12 (boi 1);
        set_bit b 13 (boi 1);
        set_bit b 14 (boi 1);
        set_bit b 15 (boi 1);
        set_bit b 16 (boi 0);
        set_bit b 17 (boi 1);
        set_bit b 18 (boi 0);
        set_bit b 19 (boi 1);
        set_bit b 20 (boi 0);
        set_bit b 21 (boi 1);
        set_bit b 22 (boi 0);
        set_bit b 23 (boi 1);
        set_bit b 24 (boi 0);
        set_bit b 25 (boi 1);
        set_bit b 26 (boi 0);
        set_bit b 27 (boi 0);
        set_bit b 28 (boi 0);
        set_bit b 29 (boi 0);
        set_bit b 30 (boi 1);
        set_bit b 31 (boi 0);
        TEST ASSERT ((to_string b) = "\x42\xFF\x55\x42");
        TEST NAME "get/set bit 1" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (get_bit b 32);
        TEST NAME "get/set bit 2" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (set_bit b 32 true);
        TEST NAME "get/set bit 3" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (get_bit b (-1));
        TEST NAME "get/set bit 4" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (set_bit b (-1) true);
    in

    (* Write byte: *)
    let () = 
        let etalon = "\xf0\xf0\xff\x00" in
        let b = of_string etalon in
        write_byte b 0 '\x7E';
        TEST ASSERT (to_string b = "\x7E\xf0\xff\x00");
        write_byte b 9 '\x55';
        TEST ASSERT (to_string b = "\x7E\xAA\xff\x00");
        write_byte b 20 '\x0F';
        TEST ASSERT (to_string b = "\x7E\xAA\xf0\xF0");
        TEST NAME "write_byte 1" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_byte b 25 'B');
        TEST NAME "write_byte 2" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_byte b (-1) 'B');
    in

    (* write_int *)
    let () = 
        (* case 1: small int *)
        let b = create 23 in
        write_int b ~index:2 ~size:4 0b1010;
        TEST ASSERT ((to_string b).[0] = (coi 0b00101000));
        write_int b ~index:13 ~size:2 0b111010;
        TEST ASSERT ((to_string b).[1] = (coi 0b00000100));
        TEST ASSERT ((to_string ~complete_with:true b) = "\x28\x04\x01");

        (* case 2: bigger ints *)
        let b = create 51 in
        write_int b ~index:11 ~size:30 0x3cc3_c3c3;
        let wanted = "\x00\x1E\x61\xE1\xE1\x80\x1F" in
        let result = (to_string ~complete_with:true b) in
        (* p "result: %t\n" (prbin result); *)
        (* p "wanted: %t\n" (prbin wanted); *)
        TEST ASSERT (result = wanted);
        write_int b ~index:0 ~size:9 0xffff;
        let wanted = "\xFF\x9E\x61\xE1\xE1\x80\x1F" in
        let result = (to_string ~complete_with:true b) in
        TEST ASSERT (result = wanted);
        write_int b ~index:47 ~size:4 0xB;
        let wanted = "\xFF\x9E\x61\xE1\xE1\x81\x60" in
        let result = (to_string ~complete_with:false b) in
        (* p "result: %t\n" (prbin result); *)
        (* p "wanted: %t\n" (prbin wanted); *)
        TEST ASSERT (result = wanted);
        write_int b ~index:47 ~size:0 0xB;
        let result = (to_string ~complete_with:false b) in
        TEST ASSERT (result = wanted);
        (* The max: *)
        let () =
            let bb = create ~with_val:true Sys.word_size in
            write_int bb ~index:0 ~size:(Sys.word_size - 1) 0;
            let result =
                (to_string ~complete_with:false bb) in
            let wanted =
                match Sys.word_size with
                | 32 -> "\x00\x00\x00\x01"
                | 64 -> "\x00\x00\x00\x00\x00\x00\x00\x01"
                | _ -> failwith "Unkown word size !!"
            in
            (* p "result: %t\n" (prbin result); *)
            (* p "wanted: %t\n" (prbin wanted); *)
            TEST ASSERT (result = wanted);
        in

        (* Exceptions: *)
        TEST NAME "write_int 1" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_int b ~index:51 ~size:0 42);
        TEST NAME "write_int 2" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_int b ~index:45 ~size:7 42);
        TEST NAME "write_int 3" EXCEPTION (Error `invalid_int_size)
        IN (write_int b ~index:45 ~size:Sys.word_size 42);
        TEST NAME "write_int 4" EXCEPTION (Error `invalid_int_size)
        IN (write_int b ~index:45 ~size:(-1) 42);

        (* Small size but big int *)
        let b = create 20 in
        write_int b ~index:4 ~size:2 0xFF_FF_FF; 
        TEST ASSERT (to_string b = "\x0C\x00\x00")
    in

    (* write_int64 *)
    let () = 

        let b = create 45 in
        write_int64 b ~index:5 ~size:31 0xFF_FF_FF_FF_L;
        let wanted = "\x07\xFF\xFF\xFF\xF0\x00" in
        let result = (to_string ~complete_with:false b) in
        (* p "result: %t\n" (prbin result); *)
        (* p "wanted: %t\n" (prbin wanted); *)
        TEST ASSERT (result = wanted);

        let b = create ~with_val:true 80 in
        write_int64 b ~index:4 ~size:64 0L;
        let wanted = "\xF0\x00\x00\x00\x00\x00\x00\x00\x0F\xFF" in
        let result = (to_string ~complete_with:false b) in
        (* p "result: %t\n" (prbin result); *)
        (* p "wanted: %t\n" (prbin wanted); *)
        TEST ASSERT (result = wanted);

        let b = create ~with_val:true 80 in
        write_int64 b ~index:5 ~size:63 0L;
        let wanted = "\xF8\x00\x00\x00\x00\x00\x00\x00\x0F\xFF" in
        let result = (to_string ~complete_with:false b) in
        (* p "result: %t\n" (prbin result); *)
        (* p "wanted: %t\n" (prbin wanted); *)
        TEST ASSERT (result = wanted);

        let b = create ~with_val:true 80 in
        write_int64 b ~index:6 ~size:62 0L;
        let wanted = "\xFC\x00\x00\x00\x00\x00\x00\x00\x0F\xFF" in
        let result = (to_string ~complete_with:false b) in
        (* p "result: %t\n" (prbin result); *)
        (* p "wanted: %t\n" (prbin wanted); *)
        TEST ASSERT (result = wanted);

        let b = create ~with_val:true 80 in
        write_int64 b ~index:7 ~size:61 0L;
        let wanted = "\xFE\x00\x00\x00\x00\x00\x00\x00\x0F\xFF" in
        let result = (to_string ~complete_with:false b) in
        (* p "result: %t\n" (prbin result); *)
        (* p "wanted: %t\n" (prbin wanted); *)
        TEST ASSERT (result = wanted);


        (* Exceptions: *)
        let b = create 67 in
        TEST NAME "write_int64 1" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_int64 b ~index:67 ~size:0 42L);
        TEST NAME "write_int64 2" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_int64 b ~index:66 ~size:2 42L);
        TEST NAME "write_int64 3" EXCEPTION (Error `invalid_int_size)
        IN (write_int64 b ~index:45 ~size:65 42L);
        TEST NAME "write_int64 4" EXCEPTION (Error `invalid_int_size)
        IN (write_int64 b ~index:45 ~size:(-1) 42L);


    in

    (* Read byte: *)
    let () = 
        let b = create 80 in
        write_int b ~index:0 ~size:24 0b10111011_01010101_11001100; 
        TEST ASSERT (read_byte b 0 = (coi 0b10111011));
        TEST ASSERT (read_byte b 1 = (coi  0b0111011_0));
        TEST ASSERT (read_byte b 2 = (coi   0b111011_01));
        TEST ASSERT (read_byte b 4 = (coi     0b1011_0101));
        TEST ASSERT (read_byte b 7 = (coi        0b1_0101010));
        TEST ASSERT (read_byte b 11 = (coi 0b10101_110));
        TEST ASSERT (read_byte b 16 = (coi 0b11001100));
        TEST ASSERT (read_byte b 21 = (coi      0b100_00000));
        TEST ASSERT (read_byte b 72 = (coi 0b00000000));
        TEST NAME "read_byte 1" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (read_byte b 80);
        TEST NAME "read_byte 2" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (read_byte b (-1));
        TEST NAME "read_byte 3" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (read_byte b 73);
    in


    (* Read int: *)
    let () = 
        let b =  create 80 in
        write_int64 b ~index:8 ~size:40
            0b10111011_01010101_11001100_10101010_11101110L;
        TEST ASSERT (read_int b ~index:0 ~size:8 = 0);
        TEST ASSERT (read_int b ~index:0 ~size:16 = 0b10111011);
        TEST ASSERT (read_int b ~index:2 ~size:16 = 0b10111011_01);
        TEST ASSERT (read_int b ~index:3 ~size:16 = 0b10111011_010);
        TEST ASSERT (read_int b ~index:5 ~size:16 = 0b10111011_01010);
        TEST ASSERT (read_int b ~index:8 ~size:16 = 0b10111011_01010101);

        TEST ASSERT (read_int b ~index:10 ~size:3 = 0b111);
        TEST ASSERT (read_int b ~index:14 ~size:4 = 0b11_01);

        TEST ASSERT (read_int b ~index:23 ~size:31 = 0b1_11001100_10101010_11101110_000000);
        TEST ASSERT (read_int b ~index:8 ~size:31 = 0b10111011_01010101_11001100_1010101);
        begin match Sys.word_size with
        | 32 -> 
                TEST ASSERT (read_int b ~index:6 ~size:31 =
                     0b10111011_01010101_11001100_10101);
        | 64 ->
                TEST ASSERT (read_int b ~index:8 ~size:63 =
                    Int64.to_int
                    0b10111011_01010101_11001100_10101010_11101110_00000000_00000000_0000000L);
        | _ -> failwith "Unknown word_size";
        end;

        (* Exceptions: *)
        TEST NAME "read_int 1" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (read_int b ~index:80 ~size:0);
        TEST NAME "read_int 2" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (read_int b ~index:74 ~size:7);
        TEST NAME "read_int 3" EXCEPTION (Error `invalid_int_size)
        IN (read_int b ~index:45 ~size:Sys.word_size);
        TEST NAME "read_int 4" EXCEPTION (Error `invalid_int_size)
        IN (read_int b ~index:45 ~size:(-1));
    in

    (* Read int64: *)
    let () = 
        let b =  create 80 in
        write_int64 b ~index:8 ~size:64
            0b10111011_01010101_11001100_10101010_11101110_01010101_11001100_11100011L;
        (* p "wanted: %t\n" (prbin (to_string b)); *)
        TEST ASSERT (read_int64 b ~index:0 ~size:8 = 0L);
        TEST ASSERT (read_int64 b ~index:72 ~size:8 = 0L);
        TEST ASSERT (read_int64 b ~index:0 ~size:16 = 0b10111011L);
        TEST ASSERT (read_int64 b ~index:2 ~size:16 = 0b10111011_01L);
        TEST ASSERT (read_int64 b ~index:15 ~size:16 = 0b1_01010101_1100110L);

        TEST ASSERT (read_int64 b ~index:3 ~size:31 = 0b00000_10111011_01010101_11001100_10L);
        TEST ASSERT (read_int64 b ~index:5 ~size:31 =   0b000_10111011_01010101_11001100_1010L);
        TEST ASSERT (read_int64 b ~index:15 ~size:31 =   0b1_01010101_11001100_10101010_111011L);

        TEST ASSERT (read_int64 b ~index:3 ~size:32 = 0b00000_10111011_01010101_11001100_101L);
        TEST ASSERT (read_int64 b ~index:5 ~size:32 =   0b000_10111011_01010101_11001100_10101L);
        TEST ASSERT (read_int64 b ~index:15 ~size:32 =   0b1_01010101_11001100_10101010_1110111L);

        TEST ASSERT (read_int64 b ~index:3 ~size:62 =
            0b00000_10111011_01010101_11001100_10101010_11101110_01010101_11001100_1L);
        TEST ASSERT (read_int64 b ~index:5 ~size:62 = 
              0b000_10111011_01010101_11001100_10101010_11101110_01010101_11001100_111L);
        TEST ASSERT (read_int64 b ~index:10 ~size:62 = 
            0b111011_01010101_11001100_10101010_11101110_01010101_11001100_11100011L);

        TEST ASSERT (read_int64 b ~index:3 ~size:63 =
            0b00000_10111011_01010101_11001100_10101010_11101110_01010101_11001100_11L);
        TEST ASSERT (read_int64 b ~index:5 ~size:63 = 
              0b000_10111011_01010101_11001100_10101010_11101110_01010101_11001100_1110L);
        TEST ASSERT (read_int64 b ~index:10 ~size:63 = 
            0b111011_01010101_11001100_10101010_11101110_01010101_11001100_111000110L);
        

        TEST ASSERT (read_int64 b ~index:3 ~size:64 =
            0b00000_10111011_01010101_11001100_10101010_11101110_01010101_11001100_111L);
        TEST ASSERT (read_int64 b ~index:5 ~size:64 = 
              0b000_10111011_01010101_11001100_10101010_11101110_01010101_11001100_11100L);
        TEST ASSERT (read_int64 b ~index:10 ~size:64 = 
            0b111011_01010101_11001100_10101010_11101110_01010101_11001100_11100011_00L);

        set_bit b 79 true;
        TEST ASSERT (read_int64 b ~index:16 ~size:64 = 
            0b01010101_11001100_10101010_11101110_01010101_11001100_11100011_00000001L);

        (* Exceptions: *)
        TEST NAME "read_int64 1" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (read_int64 b ~index:80 ~size:0);
        TEST NAME "read_int64 2" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (read_int64 b ~index:74 ~size:7);
        TEST NAME "read_int64 3" EXCEPTION (Error `invalid_int_size)
        IN (read_int64 b ~index:45 ~size:65);
        TEST NAME "read_int64 4" EXCEPTION (Error `invalid_int_size)
        IN (read_int64 b ~index:45 ~size:(-1));

    in

    (* Some more on int64's *)
    let () =
        let bb = create 83 in
        for i = 0 to 82 do set_bit bb i (Random.bool ()) done;
        let cc = create 64 in
        let i1 = read_int64 bb ~index:0 ~size:64 in
        write_int64 cc ~index:0 ~size:64 i1;
        TEST ASSERT (read_int64 cc ~index:0 ~size:64 = i1);

        let i2 = read_int64 bb ~index:19 ~size:64 in
        write_int64 cc ~index:0 ~size:64 i2;
        TEST ASSERT (read_int64 cc ~index:0 ~size:64 = i2);

        let i3 = read_int64 bb ~index:80 ~size:3 in
        write_int64 cc ~index:0 ~size:64 i3;
        TEST ASSERT (read_int64 cc ~index:0 ~size:64 = i3);

        for index = 0 to 83 - 8 do
            let c = coi (Random.int 256) in
            write_byte bb index c;
            TEST ASSERT (read_int64 bb ~index ~size:8 = Int64.of_int (ioc c));
        done;
    in

    (* Write buffer *)
    let () = 

        (* The source *)
        let src = create 67 in
        write_int64 src ~index:0 ~size:64
            0b11001100_11101110_11110000_10101010_11011101_00100010_00001111_11000011L;
        let src_save = to_string src in (* for the last test *)

        (* First test family: Small destination *)
        let dst = create 5 in
        write_int dst ~index:0 ~size:5 0b11011;
        (* let b_before = (to_string dst) in *)

        write_buffer ~dst ~dst_index:1 ~size:3 ~src ~src_index:8;
        TEST ASSERT (read_int dst ~index:0 ~size:5 = 0b11111);

        write_buffer ~dst ~dst_index:0 ~size:4 ~src ~src_index:25; (* write 0101 *)
        TEST ASSERT (read_int dst ~index:0 ~size:5 = 0b01011);

        write_buffer ~dst ~dst_index:1 ~size:4 ~src ~src_index:18; (* write 1100 *)
        TEST ASSERT (read_int dst ~index:0 ~size:5 = 0b01100);

        TEST NAME "write_buffer fst 1" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_buffer ~dst ~dst_index:1 ~size:5 (* <- *) ~src ~src_index:18;);
        TEST NAME "write_buffer fst 2" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_buffer ~dst ~dst_index:(-1) (* <- *) ~size:4 ~src ~src_index:25;);
        TEST NAME "write_buffer fst 3" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_buffer ~dst ~dst_index:0 ~size:4 ~src ~src_index:(-1) (* <- *););
        TEST NAME "write_buffer fst 4" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_buffer ~dst ~dst_index:0 ~size:2 ~src ~src_index:200 (* <- *););
        TEST NAME "write_buffer fst 5" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_buffer ~dst ~dst_index:0 ~size:4 ~src ~src_index:64 (* 64 65 66 [67] <- *););

        (* Second test family: Big destination *)
        let dst = create 203 in
        let reinit dst =
            write_int64 dst ~index:0 ~size:55 
                0b11001100_11101110_11110000_10101010_11011101_00100010_0001111L;
            write_int64 dst ~index:56 ~size:55 (* there will be a zero between two *)
                0b11001100_11101110_11110000_10101010_11011101_00100010_0001111L;
                (* 0b[56]11001100_[64]11101110_11110000_10101010_11011101_00100010_0001111L; *)
        in
        reinit dst;
        (* let c_before = (to_string dst) in *)


        (* Same as first family *)
        write_buffer ~dst ~dst_index:1 ~size:3 ~src ~src_index:8;
        TEST ASSERT (read_int dst ~index:0 ~size:5 = 0b11111);
        write_buffer ~dst ~dst_index:0 ~size:4 ~src ~src_index:25; (* write 0101 *)
        TEST ASSERT (read_int dst ~index:0 ~size:5 = 0b01011);
        write_buffer ~dst ~dst_index:1 ~size:4 ~src ~src_index:18; (* write 1100 *)
        TEST ASSERT (read_int dst ~index:0 ~size:5 = 0b01100);

        (* Now bigger, longer, uncut *)
        reinit dst;

        write_buffer ~dst ~dst_index:1 ~size:64 ~src ~src_index:2;
        TEST ASSERT (read_int dst ~index:0 ~size:5 = 0b10011);
        (* p "%x %x\n" (read_int dst ~index:0 ~size:5) 0b10011; *)
        TEST ASSERT (read_int64 dst ~index:1 ~size:64 = 
            0b001100_11101110_11110000_10101010_11011101_00100010_00001111_11000011_00L
        );
        TEST ASSERT (read_int dst ~index:65 ~size:7 = 0b1101110);

        write_int dst ~index:137 ~size:5 0b11011;
        write_buffer ~dst ~dst_index:139 ~size:64 ~src ~src_index:3;
        TEST ASSERT (read_int dst ~index:137 ~size:5 = 0b11011);
        (* p "%x %x\n" (read_int dst ~index:0 ~size:5) 0b10011; *)
        TEST ASSERT (read_int64 dst ~index:139 ~size:64 = 
            0b01100_11101110_11110000_10101010_11011101_00100010_00001111_11000011_000L
        );
        TEST ASSERT (read_int dst ~index:198 ~size:5 = 0b11000);



        TEST NAME "write_buffer fst 1" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_buffer ~dst ~dst_index:1 ~size:203 (* <- *) ~src ~src_index:18;);
        TEST NAME "write_buffer fst 2" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_buffer ~dst ~dst_index:(-1) (* <- *) ~size:4 ~src ~src_index:25;);
        TEST NAME "write_buffer fst 3" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_buffer ~dst ~dst_index:0 ~size:4 ~src ~src_index:(-1) (* <- *););
        TEST NAME "write_buffer fst 4" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_buffer ~dst ~dst_index:0 ~size:2 ~src ~src_index:200 (* <- *););
        TEST NAME "write_buffer fst 5" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_buffer ~dst ~dst_index:0 ~size:4 ~src ~src_index:64 (* 64 65 66 [67] <- *););

        let ba = of_string "AAAA" in
        let bb = of_string "BBBB" in
        TEST EXCEPTION (Error `invalid_buffer_size)
        IN (write_buffer ~dst:ba ~dst_index:5 ~size:(-1) ~src:bb ~src_index:4);
        TEST EXCEPTION (Error `invalid_buffer_size)
        IN (write_buffer ~dst:ba ~dst_index:5 ~size:(8*Sys.max_string_length + 1) ~src:bb ~src_index:4);


        TEST ASSERT (to_string src = src_save);
        (* p "after1: b=%t\n" (prbin (to_string b)); *)
        (* p "after1: c=%t\n" (prhex (to_string c)); *)
    in


    (* Zero Sizes: *)
    let () =
        let b = create 0 in
        TEST ASSERT (to_string b = "");
        TEST ASSERT (is_byte_array b);
        TEST ASSERT (to_string (of_string "") = "");
        TEST NAME "zero 1" EXCEPTION (Error `bit_index_out_of_bounds) IN (set_bit b 0 true;);
        TEST NAME "zero 1" EXCEPTION (Error `bit_index_out_of_bounds) IN (write_byte b 0 'B';);
        let size = 0 in
        let bb = of_string "BBBB" in
        let cc = create 0 in
        write_int bb ~index:2 ~size 0b1111111;
        write_int bb ~index:0 ~size 0b1111111;
        TEST NAME "zero 3" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_int cc ~index:0 ~size 0b1111111;);
        TEST ASSERT (to_string bb = "BBBB");
        write_int64 bb ~index:2 ~size 0b1111111L;
        write_int64 bb ~index:0 ~size 0b1111111L;
        TEST NAME "zero 4" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (write_int64 cc ~index:0 ~size 0b1111111L;);
        TEST ASSERT (to_string bb = "BBBB");
        TEST ASSERT (read_int bb ~index:2 ~size = 0);
        TEST ASSERT (read_int bb ~index:0 ~size = 0);
        TEST NAME "zero 5" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (read_int cc ~index:0 ~size);
        TEST ASSERT (read_int64 bb ~index:2 ~size = 0L);
        TEST ASSERT (read_int64 bb ~index:0 ~size = 0L);
        TEST NAME "zero 6" EXCEPTION (Error `bit_index_out_of_bounds)
        IN (read_int64 cc ~index:0 ~size);

        write_buffer ~dst:bb ~dst_index:2 ~src:(of_string "\xff") ~src_index:4 ~size;
        write_buffer ~dst:bb ~dst_index:0 ~src:(of_string "\xff") ~src_index:4 ~size;
        TEST ASSERT (to_string bb = "BBBB");
    in

    (* String of error *)
    let () =
        TEST ASSERT (string_of_error `bit_index_out_of_bounds <> "");
        TEST ASSERT (string_of_error `invalid_int_size <> "");
        TEST ASSERT (string_of_error `invalid_int_size <> string_of_error `bit_index_out_of_bounds);

        (* no mutability problems: *)
        let s = string_of_error `bit_index_out_of_bounds in
        TEST ASSERT (s = string_of_error `bit_index_out_of_bounds);
        s.[2] <- 'B';
        TEST ASSERT (s <> string_of_error `bit_index_out_of_bounds);


    in

    (* copy *)
    let () =
        let b = create 23 in write_int b ~index:0 ~size:23 0b1110000_10101010_00110011;
        let c = copy b in
        TEST ASSERT (length c = 23);
        TEST ASSERT ((to_string b) = (to_string c));
        set_bit b 8 true;
        TEST ASSERT (get_bit c 8 = false);
    in
    (* append *)
    let () =
        let b = create 23 in write_int b ~index:0 ~size:23 0b1110000_10101010_00110011;
        let c = create 17 in write_int c ~index:0 ~size:17 0b1_01010101_00110011;
        append b c;
        TEST ASSERT (length c = 17);
        TEST ASSERT (length b = (17 + 23));
        TEST ASSERT (get_bit b 1 = true);
        TEST ASSERT (get_bit b 6 = false);
        TEST ASSERT (get_bit b 23 = true);
        TEST ASSERT (get_bit b 24 = false);
        set_bit c 1 true;
        TEST ASSERT (get_bit b 24 = false);
    in
    (* sub *)
    let () =
        let b = create 23 in write_int b ~index:0 ~size:23 0b1110000_10101010_00110011;
        let c = sub b ~index:7 ~size:8 in
        TEST ASSERT (length c = 8);
        TEST ASSERT ((to_string c).[0] = (char_of_int 0b10101010));
        set_bit b 8 true;
        TEST ASSERT (get_bit c 1 = false);
    in

    (* concat *)
    let () =
        (* without 'size' *)
        let a1 = create 23 in write_int a1 ~index:0 ~size:23 0b1110000_10101010_00110011;
        let a2 = create 17 in write_int a2 ~index:0 ~size:17 0b1_01010101_00110011;
        let a3 = create 30 in write_int a3 ~index:0 ~size:30 0b110000_10101010_00110011_11101110;
        let a4 = create  3 in write_int a4 ~index:0 ~size:3  0b101;
        let b = concat [a1;a2;a3;a4] in
        TEST ASSERT (read_int b ~index:0   ~size:16 = 0b1110000_10101010_0);
        TEST ASSERT (read_int b ~index:16  ~size:16 = 0b01100111_01010101);
        TEST ASSERT (read_int b ~index:32  ~size:16 = 0b00110011_110000_10);
        TEST ASSERT (read_int b ~index:48  ~size:16 = 0b101010_00110011_11);
        TEST ASSERT (read_int b ~index:64  ~size:9 = 0b101110_101);
        TEST ASSERT (length b = 23 + 17 + 30 + 3);

        let a5 = create 0 in
        let b = concat [a1;a5;a2;] in
        TEST ASSERT (read_int b ~index:0   ~size:23 = 0b1110000_10101010_00110011);
        TEST ASSERT (read_int b ~index:23  ~size:17 = 0b1_01010101_00110011);
        TEST ASSERT (length b = 23 + 17);

        let b = concat ~size:68 [a1;a2;a3;a4] in
        TEST ASSERT (read_int b ~index:0   ~size:16 = 0b1110000_10101010_0);
        TEST ASSERT (read_int b ~index:16  ~size:16 = 0b01100111_01010101);
        TEST ASSERT (read_int b ~index:32  ~size:16 = 0b00110011_110000_10);
        TEST ASSERT (read_int b ~index:48  ~size:16 = 0b101010_00110011_11);
        TEST ASSERT (read_int b ~index:64  ~size:4 = 0b1011);
        TEST ASSERT (length b = 68);

        let b = concat ~size:89 [a1;a2;a3;a4] in
        TEST ASSERT (read_int b ~index:0   ~size:16 = 0b1110000_10101010_0);
        TEST ASSERT (read_int b ~index:16  ~size:16 = 0b01100111_01010101);
        TEST ASSERT (read_int b ~index:32  ~size:16 = 0b00110011_110000_10);
        TEST ASSERT (read_int b ~index:48  ~size:16 = 0b101010_00110011_11);
        TEST ASSERT (read_int b ~index:64  ~size:15 = 0b101110_101_000000);
        TEST ASSERT (length b = 89);

        let a1 = create 4 in write_int a1 ~index:0 ~size:4 0b1110;
        let a2 = create 7 in write_int a2 ~index:0 ~size:7 0b1_010101;
        let a3 = create 3 in write_int a3 ~index:0 ~size:3  0b101;
        let b = concat [a1;a2;a3] in
        TEST ASSERT (read_int b ~index:0 ~size:14 = 0b11101010_101101);
        (* let result = (to_string ~complete_with:true b) in *)
        (* p "result: %t\n" (prbin result); *)

        let b = concat ~size:0 [a1;a2;a3;a4] in
        TEST ASSERT (length b = 0);

        TEST EXCEPTION (Error `invalid_buffer_size)
        IN (concat ~size:(-1) [a1;a2;a3;a4]);

        TEST EXCEPTION (Error `invalid_buffer_size)
        IN (concat ~size:(8*Sys.max_string_length + 1) [a1;a2;a3;a4]);

        begin try
            let too_big_list = 
                let the_size = Sys.max_string_length - 2 in
                let one_item () = create the_size in
                [
                    one_item ();
                    one_item ();
                    one_item ();
                    one_item ();
                    one_item ();
                    one_item ();
                    one_item ();
                    one_item ();
                    one_item ();
                    create 40;
                ]
            in
            TEST EXCEPTION (Error `invalid_buffer_size)
            IN (concat too_big_list);
        with
        Out_of_memory -> 
            p "[Warning]: the very big string test has gone out of memory\n";
        end;

    in

    ()
)


let () = 
    let test_end =
        try tests (); "OK"
        with
        | BitBuffer.Error e ->
            p "BitBuffer exception: %s\n" (BitBuffer.string_of_error e);
            raise (BitBuffer.Error e);
        | e ->
            raise e;
    in
    p "End of the tests: %s\n" test_end;

