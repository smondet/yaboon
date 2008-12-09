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
---------
Bugs:
    x ~size:(-1) not checked !!!
    x of_string does not copy the string (should be at least optional)

Features:
- concatenations:
    x concat (functional)
    x append (imperative... but done with a copy ;-) )
    - append_list
- writes (imperatives + exceptions):
    x write_byte (c.f. write_contest.ml for the implementation choice)
    x write_int
    x write_int64
    x write_buffer
- reads 
    x read_byte
    x read_int (with size)
    x read_int64
    - read_buffer
    - read_string
    x sub (functional... = read_buffer ?)
- functional:
    - iter_byte: buffer -> (char -> unit) -> ?ofs:int -> ?size:int -> unit
    - iter_bit: with (bool -> unit)
    - for Unix.write or Pervasives.output:
        - apply_write: buffer -> write:(string -> int -> int -> int)
            -> ?fill:bool -> ?index:int -> ?size:int -> int
        - apply_output: buffer -> output:(string -> int -> int -> unit)
            -> ?fill:bool -> ?index:int -> ?size:int -> unit
- copies:
    x copy: buffer -> buffer
- misc:
    x error to string
- HumanReadable module:
    - to_string (printf like ??)
- Conversions:
    - of_int(64)
    ? of_list: e.g. [`int 42,16; `string "bouh",18; ...]
? Parsing1:
    - parse: 'format string' -> [ elements list ]
? Parsing2:
    - bitstream type (a buffer, and a current bit)
    - type_in = [ `int; `float; `bool; `char; `string;]
    - type_out = [ `int of int; `float of float; `bool of bool; ... ; `parsing_error]
    - read: bitstream -> size -> type_in -> type_out
? Parsing3:
    - bitstream type (a buffer, and a current bit)
    - read_int: ?size -> bitstream -> int 
    - read_float: ?size ->  -> bitstream -> float
    - ...

 
 
 
 *)

module Op = struct 
    let ioc = int_of_char
    let coi = char_of_int
    let cob = function true -> '\xff' | false -> '\x00'

    let boi = (<>) 0
    let iob = function true -> 1 | false -> 0
end
open Op

let p = Printf.printf

let charsize = 8


type buffer = {
    mutable b_string: string;
    mutable b_bit_length: int;
}

type error = [
    | `bit_index_out_of_bounds
    | `invalid_int_size
    | `invalid_buffer_size
]

let string_of_error (e:error) = (
    (* we don't want anybody to write in our internal strings ;-) *)
    String.copy (match e with
    | `bit_index_out_of_bounds -> "Bit index out of bounds"
    | `invalid_int_size -> "Invalid int size"
    | `invalid_buffer_size -> "Invalid buffer size")
)


exception Error of error

module Internal = struct

    let end_bits c nb_bits complete = (
        if complete then 
            ((ioc c) lor ((1 lsl ( 8 - nb_bits)) - 1))
        else 
            ((ioc c) land (lnot ((1 lsl ( 8 - nb_bits)) - 1)))
            (* (ioc c) is a 8 bits integer
             * the (lnot ....) is  11...11100...00 with (8 - nb_bits) zeros *)
    )

    let check_bounds_exn t i = (
        if not (0 <= i && i < t.b_bit_length) then (
            raise (Error `bit_index_out_of_bounds);
        )
    )

    let check_bit_size i = (
        if not (0 <= i && i <= (8 * Sys.max_string_length)) then (
            raise (Error `invalid_buffer_size);
        );
    )

    (* 
     * Sets bits [start_from, 7] of str.[byte_index]
     * with high bits of value byte_v
     *)
    let set_low_bits_of_char str byte_index start_from byte_v = (
        let mask_for_string = (* 11100000 *)
            ((1 lsl start_from) - 1) lsl (charsize - start_from) in
        let mask_for_value = (* 11111000 *)
            ((1 lsl (charsize - start_from)) - 1) lsl start_from in
        str.[byte_index] <- coi (
            ((ioc str.[byte_index]) land (mask_for_string))
            lor (((ioc byte_v) land mask_for_value) lsr start_from)
        );
    )

    (* 
     * Sets bits [0, go_until - 1] of str.[byte_index] with value byte_v
     *)
    let set_high_bits_of_char str byte_index go_until byte_v = (
        let mask_for_string = (1 lsl (charsize - go_until)) - 1  in
        let mask_for_value = (1 lsl go_until) - 1 in
        str.[byte_index] <- coi (
            ((ioc str.[byte_index]) land (mask_for_string))
            lor
            (((ioc byte_v) land mask_for_value) lsl (charsize - go_until))
        );
    )

    let set_char_with_mask buf index mask v = (
        let antimask = mask lxor 0b1111_1111 in
        buf.[index] <-
            coi (((ioc buf.[index]) land antimask) lor ((ioc v) land mask));
    )
end



let make_struct ?(str="")  ?(lgth=0) () = {
    b_string = str;
    b_bit_length = lgth;
}

let create ?(with_val=false) size = (
    Internal.check_bit_size size;
    let str_size = 
        if size mod charsize = 0 then
            size / charsize
        else
            (size / charsize) + 1
    in
    make_struct ~str:(String.make str_size (cob with_val)) ~lgth:size ()
)

let length t = t.b_bit_length

let copy b = (
    make_struct ~str:(String.copy b.b_string) ~lgth:b.b_bit_length ()
)



let unsafe_get_string b = b.b_string

let of_string str = (
    let lgth = 8 * (String.length str) in
    make_struct ~str:(String.copy str)  ~lgth ()
)

let is_byte_array b = b.b_bit_length mod charsize = 0

let to_string ?(complete_with=false) b = (
    let str =  String.copy b.b_string in
    if is_byte_array b then str else (
        str.[b.b_bit_length  / charsize] <- coi (
            Internal.end_bits str.[b.b_bit_length  / charsize]
            (b.b_bit_length mod charsize)
            complete_with
        );
        str
    )
)

    
let get_bit t i = (
    Internal.check_bounds_exn t i;
    let index_in_array = i / charsize in
    let index_in_char = i mod charsize in
    let power_of_two = 0x80 lsr index_in_char in
    ((ioc t.b_string.[index_in_array]) land power_of_two) <> 0
)

let set_bit t i v = (
    Internal.check_bounds_exn t i;
    let index_in_array = i / charsize in
    let index_in_char = i mod charsize in
    let power_of_two = 0x80 lsr index_in_char in
    let current =
        ((ioc t.b_string.[index_in_array]) land power_of_two) <> 0 in
    if current = v then
        ()(* nothing to do *)
    else (
        if v then (
            t.b_string.[index_in_array] <- coi (
                (ioc t.b_string.[index_in_array]) lor power_of_two);
        ) else (
            t.b_string.[index_in_array] <- coi (
                (ioc t.b_string.[index_in_array]) lxor power_of_two);
        )
    )
)


let write_byte buf index v = (
    Internal.check_bounds_exn buf index;
    Internal.check_bounds_exn buf (index + charsize - 1);
    let ofset_in_byte = index mod charsize in
    if ofset_in_byte = 0 then (
        buf.b_string.[index / charsize] <- v;
    ) else (
        (* setting the first semi-byte *)
        Internal.set_low_bits_of_char
            buf.b_string (index / charsize) ofset_in_byte v;
        (* setting the second part *)
        Internal.set_high_bits_of_char
            buf.b_string ((index / charsize) + 1) ofset_in_byte v;
    );
)

let unsafe_write_int buffer ~index ~size v = (

    let to_write_first = charsize - (index mod charsize) in
    if size <= to_write_first then (
        (* Everything fits in one byte of the internal string *)
        (* example: to_write_first = 5 size = 3 *)
        let movmt = to_write_first - size in
        let mask = (* ex: 00011100 *) ((1 lsl size) - 1) lsl movmt in
        let the_char = (* ex: ---abc00 *) (coi ((v lsl movmt) land 0xFF)) in
        (* p "mask: %x char:%x\n" mask (ioc the_char); *)
        Internal.set_char_with_mask
            buffer.b_string (index / charsize) mask the_char;
    ) else (
        (*
        1. the first truncated byte: from index to (index + (8 - (index mod 8))
        *)
        (* example: to_write_first=3, size=9 *)
        let round_trip = size - to_write_first in
        let mask_for_v = (* ex: 111000000 *)
            ((1 lsl to_write_first) - 1) lsl round_trip in
        let mask_for_char = (* ex: 00000111 *)
            (1 lsl to_write_first) - 1 in
        let the_char =
            (* ex: char 00000abc where abc are the 3 first bits of v *)
            coi ((v land mask_for_v) lsr round_trip) in
        Internal.set_char_with_mask 
            buffer.b_string (index / charsize) mask_for_char the_char;

        let written = ref to_write_first in
        (* 
        2. Take care of "complete" bytes
        *)
        while ((* remaining = *)size - !written) >= charsize do
            let movement = size - !written - charsize in
            let the_mask = 0xff lsl movement in
            let the_char = coi ((the_mask land v) lsr movement) in
            buffer.b_string.[(index + !written) / charsize] <- the_char;
            written := !written + 8;
        done;
        (*
        3. The ending bits
        *)
        let to_write = size - !written in
        if to_write > 0 then (
            (* example: to_write = 3 *)
            let mask_for_v = (* ex: 00000111 *)
                (1 lsl to_write) - 1 in
            let the_char =
                (* ex: char xyz00000 where xyz are lowest bits of v *)
                coi ((v land mask_for_v) lsl (8 - to_write)) in
            let mask_for_char = (* ex: 11100000 *)
                mask_for_v lsl (charsize - to_write) in
            Internal.set_char_with_mask
                buffer.b_string ((index + !written) / charsize)
                mask_for_char the_char;
        );
    );
)


let write_int buffer ~index ~size v = (
    let int_size = Sys.word_size - 1 in
    if not (0 <= size && size <= int_size) then (
        raise (Error `invalid_int_size);
    );
    Internal.check_bounds_exn buffer index;
    if size <> 0 then (
        Internal.check_bounds_exn buffer (index + size - 1);
    );

    if size <> 0 then (
        unsafe_write_int buffer ~index ~size v;
    );
)

let write_int64 buffer ~index ~size v = (

    if not (0 <= size && size <= 64) then (
        raise (Error `invalid_int_size);
    );
    Internal.check_bounds_exn buffer index;
    if size <> 0 then (
        Internal.check_bounds_exn buffer (index + size - 1);
    );

    (* We want to minimize Int64 arithmetic *)
    if size <= Sys.word_size - 1 then (
        if size <> 0 then (
            unsafe_write_int buffer ~index ~size (Int64.to_int v);
        );
    ) else (
        begin match Sys.word_size with
        | 32 ->
                (* size \in [32, 64] *)
                if size <= 62 then (
                    (* We cut in two parts *)
                    let first_31 =
                        Int64.shift_right
                            (Int64.logand (Int64.shift_left 0x7F_FF_FF_FFL 31) v)
                            31
                    in
                    unsafe_write_int buffer 
                        ~index ~size:(size - 31) (Int64.to_int first_31);
                    unsafe_write_int buffer
                        ~index:(index + size - 31) ~size:31 (Int64.to_int v);
                ) else (
                    (* We cut in 2 parts but before we set two bits *)
                    if size = 63 then (
                        let first_bit =
                            (Int64.logand (Int64.shift_left 1L 62) v) <> 0L in
                        set_bit buffer index first_bit;
                    ) else ( (* size = 64 *)
                        let first_bit =
                            (Int64.logand (Int64.shift_left 1L 63) v) <> 0L in
                        set_bit buffer index first_bit;
                        let second_bit =
                            (Int64.logand (Int64.shift_left 1L 62) v) <> 0L in
                        set_bit buffer (index + 1) second_bit;
                    );
                    let second_31 =
                        Int64.shift_right
                            (Int64.logand (Int64.shift_left 0x7F_FF_FF_FF_L 31) v)
                            31
                    in
                    unsafe_write_int buffer
                        ~index:(index + size - 62) ~size:31
                        (Int64.to_int second_31);
                    unsafe_write_int buffer
                        ~index:(index + size - 62 + 31) ~size:31
                        (Int64.to_int v);
                );
        | 64 ->
                (* size = 64 *)
                (* set the first bit *)
                let first_bit =
                    (Int64.logand (Int64.shift_left 1L 63) v) <> 0L in
                set_bit buffer index first_bit;
                let new_value = (* first bit will be truncated *)
                    Int64.to_int v in
                unsafe_write_int buffer
                    ~index:(index + 1) ~size:(size - 1) new_value;
        | incredible ->
                failwith
                    (Printf.sprintf "BitBuffer: Sys.word_size = %d" incredible)
        end;
    );

)

let read_byte buffer index = (
    Internal.check_bounds_exn buffer index;
    Internal.check_bounds_exn buffer (index + charsize - 1);
    let ofset_in_byte = index mod charsize in
    if ofset_in_byte = 0 then (
        buffer.b_string.[index / charsize]
    ) else (
        (* example: ofset_in_byte = 3 ,
           b_string = "...,10111011,11011101,..."
           goal ->            |-------|
        *)
        (* Get the first semi-byte *)
        let fst_mask = (* ex: mask = 00011111 *)
            (1 lsl (charsize - ofset_in_byte)) - 1 in
        let fst_semi_byte = (* ex: (10111011 & 00011111) lsl 3 = 11011000 *)
            ((ioc buffer.b_string.[index / charsize]) land fst_mask)
            lsl (ofset_in_byte) in
        (* Get the second part *)
        let snd_mask = (* ex: mask = 11100000 *)
            ((1 lsl ofset_in_byte) - 1) lsl (charsize - ofset_in_byte) in
        let snd_semi_byte = (* ex: (11011101 & 11100000) lsr 5 = 00000110 *)
            ((ioc buffer.b_string.[(index / charsize) + 1]) land snd_mask)
            lsr (charsize - ofset_in_byte) in
        coi (fst_semi_byte lor snd_semi_byte) (* 11011110, a.k.a. the goal *)
    );
)

let unsafe_read_int buffer ~index ~size = (

    let to_read_first = charsize - (index mod charsize) in
    if size <= to_read_first then (
        (* Everything fits in one byte of the internal string *)
        (* example: to_read_first = 5 size = 3 *)
        let movmt = to_read_first - size in
        let mask = (* ex: 00011100 *) ((1 lsl size) - 1) lsl movmt in
        let the_byte = ioc buffer.b_string.[(index / charsize)] in
        let v = (* ex: ---abc00 *)
            (the_byte land mask) lsr movmt in
        (* p "mask: %x char:%x\n" mask (ioc the_char); *)
        v
    ) else (
        let accumulator = ref 0 in
        let read = ref 0 in
        let (|=) r v = r := !r lor v in
        let (+=) r v = r := !r + v in
        (*
        1. the first truncated byte: from index to (index + (8 - (index mod 8))
        *)
        (* example: to_read_first=3, size=9 *)
        let mask_for_char = (* ex: 00000111 *)
            (1 lsl to_read_first) - 1 in
        let the_byte_part = (* ex: 00000abc *)
            (ioc buffer.b_string.[(index / charsize)]) land mask_for_char in
        accumulator |= (the_byte_part lsl (size - to_read_first));
        read += to_read_first;
        (* accumulator: abc00..000, read: 3 *)

        (* 
        2. Take care of "complete" bytes
        *)
        while ((* remaining = *)size - !read) >= charsize do
            let the_byte = ioc buffer.b_string.[(index + !read) / charsize] in
            let movement = size - !read - charsize in
            (* put the byte at the right place in the int: *)
            accumulator |= (the_byte lsl movement);
            read += 8;
        done;

        (*
        3. The ending bits
        *)
        let to_read = size - !read in
        if to_read > 0 then (
            (* example: to_read = 3 *)
            let mask_for_char = (* ex: 11100000 *)
                ((1 lsl to_read) - 1) lsl (charsize - to_read) in
            let the_byte_part = (* ex: abc00000 *)
                (ioc buffer.b_string.[((index + !read) / charsize)])
                land mask_for_char in
            accumulator |= the_byte_part lsr (charsize - to_read);
        );
        (!accumulator)
    )
)

let read_int buffer ~index ~size = (
    let int_size = Sys.word_size - 1 in
    if not (0 <= size && size <= int_size) then (
        raise (Error `invalid_int_size);
    );
    Internal.check_bounds_exn buffer index;
    if size <> 0 then (
        Internal.check_bounds_exn buffer (index + size - 1);
        (unsafe_read_int buffer ~index ~size)
    ) else 
        0
)

let read_int64 buffer ~index ~size = (
    let protected_of_int the_int =
        let module I = Int64 in
        let protection = 
            let nb_of_ones = Sys.word_size - 1 in
            (I.sub (I.shift_left 1L nb_of_ones) 1L)
        in
        I.logand protection (I.of_int the_int)
    in
            

    if not (0 <= size && size <= 64) then (
        raise (Error `invalid_int_size);
    );
    Internal.check_bounds_exn buffer index;
    if size <> 0 then (
        Internal.check_bounds_exn buffer (index + size - 1);
    );

    (* We want to minimize Int64 arithmetic *)
    if size <= Sys.word_size - 1 then (
        if size <> 0 then (
            (protected_of_int (unsafe_read_int buffer ~index ~size))
        ) else 
            0L
    ) else (
        let module I = Int64 in
        begin match Sys.word_size with
        | 32 ->
                let protect_of_int i =
                    (* to prevent negative integers to become ffffffff_1...L *)
                    (I.logand (I.of_int i) 0x7f_ff_ff_ffL)
                in
                (* size \in [32, 64] *)
                if size <= 62 then (
                    (* We cut in two parts *)
                    let first_at_most_31 =
                        unsafe_read_int buffer ~index ~size:(size - 31) in
                    let next_31 =
                        unsafe_read_int buffer
                            ~index:(index + size - 31) ~size:31 in
                    (*p "size: %d first_at_most_31:%x next_31:%x\n" size first_at_most_31 next_31;
                    p "logor: %Lx %Lx\n"
                        (I.shift_left (protect_of_int first_at_most_31)  31)
                        (protect_of_int next_31);*)
                    I.logor 
                        (I.shift_left (protect_of_int first_at_most_31)  31)
                        (protect_of_int next_31)
                ) else (
                    (* We cut in 2 parts and then get one or two more bits *)
                    let first_62 = 
                        let first_31 =
                            unsafe_read_int buffer
                                ~index:(index + size - 62) ~size:31 in
                        let next_31 =
                            unsafe_read_int buffer
                                ~index:(index + size - 62 + 31) ~size:31 in
                        I.logor 
                            (I.shift_left (protect_of_int first_31) 31)
                            (protect_of_int next_31)
                    in
                    if size = 63 then (
                        let first_bit =
                            if get_bit buffer index then 1L else 0L in
                        (I.logor 
                            first_62
                            (I.shift_left first_bit 62))
                    ) else ( (* size = 64 *)
                        let first_two_bits =
                            (* This of int doesn't need protection:
                               size = 2 => positive *)
                            (I.of_int (unsafe_read_int buffer ~index ~size:2))
                        in
                        (I.logor 
                            first_62
                            (I.shift_left first_two_bits 62))
                    )
                )
        | 64 ->
                (* size = 64 *)
                (* set the first bit *)
                let first_bit =
                    if get_bit buffer index then 1L else 0L in
                let next_63 =
                    unsafe_read_int buffer ~index:(index + 1) ~size:(size - 1)
                in
                (I.logor (protected_of_int next_63) (I.shift_left first_bit 63))
        | incredible ->
                failwith
                    (Printf.sprintf "BitBuffer: Sys.word_size = %d" incredible)
        end;
    );

)


let unsafe_write_buffer dst ~dst_index ~src_index ~size src = (

    let to_write_first = charsize - (dst_index mod charsize) in
    if size <= to_write_first then (
        (* Everything fits in the first partial byte of the dst buffer *)
        if to_write_first <> 0 then (
            (* example: to_write_first = 5, size = 3*)
            let mask = (* ex: 00011100 *)
                ((1 lsl size) - 1) lsl (to_write_first - size)
            in
            let to_write = (* ex: 3 first bits of src: 0bABC *)
                read_int src ~index:src_index ~size in
            let char_to_write = (* char: 000ABC00 *)
                coi (to_write lsl (to_write_first - size)) in
            Internal.set_char_with_mask
                dst.b_string (dst_index / charsize) mask char_to_write;
        );
    ) else (
        let written = ref 0 in
        let (+=) a b = a := !a + b in

        (* 1. Write the first incomplete byte (incomplete for dst) *)
        if to_write_first <> 0 then (
            (* example: to_write_first = 5 *)
            let mask =
                (* ex: 00011111 *) (1 lsl to_write_first) - 1 in
            let to_write = (* ex: 5 first bits of src *)
                read_int src ~index:src_index ~size:to_write_first in
            let char_to_write = coi to_write in
            Internal.set_char_with_mask
                dst.b_string (dst_index / charsize) mask char_to_write;

            written += to_write_first;
        );

        (* 2. Write all complete bytes *)
        while size - !written >= charsize do
            let the_byte =
                read_byte src (src_index + !written) in
            dst.b_string.[(dst_index + !written) / charsize] <- the_byte;
            written += charsize;
        done;

        (* 3. Write the last incomplete byte *)
        let to_write = size - !written in
        if to_write > 0 then (
            (* example: to_write = 3 *)
            let mask_for_char = (* ex: 11100000 *)
                ((1 lsl to_write) - 1) lsl (charsize - to_write) in
            let the_bits = (* 3 last bits of src: 0bXYZ *)
                read_int src ~index:(src_index + !written) ~size:to_write in
            let the_char_to_write = (* char: XYZ00000 *)
                coi (the_bits lsl (charsize - to_write)) in
            Internal.set_char_with_mask
                dst.b_string ((dst_index + !written) / charsize)
                mask_for_char the_char_to_write;
        );

    );
)

let write_buffer ~dst ~dst_index ~src ~src_index ~size = (
    Internal.check_bit_size size;
    Internal.check_bounds_exn dst dst_index;
    if size <> 0 then (
        Internal.check_bounds_exn dst (dst_index + size - 1);
    );
    Internal.check_bounds_exn src src_index;
    if size <> 0 then (
        Internal.check_bounds_exn src (src_index + size - 1);
    );

    if size <> 0 then (
        unsafe_write_buffer dst ~dst_index ~src_index ~size src;
    );

)

let concat ?size buffers = (
    let total_length =
        (* The 'create' function will do the 'check_bit_size' *)
        match size with
        | Some s -> s
        | None ->
                List.fold_left (fun l b -> l + b.b_bit_length) 0 buffers
    in
    let dst = create total_length in
    let rec parse_list dst_index remaining = function
        | [] -> ()
        | src :: t ->
            if src.b_bit_length = 0 then (
                parse_list (dst_index) (remaining) t;
            ) else (
                let size = min src.b_bit_length remaining in
                if size > 0 then (
                    write_buffer ~dst ~dst_index ~src ~src_index:0 ~size;
                    parse_list (dst_index + size) (remaining - size) t;
                );
            );
    in
    parse_list 0 total_length buffers;
    dst

)

let append bufa bufb = (
    let la = length bufa in
    let lb = length bufb in
    let new_la = create (la + lb) in
    write_buffer ~dst:new_la ~dst_index:0  ~src:bufa ~src_index:0  ~size:la;
    write_buffer ~dst:new_la ~dst_index:la ~src:bufb ~src_index:0  ~size:lb;
    bufa.b_bit_length <- new_la.b_bit_length;
    bufa.b_string <- new_la.b_string;
)

let sub buf ~index ~size = (
    let newbuf = create size in
    write_buffer ~dst:newbuf ~dst_index:0 ~src:buf ~src_index:index ~size; 
    newbuf
)
