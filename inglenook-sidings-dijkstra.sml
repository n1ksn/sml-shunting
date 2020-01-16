(*-------------------------------------------------------------------------
  inglenook-sidings-dijkstra.sml

  Find optimal solutions to a classic shunting (switching) puzzle using
  Standard ML and Dijkstra's algorithm.

  This program is intended to be run after compilation with mlton.  To do
  so, uncomment the statement "main();"" at the end of this file and run

        mlton -output <executable name> inglenook-sidings-dijkstra.sml

  in a terminal.  On a 3 GHz laptop the program run time was about 9 hours.

  The program will write two files, one with all 40320 standard starting
  conditions together with the number of moves in a minimal solution.  The
  other file contains the data from the table used with Dijkstra's
  algorithm (except for the "visited" column).  That file can be read by
  the program in inglenook-sidings-full-opt.sml which is used to print
  solution paths from any puzzle state to the standard finish state and to
  generate random standard puzzle problems and optimal solutions for them.

  Andrew Palm
  2020-01-12

 ------------------------------------------------------------------------*)
(*------------------------------------------------------------------------*)
(* Utilities *)
fun member (x, []) = false
  | member (x, h::t) = x = h orelse member (x, t);

fun remove_if (f, []) = []
  | remove_if (f, h::t) =
      if f h then remove_if(f, t)
      else h::remove_if(f, t);

(*------------------------------------------------------------------------*)
(* Functions to convert between the "external" puzzle state used for
   I/O and the internal puzzle state used for calculations.

   The layout of a 64-bit word internal puzzle state is as follows:

         Nybble         Track         Position from points
          0-2             3                  3,2,1
          3-5             2                  3,2,1
          6-10            1                  5,4,3,2,1
          11-13           0                  1,2,3

   Cars are represented by 4-bit hex values 1 to 8.  Empty track positions
   are zeros.  Cars are right-justified in their tracks.
*)
(* Internal state is 64-bit *)
type state = Word64.word;
(* Bit shift values for tracks in internal state *)
val w3shift = Word.fromInt(0);
val w2shift = Word.fromInt(12);
val w1shift = Word.fromInt(24);
val w0shift = Word.fromInt(44);
(* Bit masks for tracks *)
val w3mask = Option.valOf(Word64.fromString("0wxFFF"));
val w2mask = Option.valOf(Word64.fromString("0wxFFF000"));
val w1mask = Option.valOf(Word64.fromString("0wxFFFFF000000"));
val w0mask = Option.valOf(Word64.fromString("0wxFFF00000000000"));
(* Bit masks for positions on each track *)
val pos3w0mask = Option.valOf(Word64.fromString("0wxF0000000000000"));
val pos2w0mask = Option.valOf(Word64.fromString("0wx0F000000000000"));
val pos1w0mask = Option.valOf(Word64.fromString("0wx00F00000000000"));
val pos5w1mask = Option.valOf(Word64.fromString("0wx0000000F000000"));
val pos4w1mask = Option.valOf(Word64.fromString("0wx000000F0000000"));
val pos3w1mask = Option.valOf(Word64.fromString("0wx00000F00000000"));
val pos2w1mask = Option.valOf(Word64.fromString("0wx0000F000000000"));
val pos1w1mask = Option.valOf(Word64.fromString("0wx000F0000000000"));
val pos3w2mask = Option.valOf(Word64.fromString("0wx0000000000F000"));
val pos2w2mask = Option.valOf(Word64.fromString("0wx000000000F0000"));
val pos1w2mask = Option.valOf(Word64.fromString("0wx00000000F00000"));
val pos3w3mask = Option.valOf(Word64.fromString("0wx0000000000000F"));
val pos2w3mask = Option.valOf(Word64.fromString("0wx000000000000F0"));
val pos1w3mask = Option.valOf(Word64.fromString("0wx00000000000F00"));
(* Bit masks for 1, 2, or 3 cars on a track (must be shifted) *)
val mask1car = Option.valOf(Word64.fromString("0wxF"));
val mask2cars = Option.valOf(Word64.fromString("0wxFF"));
val mask3cars = Option.valOf(Word64.fromString("0wxFFF"));
(* Zero word used for comparisons *)
val zeroword = Option.valOf(Word64.fromString("0wx0"));

(* The external state is a 4-tuple of integer lists, one for each track
        (Track 0 list, Track 1 list, Track 2 list, Track 3 list)
   This data type is used for input and output of start and finish states.
*)
type iostate = int list * int list * int list * int list;

(* Functions to convert a track list to its internal state representation *)
fun t3_to_word (t3:int list):Word64.word =
  let
    val n = 3 - List.length(t3);
    val expt3 = List.take([0, 0, 0], n) @ t3;
    val t31 = List.nth(expt3, 0);
    val t32 = List.nth(expt3, 1);
    val t33 = List.nth(expt3, 2)
  in
    Word64.fromInt(16*(16*t31 + t32) + t33)
  end;

fun t2_to_word (t2:int list):Word64.word =
  let
    val n = 3 - List.length(t2);
    val expt2 = List.take([0, 0, 0], n) @ t2;
    val t21 = List.nth(expt2, 0);
    val t22 = List.nth(expt2, 1);
    val t23 = List.nth(expt2, 2)
  in
    Word64.fromInt(16*(16*t21 + t22) + t23)
  end;

fun t1_to_word (t1:int list):Word64.word =
  let
    val n = 5 - List.length(t1);
    val expt1 = List.take([0, 0, 0, 0, 0], n) @ t1;
    val t11 = List.nth(expt1, 0);
    val t12 = List.nth(expt1, 1);
    val t13 = List.nth(expt1, 2);
    val t14 = List.nth(expt1, 3);
    val t15 = List.nth(expt1, 4);
  in
    Word64.fromInt(16*(16*(16*(16*t11 + t12) + t13) + t14) + t15)
  end;

fun t0_to_word (t0:int list):Word64.word =
  let
    val n = 4 - List.length(t0);
    val expt0 = List.take([0, 0, 0], n) @ tl(t0);
    val t01 = List.nth(expt0, 0);
    val t02 = List.nth(expt0, 1);
    val t03 = List.nth(expt0, 2)
  in
    Word64.fromInt(16*(16*t01 + t02) + t03)
  end;

(* Functions to convert between I/O states and internal states *)
fun iostate_to_state (ios:iostate):state =
  let
    val (t0, t1, t2, t3) = ios;
    val w0 = t0_to_word(t0);
    val w1 = t1_to_word(t1);
    val w2 = t2_to_word(t2);
    val w3 = t3_to_word(t3)
  in
    Word64.<<(w0, w0shift) + Word64.<<(w1, w1shift)
         + Word64.<<(w2, w2shift) + w3
  end;

local
  fun wtil1 (w:Word64.word, acc:int list) =
    let
      val mask = Word64.fromInt(15);
      val nyb = Word64.andb(w, mask);
      val i = Word64.toInt(nyb);
      val nextw = Word64.>>(w, Word.fromInt(4))
    in
      if i = 0 then
        acc
      else
        wtil1(nextw, i::acc)
    end;
in
  fun word_to_int_list (w:Word64.word):int list =
    wtil1(w, []);
end;

fun state_to_iostate (s:state):iostate =
  let
    val w3 = Word64.andb(s, w3mask);
    val t3 = word_to_int_list(w3);
    val w2 = Word64.>>(Word64.andb(s, w2mask), w2shift);
    val t2 = word_to_int_list(w2);
    val w1 = Word64.>>(Word64.andb(s, w1mask), w1shift);
    val t1 = word_to_int_list(w1);
    val w0 = Word64.>>(Word64.andb(s, w0mask), w0shift);
    val t0 = word_to_int_list(w0);
  in
    (0::t0, t1, t2, t3)   (* Add 0 back for engine *)
  end;


(*------------------------------------------------------------------------*)
(* Functions to find the number of cars on a track (in a state) *)
fun ncars_w0 (s:state):int =
  let
    val pos1w0 = Word64.andb(s, pos1w0mask);
    val pos1w0empty = (Word64.compare(pos1w0, zeroword) = EQUAL)
    val pos2w0 = Word64.andb(s, pos2w0mask);
    val pos2w0empty = (Word64.compare(pos2w0, zeroword) = EQUAL)
    val pos3w0 = Word64.andb(s, pos3w0mask);
    val pos3w0empty = (Word64.compare(pos3w0, zeroword) = EQUAL)
  in
    if pos1w0empty then 0 else
      (if pos2w0empty then 1 else
        (if pos3w0empty then 2 else 3))
  end;

fun ncars_w1 (s:state):int =
  let
    val pos1w1 = Word64.andb(s, pos1w1mask);
    val pos1w1empty = (Word64.compare(pos1w1, zeroword) = EQUAL)
    val pos2w1 = Word64.andb(s, pos2w1mask);
    val pos2w1empty = (Word64.compare(pos2w1, zeroword) = EQUAL)
    val pos3w1 = Word64.andb(s, pos3w1mask);
    val pos3w1empty = (Word64.compare(pos3w1, zeroword) = EQUAL)
    val pos4w1 = Word64.andb(s, pos4w1mask);
    val pos4w1empty = (Word64.compare(pos4w1, zeroword) = EQUAL)
    val pos5w1 = Word64.andb(s, pos5w1mask);
    val pos5w1empty = (Word64.compare(pos5w1, zeroword) = EQUAL)
  in
    if pos5w1empty then 0 else
      (if pos4w1empty then 1 else
        (if pos3w1empty then 2 else
            (if pos2w1empty then 3 else
              (if pos1w1empty then 4 else 5))))
  end;

fun ncars_w2 (s:state):int =
  let
    val pos1w2 = Word64.andb(s, pos1w2mask);
    val pos1w2empty = (Word64.compare(pos1w2, zeroword) = EQUAL)
    val pos2w2 = Word64.andb(s, pos2w2mask);
    val pos2w2empty = (Word64.compare(pos2w2, zeroword) = EQUAL)
    val pos3w2 = Word64.andb(s, pos3w2mask);
    val pos3w2empty = (Word64.compare(pos3w2, zeroword) = EQUAL)
  in
    if pos3w2empty then 0 else
      (if pos2w2empty then 1 else
        (if pos1w2empty then 2 else 3))
  end;

fun ncars_w3 (s:state):int =
  let
    val pos1w3 = Word64.andb(s, pos1w3mask);
    val pos1w3empty = (Word64.compare(pos1w3, zeroword) = EQUAL)
    val pos2w3 = Word64.andb(s, pos2w3mask);
    val pos2w3empty = (Word64.compare(pos2w3, zeroword) = EQUAL)
    val pos3w3 = Word64.andb(s, pos3w3mask);
    val pos3w3empty = (Word64.compare(pos3w3, zeroword) = EQUAL)
  in
    if pos3w3empty then 0 else
      (if pos2w3empty then 1 else
        (if pos1w3empty then 2 else 3))
  end;

(*------------------------------------------------------------------------*)
(* Functions to calculate successor states based on legal moves *)
fun p1t1 (s:state):state =
  (* Pull one car from track 1 *)
  let
    val n0 = ncars_w0(s);
    val n1 = ncars_w1(s);
  in
    if (n0 < 3) andalso (n1 > 0) then
      let
        (* Get bit pattern of head car on track 1 *)
        val hd_w1_base = Word.fromInt(4*(n1-1)) + w1shift;
        val hd_w1_mask = Word64.<<(mask1car, hd_w1_base);
        val hd_w1_bits = Word64.andb(s, hd_w1_mask);
        (* Get moved car pattern *)
        val hd_w1_shift = w0shift - hd_w1_base;
        val moved_car = Word64.<<(hd_w1_bits, hd_w1_shift);
        (* Get pattern of new track 0 with moved car added *)
        val w0_bits = Word64.andb(s, w0mask);
        val new_w0 = moved_car + Word64.<<(w0_bits, Word.fromInt(4));
        (* Clear bits of old track 0 and head car of track 1 *)
        val clr_mask = Word64.notb(w0mask + hd_w1_mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 *)
        s1 + new_w0
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun p1t2 (s:state):state =
  (* Pull one car from track 2 *)
  let
    val n0 = ncars_w0(s);
    val n2 = ncars_w2(s);
  in
    if (n0 < 3) andalso (n2 > 0) then
      let
        (* Get bit pattern of head car on track 2 *)
        val hd_w2_base = Word.fromInt(4*(n2-1)) + w2shift;
        val hd_w2_mask = Word64.<<(mask1car, hd_w2_base);
        val hd_w2_bits = Word64.andb(s, hd_w2_mask);
        (* Get moved car pattern *)
        val hd_w2_shift = w0shift - hd_w2_base;
        val moved_car = Word64.<<(hd_w2_bits, hd_w2_shift);
        (* Get pattern of new track 0 with moved car added *)
        val w0_bits = Word64.andb(s, w0mask);
        val new_w0 = moved_car + Word64.<<(w0_bits, Word.fromInt(4));
        (* Clear bits of old track 0 and head car of track 2 *)
        val clr_mask = Word64.notb(w0mask + hd_w2_mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 *)
        s1 + new_w0
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun p1t3 (s:state):state =
  (* Pull one car from track 3 *)
  let
    val n0 = ncars_w0(s);
    val n3 = ncars_w3(s);
  in
    if (n0 < 3) andalso (n3 > 0) then
      let
        (* Get bit pattern of head car on track 3 *)
        val hd_w3_base = Word.fromInt(4*(n3-1)) + w3shift;
        val hd_w3_mask = Word64.<<(mask1car, hd_w3_base);
        val hd_w3_bits = Word64.andb(s, hd_w3_mask);
        (* Get moved car pattern *)
        val hd_w3_shift = w0shift - hd_w3_base;
        val moved_car = Word64.<<(hd_w3_bits, hd_w3_shift);
        (* Get pattern of new track 0 with moved car added *)
        val w0_bits = Word64.andb(s, w0mask);
        val new_w0 = moved_car + Word64.<<(w0_bits, Word.fromInt(4));
        (* Clear bits of old track 0 and head car of track 3 *)
        val clr_mask = Word64.notb(w0mask + hd_w3_mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 *)
        s1 + new_w0
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun d1t1 (s:state):state =
  (* Drop one car to track 1 *)
  let
    val n0 = ncars_w0(s);
    val n1 = ncars_w1(s);
  in
    if (n0 > 0) andalso (n1 < 5) then
      let
        (* Get bit pattern of head car on track 0 *)
        val hd_w0_mask = Word64.<<(mask1car, w0shift);
        val hd_w0_bits = Word64.andb(s, hd_w0_mask);
        (* Get moved car pattern *)
        val hd_w1_base = Word.fromInt(4*n1) + w1shift;
        val hd_w0_shift = w0shift - hd_w1_base;
        val moved_car = Word64.>>(hd_w0_bits, hd_w0_shift);
        (* Get pattern of new track 0 with car removed *)
        val w0_bits = Word64.andb(s, pos2w0mask + pos3w0mask);
        val new_w0 = Word64.>>(w0_bits, Word.fromInt(4));
        (* Clear bits of old track 0 *)
        val clr_mask = Word64.notb(w0mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 and moved car *)
        s1 + new_w0 + moved_car
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun d1t2 (s:state):state =
  (* Drop one car to track 2 *)
  let
    val n0 = ncars_w0(s);
    val n2 = ncars_w2(s);
  in
    if (n0 > 0) andalso (n2 < 3) then
      let
        (* Get bit pattern of head car on track 0 *)
        val hd_w0_mask = Word64.<<(mask1car, w0shift);
        val hd_w0_bits = Word64.andb(s, hd_w0_mask);
        (* Get moved car pattern *)
        val hd_w2_base = Word.fromInt(4*n2) + w2shift;
        val hd_w0_shift = w0shift - hd_w2_base;
        val moved_car = Word64.>>(hd_w0_bits, hd_w0_shift);
        (* Get pattern of new track 0 with car removed *)
        val w0_bits = Word64.andb(s, pos2w0mask + pos3w0mask);
        val new_w0 = Word64.>>(w0_bits, Word.fromInt(4));
        (* Clear bits of old track 0 *)
        val clr_mask = Word64.notb(w0mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 and moved car *)
        s1 + new_w0 + moved_car
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun d1t3 (s:state):state =
  (* Drop one car to track 3 *)
  let
    val n0 = ncars_w0(s);
    val n3 = ncars_w3(s);
  in
    if (n0 > 0) andalso (n3 < 3) then
      let
        (* Get bit pattern of head car on track 0 *)
        val hd_w0_mask = Word64.<<(mask1car, w0shift);
        val hd_w0_bits = Word64.andb(s, hd_w0_mask);
        (* Get moved car pattern *)
        val hd_w3_base = Word.fromInt(4*n3) + w3shift;
        val hd_w0_shift = w0shift - hd_w3_base;
        val moved_car = Word64.>>(hd_w0_bits, hd_w0_shift);
        (* Get pattern of new track 0 with car removed *)
        val w0_bits = Word64.andb(s, pos2w0mask + pos3w0mask);
        val new_w0 = Word64.>>(w0_bits, Word.fromInt(4));
        (* Clear bits of old track 0 *)
        val clr_mask = Word64.notb(w0mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 and moved car *)
        s1 + new_w0 + moved_car
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun p2t1 (s:state):state =
  (* Pull two cars from track 1 *)
  let
    val n0 = ncars_w0(s);
    val n1 = ncars_w1(s);
  in
    if (n0 < 2) andalso (n1 > 1) then
      let
        (* Get bit pattern of 2 cars on head of track 1 *)
        val hd_w1_base = Word.fromInt(4*(n1-2)) + w1shift;
        val hd_w1_mask = Word64.<<(mask2cars, hd_w1_base);
        val hd_w1_bits = Word64.andb(s, hd_w1_mask);
        (* Get moved cars pattern *)
        val hd_w1_shift = w0shift - hd_w1_base;
        val moved_cars = Word64.<<(hd_w1_bits, hd_w1_shift);
        (* Get pattern of new track 0 with moved cars added *)
        val w0_bits = Word64.andb(s, w0mask);
        val new_w0 = moved_cars + Word64.<<(w0_bits, Word.fromInt(8));
        (* Clear bits of old track 0 and head 2 cars of track 1 *)
        val clr_mask = Word64.notb(w0mask + hd_w1_mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 *)
        s1 + new_w0
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun p2t2 (s:state):state =
  (* Pull two cars from track 2 *)
  let
    val n0 = ncars_w0(s);
    val n2 = ncars_w2(s);
  in
    if (n0 < 2) andalso (n2 > 1) then
      let
        (* Get bit pattern of 2 cars on head of track 2 *)
        val hd_w2_base = Word.fromInt(4*(n2-2)) + w2shift;
        val hd_w2_mask = Word64.<<(mask2cars, hd_w2_base);
        val hd_w2_bits = Word64.andb(s, hd_w2_mask);
        (* Get moved cars pattern *)
        val hd_w2_shift = w0shift - hd_w2_base;
        val moved_cars = Word64.<<(hd_w2_bits, hd_w2_shift);
        (* Get pattern of new track 0 with moved cars added *)
        val w0_bits = Word64.andb(s, w0mask);
        val new_w0 = moved_cars + Word64.<<(w0_bits, Word.fromInt(8));
        (* Clear bits of old track 0 and head 2 cars of track 2 *)
        val clr_mask = Word64.notb(w0mask + hd_w2_mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 *)
        s1 + new_w0
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun p2t3 (s:state):state =
  (* Pull two cars from track 3 *)
  let
    val n0 = ncars_w0(s);
    val n3 = ncars_w3(s);
  in
    if (n0 < 2) andalso (n3 > 1) then
      let
        (* Get bit pattern of 2 cars on head of track 2 *)
        val hd_w3_base = Word.fromInt(4*(n3-2)) + w3shift;
        val hd_w3_mask = Word64.<<(mask2cars, hd_w3_base);
        val hd_w3_bits = Word64.andb(s, hd_w3_mask);
        (* Get moved cars pattern *)
        val hd_w3_shift = w0shift - hd_w3_base;
        val moved_cars = Word64.<<(hd_w3_bits, hd_w3_shift);
        (* Get pattern of new track 0 with moved cars added *)
        val w0_bits = Word64.andb(s, w0mask);
        val new_w0 = moved_cars + Word64.<<(w0_bits, Word.fromInt(8));
        (* Clear bits of old track 0 and head 2 cars of track 2 *)
        val clr_mask = Word64.notb(w0mask + hd_w3_mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 *)
        s1 + new_w0
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun d2t1 (s:state):state =
  (* Drop two cars to track 1 *)
  let
    val n0 = ncars_w0(s);
    val n1 = ncars_w1(s);
  in
    if (n0 > 1) andalso (n1 < 4) then
      let
        (* Get bit pattern of head 2 cars on track 0 *)
        val hd_w0_mask = Word64.<<(mask2cars, w0shift);
        val hd_w0_bits = Word64.andb(s, hd_w0_mask);
        (* Get moved cars pattern *)
        val hd_w1_base = Word.fromInt(4*n1) + w1shift;
        val hd_w0_shift = w0shift - hd_w1_base;
        val moved_cars = Word64.>>(hd_w0_bits, hd_w0_shift);
        (* Get pattern of new track 0 with 2 cars removed *)
        val w0_bits = Word64.andb(s, pos3w0mask);
        val new_w0 = Word64.>>(w0_bits, Word.fromInt(8));
        (* Clear bits of old track 0 *)
        val clr_mask = Word64.notb(w0mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 and moved cars *)
        s1 + new_w0 + moved_cars
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun d2t2 (s:state):state =
  (* Drop two cars to track 2 *)
  let
    val n0 = ncars_w0(s);
    val n2 = ncars_w2(s);
  in
    if (n0 > 1) andalso (n2 < 2) then
      let
        (* Get bit pattern of head 2 cars on track 0 *)
        val hd_w0_mask = Word64.<<(mask2cars, w0shift);
        val hd_w0_bits = Word64.andb(s, hd_w0_mask);
        (* Get moved cars pattern *)
        val hd_w2_base = Word.fromInt(4*n2) + w2shift;
        val hd_w0_shift = w0shift - hd_w2_base;
        val moved_cars = Word64.>>(hd_w0_bits, hd_w0_shift);
        (* Get pattern of new track 0 with 2 cars removed *)
        val w0_bits = Word64.andb(s, pos3w0mask);
        val new_w0 = Word64.>>(w0_bits, Word.fromInt(8));
        (* Clear bits of old track 0 *)
        val clr_mask = Word64.notb(w0mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 and moved cars *)
        s1 + new_w0 + moved_cars
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun d2t3 (s:state):state =
  (* Drop two cars to track 3 *)
  let
    val n0 = ncars_w0(s);
    val n3 = ncars_w3(s);
  in
    if (n0 > 1) andalso (n3 < 2) then
      let
        (* Get bit pattern of head 2 cars on track 0 *)
        val hd_w0_mask = Word64.<<(mask2cars, w0shift);
        val hd_w0_bits = Word64.andb(s, hd_w0_mask);
        (* Get moved cars pattern *)
        val hd_w3_base = Word.fromInt(4*n3) + w3shift;
        val hd_w0_shift = w0shift - hd_w3_base;
        val moved_cars = Word64.>>(hd_w0_bits, hd_w0_shift);
        (* Get pattern of new track 0 with 2 cars removed *)
        val w0_bits = Word64.andb(s, pos3w0mask);
        val new_w0 = Word64.>>(w0_bits, Word.fromInt(8));
        (* Clear bits of old track 0 *)
        val clr_mask = Word64.notb(w0mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 and moved cars *)
        s1 + new_w0 + moved_cars
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun p3t1 (s:state):state =
  (* Pull three cars from track 1 *)
  let
    val n0 = ncars_w0(s);
    val n1 = ncars_w1(s);
  in
    if (n0 = 0) andalso (n1 > 2) then
      let
        (* Get bit pattern of 3 cars on head of track 1 *)
        val hd_w1_base = Word.fromInt(4*(n1-3)) + w1shift;
        val hd_w1_mask = Word64.<<(mask3cars, hd_w1_base);
        val hd_w1_bits = Word64.andb(s, hd_w1_mask);
        (* Get moved cars pattern *)
        val hd_w1_shift = w0shift - hd_w1_base;
        val moved_cars = Word64.<<(hd_w1_bits, hd_w1_shift);
        (* Clear bits of old track 0 and head 3 cars of track 1 *)
        val clr_mask = Word64.notb(w0mask + hd_w1_mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 *)
        s1 + moved_cars
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun p3t2 (s:state):state =
  (* Pull three cars from track 2 *)
  let
    val n0 = ncars_w0(s);
    val n2 = ncars_w2(s);
  in
    if (n0 = 0) andalso (n2 = 3) then
      let
        (* Get bit pattern of 3 cars on track 2 *)
        val hd_w2_bits = Word64.andb(s, w2mask);
        (* Get moved cars pattern *)
        val hd_w2_shift = w0shift - w2shift;
        val moved_cars = Word64.<<(hd_w2_bits, hd_w2_shift);
        (* Clear bits of old track 0 and track 2 *)
        val clr_mask = Word64.notb(w0mask + w2mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 *)
        s1 + moved_cars
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun p3t3 (s:state):state =
  (* Pull three cars from track 3 *)
  let
    val n0 = ncars_w0(s);
    val n3 = ncars_w3(s);
  in
    if (n0 = 0) andalso (n3 = 3) then
      let
        (* Get bit pattern of 3 cars on track 3 *)
        val hd_w3_bits = Word64.andb(s, w3mask);
        (* Get moved cars pattern *)
        val hd_w3_shift = w0shift - w3shift;
        val moved_cars = Word64.<<(hd_w3_bits, hd_w3_shift);
        (* Clear bits of old track 0 and track 3 *)
        val clr_mask = Word64.notb(w0mask + w3mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 *)
        s1 + moved_cars
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun d3t1 (s:state):state =
  (* Drop three cars to track 1 *)
  let
    val n0 = ncars_w0(s);
    val n1 = ncars_w1(s);
  in
    if (n0 = 3) andalso (n1 < 3) then
      let
        (* Get bit pattern of 3 cars on track 0 *)
        val hd_w0_bits = Word64.andb(s, w0mask);
        (* Get moved cars pattern *)
        val hd_w1_base = Word.fromInt(4*n1) + w1shift;
        val hd_w0_shift = w0shift - hd_w1_base;
        val moved_cars = Word64.>>(hd_w0_bits, hd_w0_shift);
        (* Clear bits of old track 0 *)
        val clr_mask = Word64.notb(w0mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 and moved cars *)
        s1 + moved_cars
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun d3t2 (s:state):state =
  (* Drop three cars to track 2 *)
  let
    val n0 = ncars_w0(s);
    val n2 = ncars_w2(s);
  in
    if (n0 = 3) andalso (n2 = 0) then
      let
        (* Get bit pattern of 3 cars on track 0 *)
        val hd_w0_bits = Word64.andb(s, w0mask);
        (* Get moved cars pattern *)
        val hd_w0_shift = w0shift - w2shift;
        val moved_cars = Word64.>>(hd_w0_bits, hd_w0_shift);
        (* Clear bits of old track 0 *)
        val clr_mask = Word64.notb(w0mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 and moved cars *)
        s1 + moved_cars
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun d3t3 (s:state):state =
  (* Drop three cars to track 3 *)
  let
    val n0 = ncars_w0(s);
    val n3 = ncars_w3(s);
  in
    if (n0 = 3) andalso (n3 = 0) then
      let
        (* Get bit pattern of 3 cars on track 0 *)
        val hd_w0_bits = Word64.andb(s, w0mask);
        (* Get moved cars pattern *)
        val hd_w0_shift = w0shift - w3shift;
        val moved_cars = Word64.>>(hd_w0_bits, hd_w0_shift);
        (* Clear bits of old track 0 *)
        val clr_mask = Word64.notb(w0mask);
        val s1 = Word64.andb(s, clr_mask)
      in
        (* Add in new bits for track 0 and moved cars *)
        s1 + moved_cars
      end
    else
      (* If move can't be made, return zero state *)
      zeroword
  end;

fun next_states state =
  (* Return a list of sucessors to state, that is states that can be
     reached from the given state by a legal move *)
  remove_if((fn x => (x=zeroword)),
              [p3t1 state, p3t2 state, p3t3 state,
               d3t1 state, d3t2 state, d3t3 state,
               p2t1 state, p2t2 state, p2t3 state,
               d2t1 state, d2t2 state, d2t3 state,
               p1t1 state, p1t2 state, p1t3 state,
               d1t1 state, d1t2 state, d1t3 state]);

(*------------------------------------------------------------------------*)
(* Functions for printing a state and puzzle solution (a list of states) *)
local
  fun ilts1 (l:int list, acc:string) =
    if (l = []) then
    (
      if acc = "[" then "[]" (* l is nil on first call *)
      else String.concat [acc, "]"] (* l is nil on last recursive call *)
    )
    else
    (
      if acc = "[" then
        ilts1(tl(l), (String.concat [acc, Int.toString(hd(l))]))
      else
        ilts1(tl(l), (String.concat [acc, ",", Int.toString(hd(l))]))
    );
in
  fun int_list_to_string (l:int list):string = ilts1(l, "[");
end;

fun print_int_list (l:int list) =
  print (int_list_to_string l);

fun print_iostate ((t0, t1, t2, t3):iostate) =
  (
    print "(";
    print_int_list(t0); print ",";
    print_int_list(t1); print ",";
    print_int_list(t2); print ",";
    print_int_list(t3);
    print ")\n"
  );

fun print_iostate_list (ios:iostate list) =
  app (print_iostate) ios;

fun print_solution (solution:iostate list) =
  (
    print "Moves: "; print (Int.toString((length solution)-1));
    print "\nSolution:\n"; print_iostate_list solution;
    print "\n"
  );

(*------------------------------------------------------------------------*)
(* Functions for writing solutions to a file *)
fun print_int_list_file (out_str, l:int list) =
  TextIO.output (out_str, int_list_to_string l);

fun print_iostate_file (out_str, (t0, t1, t2, t3):iostate) =
  (
    TextIO.output(out_str, "(");
    print_int_list_file(out_str, t0); TextIO.output(out_str, ",");
    print_int_list_file(out_str, t1); TextIO.output(out_str, ",");
    print_int_list_file(out_str, t2); TextIO.output(out_str, ",");
    print_int_list_file(out_str, t3);
    TextIO.output(out_str, ")")
  );

fun print_iostate_list_file (out_str, []) = ()
  | print_iostate_list_file (out_str, ios:iostate list) =
    (
      print_iostate_file (out_str, hd(ios));
      TextIO.output(out_str, "\n");
      print_iostate_list_file (out_str, tl(ios))
    );

fun print_solution_file (out_str, solution:iostate list) =
  (
    TextIO.output(out_str, "\nMoves: ");
    TextIO.output (out_str, Int.toString((length solution)-1));
    TextIO.output(out_str, "\nSolution:\n");
    print_iostate_list_file(out_str, solution);
    TextIO.output (out_str, "\n")
  );

fun print_moves_file (out_str, solution:iostate list) =
  (
    TextIO.output(out_str, " : ");
    TextIO.output (out_str, Int.toString((length solution)-1));
    TextIO.output (out_str, "\n")
  );

(*------------------------------------------------------------------------*)
(* Functions to encode and decode puzzle states *)
fun perm_to_Lehmer (p : int list) : int list =
  (* Convert permutation of positive integers 1 to n to its Lehmer code *)
  let
    val m = ref (map (fn x => x-1) p);
    val n = length(p);
    val k = ref 0;
  in
    while (!k < n) do
    (
      let val x = List.nth(!m, !k) in
        m := List.take(!m, !k+1) @
           (map (fn y => if y >= x then y-1 else y) (List.drop(!m, !k+1)))
      end;
      k := !k + 1
    );
    !m
  end;

fun Lehmer_to_perm (m : int list) : int list =
  (* Convert Lehmer code to permutation of positive integers 1 to n *)
  let
    val p = ref m;
    val k = ref (length(m)-1);
  in
    while (!k >= 0) do
    (
      let val x = List.nth(!p, !k) in
        p := List.take(!p, !k+1) @
           (map (fn y => if y >= x then y+1 else y) (List.drop(!p, !k+1)))
      end;
      k := !k - 1
    );
    map (fn x => x+1) (!p)
  end;

val perm8_coef = [5040, 720, 120, 24, 6, 2, 1, 0];

fun perm8_to_index (p) =
  (* Convert permutation of integers 1 to 8 to its Sym(8) index *)
  ListPair.foldlEq (fn (a, b, c) => a*b + c) 0
      (perm8_coef, perm_to_Lehmer(p));

fun index_to_perm8 (index: int) =
  (* Convert Sym(8) index to permutations of integers 1 to 8 *)
  let
    val m1 = index div 5040;
    val m2 = (index mod 5040) div 720;
    val m3 = (index mod 720) div 120;
    val m4 = (index mod 120) div 24;
    val m5 = (index mod 24) div 6;
    val m6 = (index mod 6) div 2;
    val m7 = index mod 2
  in
    Lehmer_to_perm([m1, m2, m3, m4, m5, m6, m7, 0])
  end;

fun iostate_to_array_index ((t0, t1, t2, t3) : iostate) : int =
  (* Convert puzzle I/O state 4-tuple to array index *)
  let
    val p = ((tl t0)) @ t1 @ t2 @ t3;   (* Permutation as list *)
    val pind = perm8_to_index(p);
    val n0 = length((tl t0));
    val n2 = length(t2);
    val n3 = length(t3)
  in
    n0 + 4*n2 + 16*n3 + 64*pind
  end;

fun array_index_to_iostate (array_index : int) : iostate =
  (* Convert array index to puzzle I/O state 4-tuple *)
  let
    val occupy_index = array_index mod 64;
    val n3 = occupy_index div 16;
    val n2 = (occupy_index mod 16) div 4;
    val n0 = occupy_index mod 4;
    val n1 = 8 - n0 - n2 - n3;
    val pind = array_index div 64;    (* Sym(8) index of permutation *)
    val p = index_to_perm8(pind)      (* Permutation as list *)
  in
    (0::List.take(p, n0),
     List.take(List.drop(p, n0), n1),
     List.take(List.drop(p, n0 + n1), n2),
     List.take(List.drop(p, n0 + n1 + n2), n3))
  end;

fun state_to_array_index (s : state) : int =
  (* Convert internal state rep to array index *)
  iostate_to_array_index(state_to_iostate(s));

fun array_index_to_state (array_index : int) : state =
  (* Convert array index into internal state *)
  iostate_to_state(array_index_to_iostate(array_index));

(*------------------------------------------------------------------------*)
(* Data structures for Dijkstra's algorithm. *)
(* Maximum number of array elements, including non-legits *)
val num_array_elts = 3 + 4*3 + 16*3 + 64*(40320 - 1);
(* Array for internal state rep of each node *)
val states = Array.array(num_array_elts, Word64.fromInt(0));
(* Array for distance to root node (standard final puzzle state)
   initialized to large value *)
val dist_to_root = IntArray.array(num_array_elts, 1000);
(* Array for array_index of previous state for a state  *)
val prev_state = IntArray.array(num_array_elts, 0);
(* Array for marking state as visited *)
val visited = BoolArray.array(num_array_elts, false);

(* Array index and state of root node *)
val root_array_index = iostate_to_array_index(([0],[1,2,3,4,5],[6,7,8],[]));
val root_state = iostate_to_state(([0],[1,2,3,4,5],[6,7,8],[]));

(*------------------------------------------------------------------------*)
(* Helper functions for Dijkstra's algorithm *)
fun valid_array_index (array_index : int) : bool =
  (* Determine if array index corresponds to an actual puzzle state *)
  let
    val occupy_index = array_index mod 64;
    val n3 = occupy_index div 16;
    val n2 = (occupy_index mod 16) div 4;
    val n0 = occupy_index mod 4;
    val ntot = n0 + n2 + n3
  in
    (ntot >= 3) andalso (ntot <= 8)
  end;

fun init_states () =
  (* Fill in states array with valid internal states and set invalid
     states to zero word.  Called during initialization. *)
  let
    fun vai (i, x) =
      if valid_array_index(i) then array_index_to_state(i)
                              else zeroword;
  in
    Array.modifyi vai states
  end;

fun init_visited () =
  (* Set all invalid nodes to visited = true so they are masked when
     determining which nodes remain unvisited *)
  let
    fun vai (i, x) = if valid_array_index(i) then false else true;
  in
    BoolArray.modifyi vai visited
  end;

fun init_dist_to_root () =
    IntArray.modify (fn x => 1000) dist_to_root;

fun init_prev_state () =
    IntArray.modify (fn x => 0) prev_state;

(* The following functions assume that the states array has be initialized *)
fun neighbors (array_index : int) : int list =
  (* Find neighbors of state associated with array_index.  Returns
     list of array indices of neighbors *)
  let
    val next_state_list = next_states(Array.sub(states, array_index));
  in
    map state_to_array_index next_state_list
  end;

fun unvisited_neighbors (array_index : int) : int list =
  (* Find unvisited neighbors of state associated with array_index.
     Returns list of neighbor array indices *)
  let
    fun unvisited (a_i : int) : bool =
      (BoolArray.sub(visited, a_i) = false);
  in
    List.filter unvisited (neighbors(array_index))
  end;

fun all_nodes_visited () : bool =
  (* Return true when all valid nodes have been visited *)
  BoolArray.foldl (fn (a, b) => a andalso b) true visited;

fun get_min_unvisited() : int*int =
  (* Get unvisited node with minimum distance from root *)
  let
    val curr_min = ref (2000, 0);  (* (min, array index of min) *)
    fun unvisited_min(ai, x) =
      if (not(BoolArray.sub(visited, ai)) andalso x < (#1 (!curr_min)))
        then curr_min := (x, ai)
        else curr_min := !curr_min;
  in
    IntArray.appi unvisited_min dist_to_root;
    !curr_min
  end;

fun nbr_curr_dists (curr_node : int, nbrs : int list) : (int*int) list =
  (* Get current distances from root for unvisited neighbors of
     current node and put in pairs with their array indices *)
  let
    val curr_dist = IntArray.sub(dist_to_root, curr_node);
    val nbr_dists = map
      (fn ai => IntArray.sub(dist_to_root, ai)) nbrs;
  in
    ListPair.zip(nbrs, nbr_dists)
  end;

(*------------------------------------------------------------------------*)
(*
   Apply Dijkstra's algorithn to the graph of puzzle states, with the
   root node the standard finish state.  All edges have a weight of
   one.  Two nodes are joined by an edge if there is a valid puzzle
   move between them.  The table used consists of the following
   mutable arrays indexed by array_index, which is an integer code
   for puzzle states.  However, note than not all array indices
   correspond to a valid puzzle state.  These are ignored by setting
   them as "visited" during initialization.

      states - Internal state coded as hex digits in 64-bit word.
               This is a "legacy" internal state coding used as a
               helper for calculating neighbor nodes.  It is filled
               during initialization.

      dist_to_root - The current shortest distance (number of moves/
                     edges) from the root node.

      prev_state - The state/node which is the previous node on the
                   shortest path from root.

      visited - Set to true or false to indicate if this node has been
                "visited" meaning that the shortest path from it to
                root has been found.  The path can be calculated by
                following the sequence of prev_state values from this
                node to root.
*)
val visited_count = ref 0;

fun dijkstra () =
(
  init_states();
  init_dist_to_root();
  init_prev_state();
  init_visited();
  IntArray.update(dist_to_root, root_array_index, 0);
  (* Loop until all nodes are marked as visited *)
  while (not(all_nodes_visited())) do
    let
      val curr = get_min_unvisited();
      (* Current node is set to be an unvisited node with the minimum
         distance to the root node *)
      val curr_min = #1 curr;
      val curr_node = #2 curr;
      (* Get unvisited neighbors of current node *)
      val unv_nbrs = unvisited_neighbors(curr_node)
    in
      if List.null(unv_nbrs) then ()
      else
        (* If current node has unvisited neighbors whose current distance
           from root is larger than current node's distance plus one,
           change those neighbors to this new distance and make the
           current node their previous node *)
        let
          val nbr_dists = nbr_curr_dists(curr_node, unv_nbrs);
          val new_dist = 1 + IntArray.sub(dist_to_root, curr_node);
          val num_nbrs = length(unv_nbrs);
          val n = ref 0
        in

          while (!n < num_nbrs) do
          (
            let
              val nbr_pair = List.nth(nbr_dists, !n);
              val nbr_index = #1 nbr_pair;
              val nbr_dist = #2 nbr_pair;
            in
              if new_dist < nbr_dist then
              (
                IntArray.update(dist_to_root, nbr_index, new_dist);
                IntArray.update(prev_state, nbr_index, curr_node)
              )
              else ()
            end;
            n := !n + 1
          )
        end;
      (* Mark the current node as visited *)
      BoolArray.update(visited, curr_node, true);
      (* For monitoring progress *)
      visited_count := !visited_count + 1;
      if ((!visited_count mod 10000) = 0) then
      (
        print (Int.toString(!visited_count));
        print " nodes visited\n"
      )
      else ()
    end;
  print (Int.toString(!visited_count));
  print " nodes visited\n";
  print("Done building table\n")
);

(*------------------------------------------------------------------------*)
(* Functions to find path lengths from standard start states to the
   standard finish state (root node) *)
fun valid_start (array_index : int) : bool =
  (* Is the array index for a standard puzzle start state? *)
  let
    val occupy_index = array_index mod 64;
    val n3 = occupy_index div 16;
    val n2 = (occupy_index mod 16) div 4;
    val n0 = occupy_index mod 4;
    val n1 = 8 - n0 - n2- n3;
  in
    if ((n2 = 3) andalso (n1 = 5)) then true else false
  end;

fun write_moves_file (out_file) =
  let
    val out_stream = TextIO.openAppend(out_file);
    fun print_moves (ai, dist) =
      if (valid_start(ai)) then
        let
          val ios = array_index_to_iostate(ai);
        in
          print_iostate_file(out_stream, ios);
          TextIO.output(out_stream, " : ");
          TextIO.output(out_stream, Int.toString(dist));
          TextIO.output(out_stream, "\n")
        end
      else ()
  in
    IntArray.appi print_moves dist_to_root;
    TextIO.closeOut(out_stream);
    print "\nDone writing moves file\n"
  end

fun write_table_file (out_file) =
  let
    val out_stream = TextIO.openAppend(out_file);
    fun print_table (ai, dist) =
      let
        val ai_str = Int.toString(ai) ^ ", ";
        val state_str = Word64.toString(Array.sub(states, ai)) ^ ", ";
        val dist_str = Int.toString(dist) ^ ", ";
        val prev_state_str = Int.toString(IntArray.sub(prev_state, ai));
        val line = ai_str ^ state_str ^ dist_str ^ prev_state_str ^ "\n";
      in
        TextIO.output(out_stream, line)
      end
  in
    IntArray.appi print_table dist_to_root;
    TextIO.closeOut(out_stream);
    print "\nDone writing table file\n"
  end

(*
fun path_to_root (array_index : int) : int list =
  (* Get list of array indices in path from state with array_index to
     root node *)
  if (valid_array_index(array_index)) then
    let
      fun ptr (root_array_index, pl) = List.rev(root_array_index::pl)
        | ptr (ai, pl) = ptr(IntArray.sub(prev_state, ai), ai::pl);
    in
      ptr(array_index, [])
    end
  else [];
*)
(*------------------------------------------------------------------------*)
(*------------------------------------------------------------------------*)
(* The functions below are for creating stand-alone executables *)

(* For compilation of a stand-alone program *)

fun main () =
(
  dijkstra();
  write_moves_file("num-moves.txt");
  write_table_file("dijkstra-table.txt")
);

(* For compilation with mlton, the next line must be un-commented.
   it is not needed for compilation with polyc *)
(*
main ();
*)
