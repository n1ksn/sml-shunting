(*--------------------------------------------------------------------------
 * In version 4 the internal representation of puzzle states has been
 * changed to a single 64-bit word.  The previous state data structure
 * is retained for input and output, named "iostate".
 *
 * In version 3 the function generate_problem_file has been added.
 *
 * Functions in version 2 have been modified from version 1 so that
 * printing is separated from the solve functions.
 *
 * Andrew Palm
 * 2019-12-26
 *------------------------------------------------------------------------*)
(* Bring in utilities using randomness *)
(* The following function urandomlist is taken from Rosetta Code.
   Given a real seed and integer n, return a list of n random reals
   between 0 and 1 *)
val urandomlist =  fn seed => fn n =>
let
  val uniformdeviate = fn seed =>
  let
    val in31m = (Real.fromInt o Int.toInt ) (getOpt (Int.maxInt,0) );
    val in31 = in31m +1.0;
    val s1 = 41160.0;
    val s2 = 950665216.0;
    val v = Real.realFloor seed;
    val val1 = v*s1;
    val val2 = v*s2;
    val next1 = Real.fromLargeInt
                  (Real.toLargeInt IEEEReal.TO_NEGINF (val1/in31)) ;
    val next2 = Real.rem(Real.realFloor(val2/in31) , in31m );
    val valt = val1+val2 - (next1+next2)*in31m;
    val nextt = Real.realFloor(valt/in31m);
    val valt = valt - nextt*in31m;
  in
    (valt/in31m,valt)
  end;
val store =  ref (0.0,0.0);
val rec u =  fn S => fn 0 => [] |
  n=> (store:=uniformdeviate S; (#1 (!store)):: (u (#2 (!store)) (n-1))) ;
in
  u seed n
end;

fun get_real_seed () =
  (* Return a real random seed based on system time *)
  let
    val t = Time.toMilliseconds(Time.now());
    val s = Int.fromLarge(t mod 100000);
  in
    Real.fromInt(s)
  end;

(* Return the list l with the index n element removed *)
fun remove_at ([], n) = []  (* n is list index starting at 0 *)
  | remove_at (l, n) = List.take(l, n) @ List.drop(l, n+1);

(* Helper function for rnd_select *)
fun rs1 ([], url) = []
  | rs1 (l, []) = []
  | rs1 (l, url) =
    let
      val lim = Real.fromInt(length l);
      val u = hd(url);
      val i = Real.floor(Real.*(u, lim));
    in
      List.nth(l, i)::rs1(remove_at(l, i), tl(url))
    end;

(* Return a list of n elements randomly selected from list l *)
fun rnd_select (l, 0) = []
  | rnd_select ([], n) = []
  | rnd_select (l, n) =
  (* Return list of n randomly chosen elts from list l *)
    let
      val seed = get_real_seed();
      val url = urandomlist seed n;
    in
      rs1(l, url)
    end;

fun rnd_permu (l) =
  (* Return a random permutation of the list l *)
  rnd_select(l, (length l));

(*------------------------------------------------------------------------*)
(* Utilities *)
fun member (x, []) = false
  | member (x, h::t) = x = h orelse member (x, t);

fun remove_if (f, []) = []
  | remove_if (f, h::t) =
      if f h then remove_if(f, t)
      else h::remove_if(f, t);
(*
fun last_n ([], n) = []
  | last_n (l, 0) = []
  | last_n (l, n) =
      let val lgtl = length l in
        if n >= lgtl then l
        else List.drop(l, lgtl-n)
      end;
*)
(*------------------------------------------------------------------------*)
(* Functions to convert between the "external" puzzle state used for
   I/O and the internal puzzle state used for calculatios.

   The layout of a 64-bit word internal puzzle state is as follows:

         Nybble         Track         Position from points
          0-2             3                  3,2,1
          3-5             2                  3,2,1
          6-10            1                  5,4,3,2,1
          11-13           0                  1,2,3

   Cars are represented by hex digits 1 to 8.  Empty track positions
   are zeros.  Cars are right-justified in their tracks.
*)
type state = Word64.word;

val w3shift = Word.fromInt(0);
val w2shift = Word.fromInt(12);
val w1shift = Word.fromInt(24);
val w0shift = Word.fromInt(44);

val w3mask = Option.valOf(Word64.fromString("0wxFFF"));
val w2mask = Option.valOf(Word64.fromString("0wxFFF000"));
val w1mask = Option.valOf(Word64.fromString("0wxFFFFF000000"));
val w0mask = Option.valOf(Word64.fromString("0wxFFF00000000000"));

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

val mask1car = Option.valOf(Word64.fromString("0wxF"));
val mask2cars = Option.valOf(Word64.fromString("0wxFF"));
val mask3cars = Option.valOf(Word64.fromString("0wxFFF"));

val zeroword = Option.valOf(Word64.fromString("0wx0"));

(* The external state is a 4-tuple of integer lists, one for each track
        (Track 0 list, Track 1 list, Track 2 list, Track 2 list)
*)
type iostate = int list * int list * int list * int list;

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

fun word_to_int_list (w:Word64.word):int list =
  wtil1(w, []);

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
(* For printing a state and puzzle solution (a list of states) *)
fun ilts1 (l:int list, acc:string) =
 (* Helper function for int_list_to_string *)
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

fun int_list_to_string (l:int list):string = ilts1(l, "[");

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

fun print_iostate_list ([]) = ()
  | print_iostate_list (ios:iostate list) =
    (
      print_iostate (hd(ios));
      print_iostate_list (tl(ios))
    );

fun print_solution (solution:iostate list) =
  (
    print "Moves: "; print (Int.toString((length solution)-1));
    print "\nSolution:\n"; print_iostate_list solution;
    print "\n"
  );

(*------------------------------------------------------------------------*)
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


(*------------------------------------------------------------------------*)
fun next_states state =
  remove_if((fn x => (x=zeroword)),
              [p3t1 state, p3t2 state, p3t3 state,
               d3t1 state, d3t2 state, d3t3 state,
               p2t1 state, p2t2 state, p2t3 state,
               d2t1 state, d2t2 state, d2t3 state,
               p1t1 state, p1t2 state, p1t3 state,
               d1t1 state, d1t2 state, d1t3 state]);

(* Return list of paths which extend path by a legal move, provided
   the result does not exceed limit in length and does not visit a state
   already in the path (to avoid cycles) *)
fun limited_extend (path, limit) =
  if (length path) < limit then
    map (fn new_state => new_state::path)
          (remove_if ((fn x => member(x, path)), (next_states (hd path))))
  else
    [];

(* Find path from start to finish state that is no longer than limit *)
fun ldfs1 (start, goal:(state -> bool), [], limit) = []
  | ldfs1 (start, goal:(state -> bool), queue, limit) =
      if goal(hd (hd queue)) then
        rev (hd queue)
      else
        ldfs1 (start, goal:(state -> bool),
                (limited_extend ((hd queue), limit)) @ (tl queue), limit);

fun ldfs (start, goal:(state -> bool), limit) =
  ldfs1(start, goal:(state -> bool), [[start]], limit);

(* Make series of limited depth first searches, increasing the limit
   on path length until a solution is found - iterative deepening
   depth-first search *)
fun iddfs (start, goal:(state -> bool), max_limit) =
  let
    val sol = ref [] and n = ref 0
  in
    while ((!n < max_limit) andalso (!sol = [])) do
      ( n := !n + 1;
        sol := ldfs(start, goal:(state -> bool), !n)
      );
    !sol
  end;

(*------------------------------------------------------------------------*)
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

(*------------------------------------------------------------------------*)
val last3mask = pos3w1mask + pos4w1mask + pos5w1mask;

fun last3t1 (s:state):Word64.word =
  Word64.andb(s, last3mask);

fun solve_pure (iostart:iostate, iofinish:iostate):iostate list =
  (* Find a solution that is a minimal solution from the start
     to the finish state *)
  let
    val finish = iostate_to_state(iofinish);
    val start = iostate_to_state(iostart);
    val fgoal = (fn x:state => (x = finish));
    val sol = iddfs(start, fgoal, 25);
  in
    map (state_to_iostate) (sol) 
  end;

fun solve (iostart:iostate, iofinish:iostate):iostate list =
  (* Find a solution that is join of two minimal solutions from the start
     to an intermediate state and from the intermediate state to the
     finish state *)
  let
    val finish = iostate_to_state(iofinish);
    val start = iostate_to_state(iostart);
    (* Last 3 cars, track 1, at finish *)
    val last3_fin_t1 = last3t1(finish);
    (* Intermediate goal is last 3 cars on track 1 same as finish *)
    (* If the igoal is met by the start state it becomes the intermediate
       state and there is effectively only one search from start to
       finish *)
    val igoal = (fn x:state => (last3t1(x) = last3_fin_t1));
    (* Solution from start to intermediate state *)
    val sol1 = iddfs(start, igoal, 25);
    (* Intermediate state *)
    val inter = List.last(sol1);
    (* Goal for final state *)
    val fgoal = (fn x:state => (x = finish));
    (* Solution from intermediate state to finish state *)
    val sol2 = iddfs(inter, fgoal, 25);
  in
    map (state_to_iostate) (sol1 @ tl(sol2)) (* Combined solution *)
  end;

fun generate_problem (0) = print "\n\nDone!\n"
  (* Randomly generate n puzzle problems and solve them *)
  | generate_problem (n) =
    let
      val perm = rnd_permu([1, 2, 3, 4, 5, 6, 7, 8]);
      val start:iostate = ([0], List.take(perm, 5), List.drop(perm, 5), []);
      val finish:iostate = ([0], [1, 2, 3, 4, 5], [6, 7, 8], [])
    in
      print "\nStart state: "; print_iostate start;
      print_solution(solve(start, finish));
      generate_problem(n-1)
   end;

fun gpf1 (0, out_stream) =
      (
        TextIO.closeOut(out_stream);
        print "\nDone!\n"
      )
  | gpf1 (n, out_stream) =
      let
        val perm = rnd_permu([1, 2, 3, 4, 5, 6, 7, 8]);
        val start:iostate = ([0], List.take(perm, 5), List.drop(perm, 5), []);
        val finish:iostate = ([0], [1, 2, 3, 4, 5], [6, 7, 8], []);
        val sol = solve(start, finish)
      in
        TextIO.output(out_stream, "\nStart State: ");
        print_iostate_file(out_stream, start);
        print_solution_file(out_stream, sol);
        gpf1(n-1, out_stream)
      end;

fun generate_problem_file(n, out_file) =
  (* Randomly generate n puzzle problems, solve them, and save
     solutions to a file *)
  let
    val out_stream = TextIO.openAppend(out_file)
  in
    gpf1(n, out_stream)
  end;

(*------------------------------------------------------------------------*)
(* For compilation of stand-alone *)
fun main () =
  let
    val args = CommandLine.arguments ();
  in
    if (length args) > 0 then
      generate_problem(Option.valOf(Int.fromString(hd args)))
    else
      generate_problem(1)
  end;

(* For mlton
main ();
*)
