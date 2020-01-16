(*-------------------------------------------------------------------------
  inglenook-sidings-full-opt.sml

  Print paths from optimal solutions to a classic shunting (switching)
  puzzle using Standard ML.  Uses table produced by Dijkstra's algorithm.

  Directions for use:

  - Run the program in inglenook-sidings-dijkstra.sml to produce the file
    dijkstra-table.txt.  This will take 9 or more hours.  See that program's
    comments for further information.

  - Start Poly/ML in a terminal to get an interactive Standard ML session
    and enter at the prompt

      use("inglenook-sidings-full-opt.sml");

    to load this program.  (You may need to include the director path to
    the file.)  The semicolon is necessary.

  - After the file is loaded, at the prompt enter

      read_table_file("dijkstra-table.txt");

    to load the data from Dijkstra's algorithm.  (Again, you may need to
    pre-pend a path name.)  Loading will take a minute or two.

  - To generate a random puzzle problem, enter

      generate_problem(1);

    An example of the output from this is:

        Start state: ([0],[5,7,2,1,8],[3,4,6],[])
        Moves: 15
        Solution:
        ([0],[5,7,2,1,8],[3,4,6],[])
        ([0,5,7,2],[1,8],[3,4,6],[])
        ([0],[1,8],[3,4,6],[5,7,2])
        ([0,1,8],[],[3,4,6],[5,7,2])
        ([0,1,8,5],[],[3,4,6],[7,2])
        ([0,1,8],[5],[3,4,6],[7,2])
        ([0,1],[5],[3,4,6],[8,7,2])
        ([0,1,3,4],[5],[6],[8,7,2])
        ([0,1],[3,4,5],[6],[8,7,2])
        ([0,1,6],[3,4,5],[],[8,7,2])
        ([0,1,6,8],[3,4,5],[],[7,2])
        ([0,1,6],[3,4,5],[8],[7,2])
        ([0,1,6,7],[3,4,5],[8],[2])
        ([0,1],[3,4,5],[6,7,8],[2])
        ([0,1,2],[3,4,5],[6,7,8],[])
        ([0],[1,2,3,4,5],[6,7,8],[])

    The solution is a series of puzzle states from the start to the finish.
    Each state is a 4-tuple (t0, t1, t2, t3) where the entries tn are a
    list of integers representing cars on a siding.  The cars are numbered
    1 through 8.  The zero is the shunting engine and the first track is the
    shunting lead.

    Change the 1 to a larger value if you want to generate more problems
    in this step.  To do the same thing, but write the results to a file,
    enter

      generate_problem_file(1, "name of output file in quotes");

  - To get an optimal solution to a particular starting condition (standard
    or not) you can use the function solve_opt.  An example is:

      solve_opt(([0], [7,3,1,2,6], [5,4,8], []));

      Moves: 13
      Solution:
      ([0],[7,3,1,2,6],[5,4,8],[])
      ([0,5,4],[7,3,1,2,6],[8],[])
      ([0,5,4,7],[3,1,2,6],[8],[])
      ([0,5,4],[3,1,2,6],[7,8],[])
      ([0,5,4,3],[1,2,6],[7,8],[])
      ([0],[1,2,6],[7,8],[5,4,3])
      ([0,1,2,6],[],[7,8],[5,4,3])
      ([0,1,2],[],[6,7,8],[5,4,3])
      ([0,1,2,5],[],[6,7,8],[4,3])
      ([0,1,2],[5],[6,7,8],[4,3])
      ([0,1,2,4],[5],[6,7,8],[3])
      ([0,1,2],[4,5],[6,7,8],[3])
      ([0,1,2,3],[4,5],[6,7,8],[])
      ([0],[1,2,3,4,5],[6,7,8],[])

    Note that the engine "0" must always be the first entry on the lead
    track.

  - To exit the Poly/ML session, hit Cntl-d.

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
(* Columns in Dijkstra's algorithm table (except "visited")

      states - Internal state coded as hex digits in 64-bit word.
               This is a "legacy" internal state coding.

      dist_to_root - The shortest distance (number of moves/
                     edges) from/to the start node.

      prev_state - The state/node which is the previous node on the
                   shortest path from/to start node.

*)

(* Number of array elements, including non-legits *)
val num_array_elts = 3 + 4*3 + 16*3 + 64*(40320 - 1);
(* Array for internal state rep of each node *)
val states = Array.array(num_array_elts, Word64.fromInt(0));
(* Array for distance to root node (standard final puzzle state) *)
val dist_to_root = IntArray.array(num_array_elts, 0);
(* Array for array_index of previous state for a state  *)
val prev_state = IntArray.array(num_array_elts, 0);

(* Array index and state of root node *)
val root_array_index = iostate_to_array_index(([0],[1,2,3,4,5],[6,7,8],[]));
val root_state = iostate_to_state(([0],[1,2,3,4,5],[6,7,8],[]));

fun read_table_file (in_file) =
  let
    val in_stream = TextIO.openIn(in_file);
  in
    while (Bool.not(TextIO.endOfStream(in_stream))) do
      let
        fun is_comma (c : char) = (c = #",");
        val in_line = Option.valOf(TextIO.inputLine(in_stream));
        val lgt_line = String.size(in_line) - 1;
        val line = String.substring(in_line, 0, lgt_line);
        val entries = String.fields is_comma line;
        val ai = Option.valOf(Int.fromString(List.nth(entries, 0)));
        val state = Option.valOf(Word64.fromString(List.nth(entries, 1)));
        val dist = Option.valOf(Int.fromString(List.nth(entries, 2)));
        val prev = Option.valOf(Int.fromString(List.nth(entries, 3)));
      in
        Array.update(states, ai, state);
        IntArray.update(dist_to_root, ai, dist);
        IntArray.update(prev_state, ai, prev)
      end;
    TextIO.closeIn(in_stream);
    print "\nTable read complete\n"
  end;

(*------------------------------------------------------------------------*)
(* Functions to find paths from state to the standard finish state (root) *)

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

fun ai_path_to_root (array_index : int) : int list =
  (* Get list of array indices in path from state with array_index to
     root node *)
  let
    val root_ai = iostate_to_array_index(([0], [1,2,3,4,5], [6,7,8], []));
    fun ptr (ai, pl) =
      if (ai = root_ai) then List.rev(root_ai::pl)
      else ptr(IntArray.sub(prev_state, ai), ai::pl);
  in
    if (valid_array_index(array_index)) then ptr(array_index, [])
    else []
  end;

fun opt_path (start : iostate) =
  (* Given start state in I/O format, return an optimal (minimum length)
     solution to the standard finish state found by Dijkstra's algorithm
     in I/O format *)
  let
    val ai = iostate_to_array_index(start);
    val ai_path = ai_path_to_root(ai);
  in
    map array_index_to_iostate ai_path
  end;

fun solve_opt (start : iostate) =
  (
    if valid_start(iostate_to_array_index(start)) then ()
    else print "\n*** This is not a standard start condition ***";
    print "\n";
    print_solution(opt_path(start));
    print "\n"
  );

(*------------------------------------------------------------------------*)
(* Functions to generate random start contitions and print solutions *)
(* Note:  These are modified from previous versions because if multiple
   solutions are requested, they are retrieved very quickly and this did
   not work well with the previous versions. *)

fun get_real_seed () =
  (* Return a real random seed based on system time *)
  let
    val t = Time.toMicroseconds(Time.now())
    val s = Int.fromLarge(t mod 1000000000);
  in
    Real.fromInt(s)
  end;

local
  val a = 16807.0;
  val m = 2147483647.0
in
  fun nextrand seed =
    let val t = a*seed
    in t - m*real(floor(t/m))
    end
  fun randlist (n, seed, acc) =
    if n = ~1 then map (fn x => x/m) (tl(List.rev(acc)))
    else randlist(n-1, nextrand(seed), seed::acc);
end;

(* Return the list l with the index n element removed *)
fun remove_at ([], n) = []  (* n is list index starting at 0 *)
  | remove_at (l, n) = List.take(l, n) @ List.drop(l, n+1);

(* Return list of randomly chosen elts from list l using a
   list url of uniformly distributed random numbers *)
fun rnd_select (l, []) = []
  | rnd_select ([], url) = []
  | rnd_select (l, url) =
      let
        val lim = Real.fromInt(length l);
        val u = hd(url);
        val i = Real.floor(Real.*(u, lim));
      in
        List.nth(l, i)::rnd_select(remove_at(l, i), tl(url))
      end;

fun rnd_permu (l, m) =
  (* Return a list of m random permutations of the list l *)
  let
    val n = length(l);
    val seed = get_real_seed();
    val urls = randlist(m*n, seed, []);
    val k = ref 0;
    val acc = ref [];
  in
    while (!k < m) do
      let
        val u = List.drop(urls, (!k)*n);
        val url = List.take(u, n)
      in
        acc := rnd_select(l, url)::(!acc);
        k := !k + 1
      end;
    !acc
  end;

local
  fun gp ([]) = ()
    | gp (perms) =
        let
          val perm = hd(perms);
          val start:iostate =
                ([0], List.take(perm, 5), List.drop(perm, 5), []);
        in
          print "\nStart state: "; print_iostate start;
          print_solution(opt_path(start));
          gp(tl(perms))
        end;
in
  fun generate_problem (n) =
    (* Generate n random standard puzzle problems and print optimal
       solution paths for them *)
    if (n <= 0) then ()
    else
      let
        val perms = rnd_permu([1, 2, 3, 4, 5, 6, 7, 8], n)
      in
        gp(perms);
        print "\nDone\n"
      end;
end;

local
  fun gpf ([], out_stream) = ()
    | gpf (perms, out_stream) =
        let
          val perm = hd(perms);
          val start:iostate =
                ([0], List.take(perm, 5), List.drop(perm, 5), []);
          val sol = opt_path(start)
        in
          TextIO.output(out_stream, "Start State: ");
          print_iostate_file(out_stream, start);
          print_solution_file(out_stream, sol);
          gpf(tl(perms), out_stream)
        end;
in
  fun generate_problem_file (n, out_file) =
    (* Generate n random standard puzzle problems and write optimal
       solution paths for them to a file *)
    if (n <= 0) then ()
    else
      let
        val perms = rnd_permu([1, 2, 3, 4, 5, 6, 7, 8], n);
        val out_stream = TextIO.openAppend(out_file)
      in
        gpf(perms, out_stream);
        TextIO.closeOut(out_stream);
        print "\nDone\n"
      end;
end;

(*------------------------------------------------------------------------*)
(* Find maximum distance to root of all puzzle states *)
(* Note:  Maximum number of moves is one less than distance to root *)
fun max_dist_to_root (dtr : IntArray.array) =
  let
    val max_ind = ref 0;
    fun larger(i, d, m) =
      if ((d < 1000) andalso (d > m)) then
        (max_ind := i; d)
        else m;
    val max = IntArray.foldli larger 0 dtr
  in
    print ("\nMax: "^(Int.toString(max)));
    print ("\nMax Index: "^(Int.toString(!max_ind))^"\n")
  end;

(*------------------------------------------------------------------------*)
(* Find counts of distance to root for all puzzle states *)
val count_dist = IntArray.array(26, 0);

fun find_count_dist (out_file, dtr : IntArray.array) =
  let
    val out_stream = TextIO.openAppend(out_file);
    fun incr_count (d) =
      if (d < 1000) then
        IntArray.update(count_dist, d, IntArray.sub(count_dist, d)+1)
      else ();
    fun write_count (i, d) =
      TextIO.output(out_stream,
            "Moves: "^Int.toString(i)^"\tCount: "^Int.toString(d)^"\n")
  in
    IntArray.app incr_count dtr;
    IntArray.appi write_count count_dist;
    TextIO.closeOut(out_stream)
  end

(*------------------------------------------------------------------------*)
(* The functions below are for creating stand-alone executables *)

(* For compilation of a stand-alone program *)
(*
fun main () =
(
  read_table_file(...);
  etc.
);
*)
(* For compilation with mlton, the next line must be un-commented.
   it is not needed for compilation with polyc *)
(* main (); *)
