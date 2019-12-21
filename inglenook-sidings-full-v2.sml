(*--------------------------------------------------------------------------
 * Functions in this version 2 have been modified from version 1 so that
 * printing is separated from the solve functions.
 *
 * Andrew Palm
 * 2019-12-21
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

fun last_n ([], n) = []
  | last_n (l, 0) = []
  | last_n (l, n) =
      let val lgtl = length l in
        if n >= lgtl then l
        else List.drop(l, lgtl-n)
      end;

(*------------------------------------------------------------------------*)
(* Define type "state" for a puzzle configuration *)
type state = int list * int list * int list * int list;
val empty_state = ([], [], [], []):state;

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

fun print_state ((t0, t1, t2, t3):state) =
  (
    print "(";
    print_int_list(t0); print ", ";
    print_int_list(t1); print ", ";
    print_int_list(t2); print ", ";
    print_int_list(t3);
    print ")\n"
  );

fun print_state_list ([]) = ()
  | print_state_list (s:state list) =
    (
      print_state (hd(s));
      print_state_list (tl(s))
    );

fun print_solution solution =
  (
    print "Moves: "; print (Int.toString((length solution)-1));
    print "\nSolution:\n"; print_state_list solution
  );

(*------------------------------------------------------------------------*)
(* Functions that define successor states using legal moves *)
fun p3t1 ((t0, t1, t2, t3):state):state =
  (* Pull three cars from track 1 *)
  if ((length t0 = 1) andalso (length t1 >= 3)) then
    (t0 @ List.take(t1, 3), List.drop(t1, 3), t2, t3)
  else
    empty_state;

fun p3t2 ((t0, t1, t2, t3):state):state =
  (* Pull three cars from track 2 *)
  if ((length t0 = 1) andalso (length t2 = 3)) then
    (t0 @ t2, t1, [], t3)
  else
    empty_state;

fun p3t3 ((t0, t1, t2, t3):state):state =
  (* Pull three cars from track 3 *)
  if ((length t0 = 1) andalso (length t3 = 3)) then
    (t0 @ t3, t1, t2, [])
  else
    empty_state;

fun d3t1 ((t0, t1, t2, t3):state):state =
  (* Drop three cars to track 1 *)
  if ((length t0 = 4) andalso (length t1 < 3)) then
    (List.take(t0, 1), List.drop(t0,1) @ t1, t2, t3)
  else
    empty_state;

fun d3t2 ((t0, t1, t2, t3):state):state =
  (* Drop three cars to track 2 *)
  if ((length t0 = 4) andalso (length t2 = 0)) then
    (List.take(t0, 1), t1, List.drop(t0,1), t3)
  else
    empty_state;

fun d3t3 ((t0, t1, t2, t3):state):state =
  (* Drop three cars to track 3 *)
  if ((length t0 = 4) andalso (length t3 = 0)) then
    (List.take(t0, 1), t1, t2, List.drop(t0,1))
  else
    empty_state;

fun p2t1 ((t0, t1, t2, t3):state):state =
  (* Pull two cars from track 1 *)
  if ((length t0 < 3) andalso (length t1 >= 2)) then
    (t0 @ List.take(t1, 2), List.drop(t1, 2), t2, t3)
  else
    empty_state;

fun p2t2 ((t0, t1, t2, t3):state):state =
  (* Pull two cars from track 2 *)
  if ((length t0 < 3) andalso (length t2 >= 2)) then
    (t0 @ List.take(t2, 2), t1, List.drop(t2, 2), t3)
  else
    empty_state;

fun p2t3 ((t0, t1, t2, t3):state):state =
  (* Pull two cars from track 3 *)
  if ((length t0 < 3) andalso (length t3 >= 2)) then
    (t0 @ List.take(t3,2), t1, t2, List.drop(t3,2))
  else
    empty_state;

fun d2t1 ((t0, t1, t2, t3):state):state =
  (* Drop two cars to track 1 *)
  if ((length t0 >= 3) andalso (length t1 < 4)) then
    let val n = (length t0) - 2 in
      (List.take(t0, n), List.drop(t0,n) @ t1, t2, t3)
    end
  else
    empty_state;

fun d2t2 ((t0, t1, t2, t3):state):state =
  (* Drop two cars to track 2 *)
  if ((length t0 >= 3) andalso (length t2 < 2)) then
    let val n = (length t0) - 2 in
      (List.take(t0, n), t1, List.drop(t0, n) @ t2, t3)
    end
  else
    empty_state;

fun d2t3 ((t0, t1, t2, t3):state):state =
  (* Drop two cars to track 3 *)
  if ((length t0 >= 3) andalso (length t3 < 2)) then
    let val n = (length t0) - 2 in
      (List.take(t0, n), t1, t2, List.drop(t0, n) @ t3)
    end
  else
    empty_state;

fun p1t1 ((t0, t1, t2, t3):state):state =
  (* Pull one car from track 1 *)
  if ((length t0 < 4) andalso (length t1 >= 1)) then
    (t0 @ List.take(t1, 1), List.drop(t1, 1), t2, t3)
  else
    empty_state;

fun p1t2 ((t0, t1, t2, t3):state):state =
  (* Pull one car from track 2 *)
  if ((length t0 < 4) andalso (length t2 >= 1)) then
    (t0 @ List.take(t2, 1), t1, List.drop(t2, 1), t3)
  else
    empty_state;

fun p1t3 ((t0, t1, t2, t3):state):state =
  (* Pull one car from track 3 *)
  if ((length t0 < 4) andalso (length t3 >= 1)) then
    (t0 @ List.take(t3, 1), t1, t2, List.drop(t3, 1))
  else
    empty_state;

fun d1t1 ((t0, t1, t2, t3):state):state =
  (* Drop one car to track 1 *)
  if ((length t0 >= 2) andalso (length t1 < 5)) then
    let val n = (length t0) - 1 in
      (List.take(t0, n), List.drop(t0, n) @ t1, t2, t3)
    end
  else
    empty_state;

fun d1t2 ((t0, t1, t2, t3):state):state =
  (* Drop one car to track 2 *)
  if ((length t0 >= 2) andalso (length t2 < 3)) then
    let val n = (length t0) - 1 in
      (List.take(t0, n), t1, List.drop(t0, n) @ t2, t3)
    end
  else
    empty_state;

fun d1t3 ((t0, t1, t2, t3):state):state =
  (* Drop one car to track 3 *)
  if ((length t0 >= 2) andalso (length t3 < 3)) then
    let val n = (length t0) - 1 in
      (List.take(t0, n), t1, t2, List.drop(t0, n) @ t3)
    end
  else
    empty_state;

(*------------------------------------------------------------------------*)
fun next_states state =
  remove_if((fn x => (x=empty_state)),
              [p1t1 state, p1t2 state, p1t3 state,
               d1t1 state, d1t2 state, d1t3 state,
               p2t1 state, p2t2 state, p2t3 state,
               d2t1 state, d2t2 state, d2t3 state,
               p3t1 state, p3t2 state, p3t3 state,
               d3t1 state, d3t2 state, d3t3 state]);

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
fun solve (start:state, finish:state) =
  (* Find a solution that is join of two minimal solutions from the start
     to an intermediate state and from the intermediate state to the
     finish state *)
  let
    val f1 = #2 finish;
    (* Last 3 cars, track 1, at finish *)
    val f1_last3 = last_n(f1, 3);
    (* Intermediate goal is last 3 cars on track 1 same as finish *)
    (* If the igoal is met by the start state it becomes the intermediate
       state and there is effectively only one search from start to
       finish *)
    val igoal = (fn x:state => (last_n((#2 x), 3) = f1_last3));
    (* Solution from start to intermediate state *)
    val sol1 = iddfs(start, igoal, 25);
    (* Intermediate state *)
    val inter = List.last(sol1);
    (* Goal for final state *)
    val fgoal = (fn x:state => (x = finish));
    (* Solution from intermediate state to finish state *)
    val sol2 = iddfs(inter, fgoal, 25);
  in
    sol1 @ tl(sol2) (* Combined solution *)
  end;

fun generate_problem (0) = print "\n\nDone!\n"
    | generate_problem (n) =
  (* Randomly generate n puzzle problems and solve them *)
    let
      val perm = rnd_permu([1, 2, 3, 4, 5, 6, 7, 8]);
      val start:state = ([0], List.take(perm, 5), List.drop(perm, 5), []);
      val finish:state = ([0], [1, 2, 3, 4, 5], [6, 7, 8], [])
    in
      print "\nStart state: "; print_state start;
      print_solution(solve(start, finish));
      generate_problem(n-1)
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
