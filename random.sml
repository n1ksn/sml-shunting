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
    val next1 = Real.fromLargeInt (Real.toLargeInt IEEEReal.TO_NEGINF (val1/in31)) ;
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
