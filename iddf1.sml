(* Utilities *)
fun member (x, []) = false
  | member (x, h::t) = x = h orelse member (x, t);

fun remove_if (f, []) = []
  | remove_if (f, h::t) = if f h then remove_if(f, t)
                                 else h::remove_if(f, t);

(* Functions that define successor (neighbor) states *)
fun ns state =
  if (member (state, ["a", "s"])) then "s" else "none";
fun na state =
  if (member (state, ["s", "b", "d"])) then "a" else "none";
fun nb state =
  if (member (state, ["a", "c", "e"])) then "b" else "none";
fun nc state =
  if (member (state, ["b"])) then "c" else "none";
fun nd state =
  if (member (state, ["s", "a", "e"])) then "d" else "none";
fun ne state =
  if (member (state, ["b", "d", "f"])) then "e" else "none";
fun nf state =
  if (member (state, ["e"])) then "f" else "none";

fun next_states state =
  remove_if((fn x => (x="none")), [ns state, na state, nb state, nc state,
                      nd state, ne state, nf state]);

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
fun ldfs1 (start, finish, [], limit) = []
  | ldfs1 (start, finish, queue, limit) =
      if hd (hd queue) = finish then
        rev (hd queue)
      else
        ldfs1 (start, finish,
                (limited_extend ((hd queue), limit)) @ (tl queue), limit);

fun ldfs (start, finish, limit) =
  ldfs1(start, finish, [[start]], limit);

(* Make series of limited depth first searches, increasing the limit
   on path length until a solution is found - iterative deepening
   depth-first search *)
fun iddfs (start, finish, max_limit) =
  let val sol = ref [] and n = ref 0
  in
    while ((!n < max_limit) andalso (!sol = [])) do
      ( n := !n + 1;
        sol := ldfs(start, finish, !n)
      );
    !sol
  end;
