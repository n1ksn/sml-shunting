# sml-shunting
Standard ML programs for the Inglenook Sidings Puzzle

## inglenook-sidings-full-v6.sml

Find solutions to a classic shunting (switching) puzzle using Standard
ML.

## inglenook-sidings-small-v0.sml

Find solutions to the small version of the puzzle.

The comments in this README file apply to the full-sized version of
the puzzle (code in the first file above).  The small version has
fewer functions available and differs in the number of cars used
and the track capacities.

This code has been tested interactively with Poly/ML on Linux and
Windows 10.  Also, executables have been compiled successfully using
polyc and mlton in Linux (Fedora and Linux Mint/Ubuntu).

Andrew Palm
2020-01-04

---
##  Quick start

The Inglenook Sidings shunting puzzle uses the following track
configuration with fixed track capacities. Note how the switching
lead track (head shunt) is limited to the engine plus three cars.
<pre>
                                    /------------- 3
                                   /    3 cars
     West                         /
                                 /----------------  2     East
         Switching Lead         /       3 cars
         (Head Shunt)          /
    0  -----------------------/---------------------  1
         Engine + 3 cars                5 cars

</pre>

For input and output the tracks are represented by lists containing
the occupying cars in the order from west to east.

The cars are represented by any integers between 1 and 15.  The engine
is always represented by zero.  There must be a maximum of eight cars.

The engine is assumed to be on the west end of all movements.  The
engine must be on the west end of track 0 in the start and end
conditions (states).

To run this program interactively, start a Standard ML session and
at the prompt enter (note the semicolon at the end):

  `use "inglenook-sidings-full-v6.sml";`

This assumes you are in the folder/directory where the code file resides.
If not, you must include the path to the file in the double quotes.

If there are no errors you will get another prompt after a listing of
the program components.  (There may be a warning that can be ignored.)
Run a test by entering:

  `generate_problem(1);`

You should get a printout showing

  1.  The "start state" at the beginning of the puzzle with the engine
      "0" on the switching lead (track 0), five cars on track 1,
      three cars on track 2, and no cars on track 3.  The cars are
      in a random order.

  2.  The number of engine moves (pulls or drops) to the "end state".

  3.  A list of the intermediate states of the "solution path" which
      shows the car positions between the engine moves.  The cars in
      the end state are in numerical order, five on track 1 and three
      on track 2.

To exit Standard ML enter `<cntl>+d` (hold down the control key and hit
the "d" key).

---
##  Standard problems

At the start there are five cars on track 1 representing an arriving
train with the engine placed on the switching lead (head shunt),
track 0.  There are also three cars on siding track 2, for a total
of eight cars.  We assign the numbers 1 to 8 randomly as labels of
the cars in their starting positions.  At the end the eight cars
are distributed so that track 1 contains cars 1, 2, 3, 4, and 5 in
that order from west to east, and similarly cars 6, 7, and 8 are on
track 2 in order from west to east.

    Start:  Track 0: engine
            Track 1: 3, 2, 7, 5, 1
            Track 2: 4, 8, 6
            Track 3: empty
    State:  ([0], [3, 2, 7, 5, 1], [4, 8, 6], [])

    End:    Track 0: engine
            Track 1: 1, 2, 3, 4, 5
            Track 2: 6, 7, 8
            Track 3: empty
    State:  ([0], [1, 2, 3, 4, 5], [6, 7, 8], [])

Note the use of parentheses around the whole state and the use of square
brackets around the lists of cars on each track.  The commas between and
within the track lists are mandatory, but the spaces are not.

The following entry at the prompt finds a solution to this problem:

  `solve(([0],[3,2,7,5,1],[4,8,6],[]),([0],[1,2,3,4,5],[6,7,8],[]));`

The output will be abbreviated and inconvenient to read, so
immediately after the last entry, type in the following at the next
prompt:

  `print_solution(it);`

("it" is the generic name for the output of the previously run
function/command).  You should get the output:

```
  Moves: 14
  Solution:
  ([0],[3,2,7,5,1],[4,8,6],[])
  ([0,3,2,7],[5,1],[4,8,6],[])
  ([0,3],[5,1],[4,8,6],[2,7])
  ([0,3,5,1],[],[4,8,6],[2,7])
  ([0,3,5],[],[4,8,6],[1,2,7])
  ([0,3],[5],[4,8,6],[1,2,7])
  ([0,3,4],[5],[8,6],[1,2,7])
  ([0],[3,4,5],[8,6],[1,2,7])
  ([0,1,2],[3,4,5],[8,6],[7])
  ([0],[1,2,3,4,5],[8,6],[7])
  ([0,8,6],[1,2,3,4,5],[],[7])
  ([0,8],[1,2,3,4,5],[],[6,7])
  ([0],[1,2,3,4,5],[8],[6,7])
  ([0,6,7],[1,2,3,4,5],[8],[])
  ([0],[1,2,3,4,5],[6,7,8],[])
```
  For the convenience of the user there are two functions which
  generate one or more standard problems and find the solutions.

    generate_problem(n).  Generates N standard problems and finds a
    solution for each using the predicate solve where n is a positive
    integer.

    generate_problem_file(n, "out_file").  Like generate_problem, but
    writes the solution(s) to a file named out_file.

  Here is an example of generating a single standard problem:
  ```
  generate_problem(1);

  Start state: ([0],[6,2,8,1,4],[7,5,3],[])
  Moves: 18
  Solution:
  ([0],[6,2,8,1,4],[7,5,3],[])
  ([0,6,2,8],[1,4],[7,5,3],[])
  ([0],[1,4],[7,5,3],[6,2,8])
  ([0,7,5],[1,4],[3],[6,2,8])
  ([0,7,5,1],[4],[3],[6,2,8])
  ([0,7],[4],[5,1,3],[6,2,8])
  ([0,7,4],[],[5,1,3],[6,2,8])
  ([0,7,4,5],[],[1,3],[6,2,8])
  ([0,7],[4,5],[1,3],[6,2,8])
  ([0,7,1,3],[4,5],[],[6,2,8])
  ([0],[7,1,3,4,5],[],[6,2,8])
  ([0,6,2,8],[7,1,3,4,5],[],[])
  ([0,6,2],[7,1,3,4,5],[8],[])
  ([0,6,2,7],[1,3,4,5],[8],[])
  ([0,6],[1,3,4,5],[2,7,8],[])
  ([0,6,1],[3,4,5],[2,7,8],[])
  ([0,6,1,2],[3,4,5],[7,8],[])
  ([0,6],[1,2,3,4,5],[7,8],[])
  ([0],[1,2,3,4,5],[6,7,8],[])
```
---
##  Non-standard problems

Non-standard start or end states can be used with the predicate
"solve" as long as the engine is on the west (bumper) end of track 0.
This may be useful if the user models specific industries on the
sidings.  A maximum of eight cars still applies.

Care must be taken to ensure that the number of cars and their labels
are identical in the start and end states and that there are no
duplicate car labels.

Here is a non-standard problem with six cars plus a transfer caboose
"15" (and the engine named "0").  Enter:

  `solve(([0,15],[3,2,7,5],[4,6],[]), ([0],[6,4,15],[2,3],[5,7]));`

  and after the solution is found, enter:

  `print_solution(it);`

  to get these results:
```
  Moves: 17
  Solution:
  ([0,15],[3,2,7,5],[4,6],[])
  ([0,15,3,2],[7,5],[4,6],[])
  ([0],[7,5],[4,6],[15,3,2])
  ([0,7,5],[],[4,6],[15,3,2])
  ([0,7,5,15],[],[4,6],[3,2])
  ([0,7,5],[15],[4,6],[3,2])
  ([0,7,5,4],[15],[6],[3,2])
  ([0,7,5],[4,15],[6],[3,2])
  ([0,7,5,6],[4,15],[],[3,2])
  ([0],[7,5,6,4,15],[],[3,2])
  ([0,7,5],[6,4,15],[],[3,2])
  ([0,7,5,3],[6,4,15],[],[2])
  ([0,7],[6,4,15],[5,3],[2])
  ([0],[6,4,15],[5,3],[7,2])
  ([0,5],[6,4,15],[3],[7,2])
  ([0,5,7,2],[6,4,15],[3],[])
  ([0,5,7],[6,4,15],[2,3],[])
  ([0],[6,4,15],[2,3],[5,7])
```
  This is not the shortest solution possible!  See the notes below.

---
##  Notes on solutions

Most of the time, the solutions found by the function "solve" are not
the shortest possible.  This is because finding a minimal length
solution with the base approach used here in most cases would take an
unacceptably long time (possibly a few hours!).  For this reason the
"solve" function uses a "trick."  It breaks the solution
into two pieces connected by an intermediate state.  This intermediate
state is chosen so the combined solution is of reasonable length and
takes at worst a few minutes to calculate.

Sometimes a shorter solution can be found by interchanging the start
and end states.  Since all the engine moves can be reversed, one can
just reverse the order of this solution to find a solution to the
original problem.  For example, taking the non-standard problem above
and reversing the start and end states we enter:

  `solve(([0],[6,4,15],[2,3],[5,7]), ([0,15],[3,2,7,5],[4,6],[]));`

followed by:

  `print_solution(it);`

resulting in the output:
```
  Moves: 14
  Solution:
  ([0],[6,4,15],[2,3],[5,7])
  ([0,2],[6,4,15],[3],[5,7])
  ([0],[6,4,15],[3],[2,5,7])
  ([0,6,4,15],[],[3],[2,5,7])
  ([0,6],[],[4,15,3],[2,5,7])
  ([0,6,2,5],[],[4,15,3],[7])
  ([0,6,2],[5],[4,15,3],[7])
  ([0,6,2,7],[5],[4,15,3],[])
  ([0],[6,2,7,5],[4,15,3],[])
  ([0,4,15,3],[6,2,7,5],[],[])
  ([0,4],[6,2,7,5],[],[15,3])
  ([0,4,6],[2,7,5],[],[15,3])
  ([0],[2,7,5],[4,6],[15,3])
  ([0,15,3],[2,7,5],[4,6],[])
  ([0,15],[3,2,7,5],[4,6],[])
```
This is three moves shorter than the first solution found, although
typically the "reverse" solution is not necessarily shorter, let
alone three moves shorter.

An interested user can try to find an optimal minimal length solution
by using the "solve_pure" function, preferably on a problem
with a known solution of 12 or fewer moves.  Here is an example:

  `solve_pure(([0],[6,7,1,5,2],[3,4,8],[]),([0],[1,2,3,4,5],[6,7,8],[]));`

followed by:

  `print_solution(it);`

gives the output:
```
  Moves: 11
  Solution:
  ([0],[6,7,1,5,2],[3,4,8],[])
  ([0,6,7],[1,5,2],[3,4,8],[])
  ([0],[1,5,2],[3,4,8],[6,7])
  ([0,1,5,2],[],[3,4,8],[6,7])
  ([0,1,5],[],[3,4,8],[2,6,7])
  ([0,1],[5],[3,4,8],[2,6,7])
  ([0,1,3,4],[5],[8],[2,6,7])
  ([0,1],[3,4,5],[8],[2,6,7])
  ([0,1,2],[3,4,5],[8],[6,7])
  ([0],[1,2,3,4,5],[8],[6,7])
  ([0,6,7],[1,2,3,4,5],[8],[])
  ([0],[1,2,3,4,5],[6,7,8],[])
```
No other solution to this problem has fewer moves than this solution.
If we use the solve function on this problem we get a solution which
is just two moves longer:

  `solve(([0],[6,7,1,5,2],[3,4,8],[]),([0],[1,2,3,4,5],[6,7,8],[]));`

  `print_solution(it);`
```
  Moves: 13
  Solution:
  ([0],[6,7,1,5,2],[3,4,8],[])
  ([0,6,7,1],[5,2],[3,4,8],[])
  ([0,6],[5,2],[3,4,8],[7,1])
  ([0,6,5,2],[],[3,4,8],[7,1])
  ([0,6,5],[],[3,4,8],[2,7,1])
  ([0,6],[5],[3,4,8],[2,7,1])
  ([0,6,3,4],[5],[8],[2,7,1])
  ([0],[6,3,4,5],[8],[2,7,1])
  ([0,6],[3,4,5],[8],[2,7,1])
  ([0,6,2],[3,4,5],[8],[7,1])
  ([0,6],[2,3,4,5],[8],[7,1])
  ([0,6,7,1],[2,3,4,5],[8],[])
  ([0,6,7],[1,2,3,4,5],[8],[])
  ([0],[1,2,3,4,5],[6,7,8],[])
```
---
## Original Puzzle Solutions

In the original version of the Inglenook Sidings Puzzle, the final
state is the same as in the standard solutions described above, except
that the positions of the three cars on track 2 are arbitrary.
Only the five cars on track 1 must be in the specified order, and
track 3 and track 0 (except for the engine) must still be empty.

The functions solve_pure_orig and solve_orig use this original version
specification for the final state.  There are also generate_problem_orig
and generate_problem_orig_file functions that follow this puzzle variant.

For example

  `generate_problem_orig(1);`

gives the output
```
  Start state: ([0],[4,6,2,3,1],[8,7,5],[])
  Moves: 15
  Solution:
  ([0],[4,6,2,3,1],[8,7,5],[])
  ([0,4,6],[2,3,1],[8,7,5],[])
  ([0],[2,3,1],[8,7,5],[4,6])
  ([0,8,7,5],[2,3,1],[],[4,6])
  ([0,8,7],[2,3,1],[],[5,4,6])
  ([0],[2,3,1],[8,7],[5,4,6])
  ([0,2,3,1],[],[8,7],[5,4,6])
  ([0,2,3],[],[1,8,7],[5,4,6])
  ([0,2,3,5],[],[1,8,7],[4,6])
  ([0,2,3],[5],[1,8,7],[4,6])
  ([0,2,3,4],[5],[1,8,7],[6])
  ([0],[2,3,4,5],[1,8,7],[6])
  ([0,1,8],[2,3,4,5],[7],[6])
  ([0,1,8,6],[2,3,4,5],[7],[])
  ([0,1],[2,3,4,5],[8,6,7],[])
  ([0],[1,2,3,4,5],[8,6,7],[])
```
Note that in the final state the cars on track 2 are not in order
in this original version.  Using this version will generally result in
shorter solutions.

---
## Development Notes

Version 6 has added functions to solve the original version of the
puzzle and revises some comments.

Version 5 has some simple refactoring of functions that make use of a
helper function, making the helper functions local.  Also, the function
write_perms_file has been added to create a file of all permutations
of `[1,2,3,4,5,6,7,8]`, subsets of which can be used in conjunction with
batch processing of solutions.

Version 4b is the same as version 4 except for added functions which
read permutations of `[1,2,3,4,5,6,7,8]` from a file and print the
number of moves in a solution in another file.

In version 4 the internal representation of puzzle states has been
changed to a single 64-bit word.  The previous state data structure
is retained for input and output, named "iostate".  This change
noticably increased the speed of the iterative deepening depth-first
search for solutions.

In version 3 the function generate_problem_file has been added.

Functions in version 2 have been modified from version 1 so that
printing is separated from the solve functions.

Version 1 was based on a Prolog program and used integer lists to
represent puzzle states.

