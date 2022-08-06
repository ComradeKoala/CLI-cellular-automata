# Cellular automata simulatior 


### Descrition
A [cellular automata](https://en.wikipedia.org/wiki/Cellular_automaton) simulator that runs in a CLI, implemented in Haskell.
Originily developed as a compuslory exersie for {UIB's course on Functional Programing](https://www.uib.no/en/course/INF122) for the fall of 2020.


### Features
- Customisable ruleset, the deafult ruleset is set to [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)
- Iterating over genreations step by step
- Livemode that let's you iterate over n genration
- up to 99 x 99 sized field.


## Prerequisites
- [Haskell](https://www.haskell.org/downloads/)


### How to run
1. Make sure you have Haskell installed on your device, including GHCI.
2. Clone the reposetory using git
3. Open GHCI using your prefered CLI.
4. Load Main.hs using ```:load <filename>```
5. Run the main action by typing: ```main``` and pressing enter


### Commands
`c n` – create and show a new, empty board of size n × n.
(n :: Int and 1 ≤ n ≤ 99)

`n x1 y1 ... xk yk` – place k living cells at (x1, y1), ...,(xk, yk).
(xi, yi :: Int and 1 ≤ xi, yi ≤ n, for 1 ≤ i ≤ k, where n is the size of the grid)

`e x1 y1 ... xk yk` – make positions (x1, y1), ...,(xk, yk) empty.
(xi, yi :: Int and 1 ≤ xi , yi ≤ n, for 1 ≤ i ≤ k, where n is the size of the grid)

`b m n` – redefine the automaton so that an empty cell with [m..n] living neighbours becomes alive.
(m, n :: Int, 0 ≤ m ≤ n)

`s m n` – redefine the automaton so that a living cell with [m..n] living neighbours survives.
(m, n :: Int, 0 ≤ m ≤ n)

`?` – show the rules of the current game, i.e. the pairs s x0 y0 and b x1 y1. Pick an appropriate
place to show these, so that they do not overwrite the grid.

`w` – show the user the positions of all living cells currently on the board. That is, pick an appropriate place, not overwriting the board, and show the positions as pairs of coordinates, each separated by comma inside parentheses, e.g. (3, 2), (5, 5), (1, 4), . . .

~~`r name` – read the rules and board from the file name~~ TODO

`‘Enter’` – i.e., pressing Enter/Return button with empty input, progresses one generation. It tells
the user if a stable configuration is reached, i.e. a generation identical to its next (or previous)
generation.

`l x` – enter a “live” mode, i.e. the game is played without user interaction, showing the changes
on the board, for x generations OR until a stable configuration is reached before the xth
generation, which is then announced to the user. It is important that the changes on the
board are visible, i.e. do not goo too fast nor too slow.
(x :: Int and x ≥ 1)

`quit` – quits the program.


### TODO:
- Refactor code for readbility
- Implement abiltiy to load custom rules from a file
- Add list of commands avilable
