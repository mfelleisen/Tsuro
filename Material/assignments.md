## ideas for assignments on Tsuro 

### Exploring Your Favorite Language 

A. Write a memo on your favorite language/IDE specification. 

B. A program that reacts to a command-line flag and reads to STDIN and STDOUT.

C. A program that reads a series of JSON expressions from STDIN and writes
some others to STDOUT. 

Specify a 'server module' with the following functionality: 
 - create from a JSON configuration of a directed, simple graph with named
   nodes and edges  
 - adds distinctly colored tokens to named nodes 
 - queries whether a token of a specific color can reach a named node
   Note that a graph may allow circular paths and the token may therefore
   never reach the destination. 

D. Implement the given server-module specification. 

Develop a 'client module' that instantiates a 'server' from the given graph
specification, adds a red and a blue token, and queries whether the red
token can reach the "blue" node and the blue token can reach the "red"
one. 

E. Adapt the server and client module to TCP. 

### The Tsuro Game 

1. Develop a data representation for tiles. 

   - Include an operation to render a configuration graphically 
     (ASCII art or an image).
   
   - Develop a function that computes the 35 distinct configurations from
     first principles. 

2. Develop a data representation for game states, including the following
three operations:

   - one that creates a state from a bunch of initial tile & player placements; 

     The operation should reject placements that result in boards where
     tiles neighbor other tiles, tiles not placed near the periphery, and
     tiles where the avatar occupies a port at the periphery of the board.

   - one that executes a turn for a specific player, that is, adds a tile
     to the board of a given state on behalf of a player whose avatar still
     occupies a place on the board.

   - one that allows access to the "surviving" player avatars. 

Design the interface to a Tsuro player component. 

3. Develop a Tsuro player that randomly chooses one of the currently legal
actions. A player comes with a name and two game operations:  

   - one that computes an initial placement from three given tiles 
   - one that computes a turn from the current game state and two tiles

Develop a test harness for a Tsuro game state. It consumes a game-state
test input from STDIN and returns the test output on STDOUT. 

Develop three game-state tests. 

Design the interface to a Tsuro referee component. 

### Deterministic Player 

- play first tile received for every move

4. Develop a referee that accepts between three and six Tsuro players and
runs a complete game of Tsuro. The players are handed over ordered by
decreasing age. The referee returns the names of the players paired with
their ranking.

Develop a test harness for a Tsuro player. It consumes a test input from
STDIN and returns the test output on STDOUT.

Design the interface for a Tsuro tournament administrator. 

### Deterministic Referee 

- choose tiles in the ordering specified 

5. Add 'dragon tiles' to Tsuro game. 

Develop a test harness for a Tsuro referee. It consumes a natural number
between three and six (inclusive), creates that many deterministic players,
and runs a Tsuro game. It produces a game ranking. 

6. Develop a Tsuro tournament administrator. It accepts an arbitrary number
of players, adds two deterministic players, and runs a tournament. 

### Tournament Rule 

- run games with six players and one with leftover players 
- pick two best players from each game for next round 
- when there's only one game to be played, it is the last one and
  determines the winners of the tournament

7. Turn the whole thing into a distributed game. 



