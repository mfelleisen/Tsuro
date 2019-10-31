## Code for a Variant of Tsuro 

This directory implements a distributed Tsuro game

_Static Organization_ 

There are three current sub-directories implementing the logic of playing a
Tsuro tournament. 

- `Common` implements the pieces through which `Players` and `Admin` communicate. 
  It should contain at a minimum the following files: 
  - `board` is a data representation for the game state 
  - `player-interface` is a player interface for the Admin components to call players
  - `rules` implements the rule checker 
  - `actions` is a data representation/definition for the actions a player may take 
  - `tiles` is a data representation for game tiles 
- `Players` implements mechanical aka AI players It  should contain at a minimum three files: 
  - `player` which implements the mechanics of playing automatically 
  - `first-s` which implements the first (silly) deterministic strategy 
  - `second-s` , the second (mostly legal) deterministic strategy 
- `Admin` implements mechanical game and tournament administrators. It should contain two files: 
  - `referee` which implements a mechanical referee for a single game 
  - `administrator` which implements a mechanical tournament administrator 

_Dynamic Organization_ 

The tournament `administrator` is invoked on a list of `players`, ordered
by age (descending). For each game to be run, the administrator creates a
referee, hands over the players that participate in this game, and expects
a result back. The `referee` interacts with the `players` according to the
local protocol specified by the software architect. The data format of
these exchanges is specified via the contracts in `board`, `action`,
`rules`, and `player-interacte`.  After each round of games the tournament
`administrator` re-organizes games for the next one, with the top finishers
as specified, until there is only one game left.  This last game determines
the outcome. Following the local protocol, the `administrator` informs each
of the players (that did not "cheat") of the outcome as to whether this AI
player won or lost. 

### PLAN 

[x] can initials/intermediate come with more than one tile for same player?

[x] tiles, rendering tiles 

[x] unique tile configs 

[x] map of tiles with:
  - initialize 
  - adding a new one (no legality check) 
  - moving a token to end of path 

[x] JSON de/serialization of map 	  

[x] batch test harness for board 
  - get all players thrown out? 
  
[X] legal move: suicide, circular paths, no more than one token per port

[X] the player 

[X] strategy 

[x] the referee 

[1/2] observers, which level? 

[x] the game administrator 

[ ] going distributed 
    

