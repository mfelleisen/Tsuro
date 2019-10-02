## Code for a Variant of Tsuro 

This directory implements a distributed Tsuro game

### PLAN 

[ ] can intermediate describe islands of tiles without an avatar on them?

[ ] can initials/intermediate come with more than one tile for same player?

[x] tiles, rendering tiles 

[x] unique tile configs 

[x] map of tiles with:
  - initialize 
  - adding a new one (no legality check) 
  - moving a token to end of path 

[x] JSON de/serialization of map 	  

[x] batch test harness for board 
  - get all players thrown out? 
  
[ ] legal move: suicide, circular paths, no more than one token per port

[ ] the player 
  - is placing a tile in some orientation a "good thing" 

[ ] strategy 

[ ] the referee 

[ ] observers, which level? 

[ ] change ports 

[ ] the game administrator 

[ ] going distributed 
    

