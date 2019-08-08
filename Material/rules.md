## the 4500 rules of Tsuro 

### Definitions 

A _tile_ is a square with 8 _ports_, 2 per side. The four sides are called
_north_, _east_, _south_, and _west_. 

A _configuration_ is a tile with 4 connections between two distinct
ports. A configuration is also called a _configured tile_. 

A _tile rotation_ may rotate a configured tile by 90, 180, or 270 degrees. 

Two configured tiles are _equivalent_ if one configuration can be
transformed into the other via a tile rotation. 

A _game board_ consists of 10 by 10 squares where configured tiles may be
placed. 

A _player avatar_ is a token that represents a player. 

An _occupied tile_ is a configured tile on the game board with an avatar on
a port that borders an empty game-board square.

### The Goal of the Game 

The goal of a round of Tsuro is to be the player with the last avatar on
the game board. 

### Starting the Game 

Each player, starting with the oldest one, places one occupied tile on the
empty board. The tiles must border empty squares. As soon as a player adds
an occupied tile on the board, the other players find out where this tile
has been placed. 

### Playing A Turn 

Each player, starting with the oldest player, places a configured tile next
to the one occupied by the player's avatar. This action connects the
avatar's port on the currently occupied tile to another port on the new
tile and, perhaps transitively, to already placed tiles that border the new
tile.  The avatar is moved as far as possible until 

 - it gets to a port on a tile that is on a border to an empty square 
 - it reaches the periphery of the game board, in which case it is eliminated. 

### Scoring a Game 

A game ends when 

 - there is only one avatar left on the board; the avatar's owner is the winner 
 - the last remaining avatars simultaneously reached the periphery;  
   the owners of these avatars are joint winners. 

