## the 4500 rules of Tsuro 

### Definitions 

A _tile_ is a square with 8 _ports_, 2 per side. The four sides are called
_north_, _east_, _south_, and _west_. Each tile specifies 4 distinct
connections between two distinct ports; every port must have exactly one
connection. 

During a game of Tsuro, a player may rotate a tile by 90, 180, or 270
degrees before placing it on the board. 

Two tiles are _equivalent_ if one can be transformed into the other via a
rotation.

A _game board_ consists of 10 by 10 squares where tiles may be placed.

A _player avatar_ is a token that represents a player. 

A tile is _occupied_ when an avatar is place near one of its ports.

### The Goal of the Game 

The goal of a round of Tsuro is to be the player with the last avatar on
the game board. 

### Starting the Game 

The board is initially empty. 

Starting with the oldest one and continuing from there by decreasing age,
each player receives three tiles, chooses one, adds an avatar to one of the
tile's ports, and places this configuration on some square of the board.
The tile must touch the periphery of the board on at least one side and
empty squares on the others. The occupied port must face an empty board
square.

As soon as a player adds an initial tile to the board, the other players
find out where this tile has been placed. 

This start-up round ends when all players have places their one tile and
their avatar on the board. 

### Playing One Round 

Every player receives two tiles from the game referee. 

Starting with the oldest one, each player takes a turn. The player chooses
one of the two give tiles and places it next to the one occupied by the
player's avatar. This action connects the avatar port on the currently
occupied tile to another port on the new tile and, perhaps transitively, to
already placed tiles that border the new tile.  The avatar is moved forward
as far as possible until

 - it gets to a port on a tile that borders an empty board square 
 - it reaches the periphery of the game board, in which case it---and
   its owner---is eliminated from the game. 

The other tile is discarded. 

The round continues with next-oldest player.  The round ends when every
player has completed a turn. 

### Ending a Game 

A game ends when 

 - there is only one avatar left on the board; the avatar's owner is the winner 
 - the last remaining avatars reach the board's periphery during the same round;
   the owners of these avatars are joint winners. 
