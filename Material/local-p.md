## The Local Interaction Protocol (Monolithic) 

The interaction between Racket players and the Racket _Admin_ framework is
governed by the set of following diagrams. 

### Tournament Set-up 

```
   manager <-------------- player (p1)  . . . player (pn)
     |                        |        |      |
     |                        |        |      |
     |------------------------|--------|----->|   playing-as(string) % in case of name clash 
     |                        |        |      |
     |------------------------|------->|      |   playing-as(string) % in case of name clash 
     .                        .               .
     |                        |               |
     |               referee  |               |
     |--new(plrs1)-------+    |               |
     |                   |    |               |
     .                   .    .               .   an encounter among players in plrs1
     |<================= |    |               |   result: ???
     |                   _    |               |
     |                        |               |
     .			      .		      .   many games 
     |               referee  |               |
     |--new(plrs2)-------+    |               |
     |                   |    |               |
     .                   .    .               .   an encounter among players in plrs2
     |<================= |    |               |   result: ???
     |                   _    |               |
     |                        |               |
     | ---------------------> |               |   end-of-tournament(results)
     |                        |               |
     .                        .               .
     | -------------------------------------> |   end-of-tournament(results)
     .                        .               .   for all surviving players 
     |                        |               |
```

Terminated players no longer compete and their past games are re-evaluated. See _Admin_ for policy
and its implementation.  


### A Referee Interaction 

``` 
  referee             player: P-1 ... ... player: P-N
     |                   |                    |
     |-----------------> |                    |   other-players (strings)
     |                   |                    |    % the colors of the other players,
     |                   |                    |    % which is also the name of its workers
     |--------------------------------------> |   other-players (strings)
     |                   |                    |
     |-----------------> |                    |   initial(initial-placements)
     | <================ |                    |   tile port 
     . 			 .		      .	  
     |--------------------------------------> |   initial(initial-placements)
     | <===================================== |   tile port 
     . 			 .		      .	  
     |                   |                    |
     |-----------------> |                    |   take-turn(intermediate-placements)
     | <================ |                    |   tile port place
     . 			 .		      .	  
     | -------------------------------------> |   take-turn(intermediate-placements)
     | <===================================== |   tile port place 
     |                   |                    |
     .                   .                    .
```

An interaction between the referee and a player is terminated if
 - the player breaks the rules (which concerns ``business logic'')
 - the player raises an exception (due to an internal error).

At this stage, we will not worry about 
 - players that cheat by exploiting the monolithic setting 
 - players that take too long for an interaction (and are possibly in an infinite loop).
Why? 


