## Bugs Recorded 

### Ordinary Bugs, detected after development 

1. in Common/board 
   - `(matrix-set m y x _)` causes a bug in initialize; 
   - I wrote the test to the function;
   - rendering it on-screen for `add-tile` uncovered it 
   
2. in Common/matrix
   - matrix-where used the coordinates in the wrong order (no tests!!!)

3. in Common/board 
   - move player computed the wrong "infinite loop" 
   - because it did not keep track of the port in addition to (x,y) 
   - adding a test that moved a player 3 times accidentally discovered it

4. in Common/board 
   - no-neighbors? removed a location from a list of tile specs
   - TYPES would have caught this one 

5. in Common/board and Common/grid 
   - I had convinced myself that at most one player could be on a square: WRONG 
   - discovered while adding a feature to state->pict for showing multiple players on one tile
   - this also affected the JSON representation of intermediate boards, and its contracts

6. in Common/grid 
   - free-for-init had negated its logic (not equal to BLANK instead of equal to BLANK)
   - I have no clue how test cases ever passed 
   - discovered while creating the right function for initial place finder 
   - not a type problem 

7. in Common/board 
   - it turns out that players can show up on the same port and tile
   - so allow intermediates to come with this too 

8. in Common/board 
   - the find-avatar function should have, but did not, return the board 

   conceptual bug: everything but the port is guaranteed to be a JSexpr 
   this breaks "religious abstraction"

9. in Common/grid
   - at-periphery was off-by-one on indicies of positive periphery

10. in Common/rules 
    - forgot the third tile as a possibility for initial placements    

11. in Common/board 
    - design bug: the infinite state must list all "looping players" not just one

12. in Common/board 
    - the "at periphery or 2 neightbors" condition on intermediates is too strong
    - BUT I do not yet know what condition to replace it with 

13. in Common/rules 
			-----------------------------------------------------------------------------
    - the all-suicide function did not check whether the result of (add-tile ..) was a state, 
      as opposed to infinite or collided (which just aren't suicidal)

### Design Bugs 

1. equality for tiles was flawed 
   - tiles are equal if their maps are equal
   - under _different circumstances_ tiles are also equal when rotated
