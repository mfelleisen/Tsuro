## Bugs Recorded 

### Ordinary Bugs, detected after development 

1. in Common/board 
   - `(matrix-set m y x _)` causes a bug in initialize; 
   - I wrote the test to the function;
   - rendering it on-screen for `add-tile` uncovered it 

2. in Common/matrix
   - matrix-where used the coordinates in the wrong order (no tests!!!)

### Design Bugs 

1. equality for tiles was flawed 
   - tiles are equal if their maps are equal
   - under _different circumstances_ tiles are also equal when rotated
