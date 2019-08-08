## ideas for assignments on Tsuro 

1. develop a data representation for tiles and port configurations 

   - include an operation to render a configuration graphically 
     (ASCII art or - something else)
   
   - use the representation to determine the 35 distinct configurations; 
     export a set of unique configurations 


2. develop a data representation for the game board 

   - include an operation that creates a board from initial tile placements;
     a tile placement by a player consists of:
     - a border coordinate for a tile 
     - an indication where the player's avatar is located on this tile 
     
     assume the coordinates are distinct and that these tiles do not touch;
     do not worry how we get these placements 

   - include an operation for placing adding tile to the board on behalf of
     a player 


3. develop an algorithm that determines whether a tile addition is legal 

4. develop a random player and a referee 


