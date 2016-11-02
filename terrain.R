'''Diamond Step Algorithm:
  1. Make a square matrix with odd dimensions (table?)
  2. Pick starting heights for each colorConverter
  3. Repeat: (do this with all sub squares until all are filled.)
      a. Across all squares within matrix
      b. Diamond step- found corners, found their center, made it equal to the average of the four surrounding things
      c. Square-step- top middle is equal to the average of the right left corner and middle.
  STOP when matrix is filled. 
  
How to do this?
1. Write a square step ... square.step(matrix) Find midpoint add rmorn to add noise?
2. Write a diamond step ... diamond.step(matrix)
3. Write a wrapper ... terrain(matrix){
                        for(i in steps){
                        matrix[...,...] <- ...     '''
############################################################################################################


 
