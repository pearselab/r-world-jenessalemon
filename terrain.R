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
terrain <- matrix(data = NA, nrow = 21, ncol = 21)
wd <- ncol
ht <- nrow
diamond.step <- function(matrix){
  terrain[1, nrow] <- rnorm(1, 0, 1)
  terrain[ncol, nrow] <- rnorm(1, 0, 1)
  terrain[ncol, 1] <- rnorm(1, 0, 1)
  terrain[1, 1] <- rnorm(1, 0, 1)
}
diamond.step(terrain)

terrain <- matrix(data = NA, nrow = 9, ncol = 9)
row.vector <- c(1, 5, 9)
col.vector <- c(1, 5, 9)
for (i in row.vector){
  for (j in col.vector){
    if (is.na(terrain[i,j])){
    terrain[i,j] <- rnorm(1, 10, 100)
    }else{
      cat("Already a number there!")
    }
  }
}
print(terrain)  
  
x = sqrt(length of one side -1)
x = 2
2 is the number of iterations we have to go through
start at x and go down to 1
do a diamnond step, then all the square steps
side_lenth = [4,2] tells you how many itreations to go through
half = 2
col.seq(1:size-1 by side_length[i])
row.seq(1:size-1 by side_length[i])
for(do-col in col.seq){
  for(do-row in row.seq)
    m[do-row in row:] # upper left
    m[do-row, do-col +side-length]
}
average all the for in statements, then add noice to
  
  
  
  
  
  
  terrain.matrix <- function(n){
    matrix <- matrix(data=NA, nrow = (2*n)+1, ncol = (2*n)+1)       #define matrix
    matrix[1,1] <- rnorm(1,0,15)                                    #defining 4 corners
    matrix[(2*n)+1,1] <- rnorm(1,0,15)
    matrix[1,(2*n)+1] <- rnorm(1,0,15)
    matrix[(2*n)+1,(2*n)+1] <- rnorm(1,0,15)
    return(matrix)
  }
  terrain.matrix(5)                                                #checking to see if it worked