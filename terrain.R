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
setup.matrix <- function(dimensions){
  if(x %% 2 == 0){
    x <- x+1           #If the number is even add one to make it odd.
  }
  ter.mat <- matrix(nrow = dimensions, ncol = dimensions) #ensuring a square matrix
  #Four corners
  ter.mat[1,1] <- rnorm(1, rnorm(1), runif(1, min = 0)) #one draw, random mean, random sd, from a uniform distribution beacuse we can't have a negative sd!
  ter.mat[1, ncol(ter.mat)] <- rnorm(1, rnorm(1), runif(1, min = 0))
  ter.mat[nrow(ter.mat), 1] <- rnorm(1, rnorm(1), runif(1, min = 0))
  ter.mat[nrow(ter.mat), ncol(ter.mat)] <- rnorm(1, rnorm(1), runif(1, min = 0))
  return(ter.mat)
}
setup <- setup.matrix(5)
#setup  #just a check

diamond.step <- function(matrix){
  topL <- matrix[1,1]
  topR <- matrix[1, ncol(matrix)]
  bottomL <- matrix[nrow(matrix), 1]
  bottomR <- matrix[nrow(matrix), ncol(matrix)]
  #find center
  center <- jitter(mean(topL, topR, bottomL, bottomR))
  #actual diamond step part
  matrix[mean(1:nrow(matrix)),mean(1:ncol(matrix))] <- center
  return(matrix)
}
pre.terrain <- diamond.step(setup)
#pre.terrain #just a check

square.step <- function(matrix){
  topL <- matrix[1,1]
  topR <- matrix[1, ncol(matrix)]
  bottomL <- matrix[nrow(matrix), 1]
  bottomR <- matrix[nrow(matrix), ncol(matrix)]
  center <- matrix[mean(1:nrow(matrix)),mean(1:ncol(matrix))]
  #target cells
  top <- jitter(mean(topL, topR, center))
  bottom <- jitter(mean(bottomL, bottomR, center))
  left <- jitter(mean(topL, bottomL, center))
  right <- jitter(mean(topR, bottomR, center))
  #actual square step
  matrix[1,mean(1:ncol(matrix))] <- top
  matrix[nrow(matrix),mean(1:ncol(matrix))] <- bottom
  matrix[mean(1:nrow(matrix)),1] <- left
  matrix[mean(1:nrow(matrix)),ncol(matrix)] <- right
  return(matrix)
}
terrain <- square.step(pre.terrain)
#terrain #just a check

#The whole burrito
diamond.square.step <- function(dimensions){
  step1 <- function(dimen){
    matrix1 <- setup.matrix(dimensions)
    matrix2 <- diamond.step(matrix1)
    matrix3 <- square.step(matrix2)
    return(matrix3)
  }
  for (however many steps it takes) #probably going to have some tricky subsetting 
    step1(matrix3)
}
fill(5)
#Awesome. Now I need a way to navigate through the matrix

