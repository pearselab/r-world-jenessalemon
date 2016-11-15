
setup.matrix <- function(dimen){
  if(dimen %% 2 == 0){
    dimen <- dimen+1           #If the number is even add one to make it odd.
  }
  ter.mat <- matrix(nrow = dimen, ncol = dimen) #ensuring a square matrix
  #Four corners
  ter.mat[1,1] <- rnorm(1, rnorm(1), runif(1, min = 0)) #one draw, random mean, random sd, from a uniform distribution beacuse we cant have a negative sd!
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
pre.terrain #just a check

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

#write two functions that expect to be given a square matrix, and will simply carry out each step on that matrix. It will have ‘step through’ your matrix, calling those functions with smaller and smaller chunks of the matrix as the algorithm progresses.
diamond.square.step <- function(dimens){
  matrix1 <- setup.matrix(dimens)
  matrix2 <- diamond.step(matrix1)
  mat <- square.step(matrix2)
  size <- ncol(mat) - 1
  for (i in seq(from=1, to=(ncol(mat)), by=size)){ #I want the by to be cut in half every iteration
    mat[i:i, i:i]<-diamond.step(mat[i:i, i:i])
    mat[i:i, i:i]<-square.step(mat[i:i, i:i])
    for (j in seq(from=1, to=(nrow(mat)), by=size)){
      mat[j:j, j:j]<-diamond.step(mat[j:j, j:j)
      mat[j:j, j:j]<-square.step(mat[j:j, j:j])
    }
  }
}
hey <- diamond.square.step(5)
hey
seq(1, 9, 8)
seq(1, 9, 4)
seq(1, 9, 2)


#In English first:
#I want a function that will loop through a matrix of any size, covering all 
#quadrants and subquadrants and applying diamond.step and square.step.
#The first block of code will make the matrix and run the initial diamond 
#and square steps.
#From then on I am going to use the seq function.
> seq(1, 9, 8)
[1] 1 9
> seq(1, 9, 4)
[1] 1 5 9
> seq(1, 9, 2)
[1] 1 3 5 7 9
#The seq function is cool because by cutting the "by" in half every time,
#it gives us the dimensions for each sub square.
#I need to somehow take these dimensions, and index the matrix from 1:3, 3:5, 5:7, 7:9
#I have no idea how to do that





