roxygenize("/Users/jimblotter/Desktop/Grad School/Programming_for_Biologists/r-world-jenessalemon")
#' Creates a variable terrain from a matrix of given dimensions
#' @param dimen stants for dimensions, which represent the length of the square sides of the matrix.
#' @author Jenessa Lemon
#' @examples setup.matrix(9)
#' @export
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

#' Based off of the dimensions of the matrix, this function finds the four corners of the square matrix, and then finds the middle of the matrix, assigning it a value.
#' @param the matrix created from the setup.matrix above should be passed in here.
#' @author Jenessa Lemon
#' @examples diamond.step(setup)
#' @export
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

#' This function uses the dimensions of a matrix to find and assign values to certain target cells.
#' @param the matrix returned by the diamond.step function above should be passed in here.
#' @author Jenessa Lemon
#' @examples square.step(pre.terrain)
#' @export
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

#' This function uses the dimensions of a matrix to find and assign values to certain target cells.
#' @param a matrix returned by setup.matrix function above should be passed in here.
#' @author Jenessa Lemon
#' @examples diamond.square.step(terrain)
#' @export
#write two functions that expect to be given a square matrix, and will simply carry out each step on that matrix. It will have ‘step through’ your matrix, calling those functions with smaller and smaller chunks of the matrix as the algorithm progresses.
#ATTEMPT 1
diamond.square.step <- function(dimens){
  matrix1 <- setup.matrix(dimens)
  matrix2 <- diamond.step(matrix1)
  mat <- square.step(matrix2)
  size <- ncol(mat) - 1
  for (i in 1:who knows){
    size <- seq(1, size, by = (i-1)/2)
  }
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

#ATTEMPT 2

  for (i in seq(1, ncol, ncol-1)){     #I need ncol-1 to be cut in half sequentially
    top <- ncol(duck.matrix)
    mid <- (top + 1)/2
    duck.matrix[1:mid, 1:mid] <- diamond.step(duck.matrix[1:mid, 1:mid])
    duck.matrix[1:mid, 1:mid] <- square.step(duck.matrix[1:mid, 1:mid])
    duck.matrix[mid:top, 1:mid] <- diamond.step(duck.matrix[mid:top, 1:mid])
    duck.matrix[mid:top, 1:mid] <- square.step(duck.matrix[mid:top, 1:mid)
    duck.matrix[mid:top,mid:top] <- diamond.step(duck.matrix[mid:top,mid:top])
    duck.matrix[mid:top,mid:top] <- square.step(duck.matrix[mid:top,mid:top])
    duck.matrix[1:mid,mid:top] <- diamond.step(duck.matrix[1:mid,mid:top])
    duck.matrix[1:mid,mid:top] <- square.step(duck.matrix[1:mid,mid:top])
  }
  duck.matrix

#function to predetermine the "top" and how many steps it's going to take
#By Paul Wolf :)
  numb.matrix.rows <- function(n, rows=3){
    if(n > 100){
      return("too many cycles, please enter smaller number")# This certainly prevents making a matrix
      # that is too big - but it should give a better error message at the approporiate spot
    }else{
      for(i in 1:n){
        rows <- rows + (rows-1)
      }
    }
    return(rows)
  }
  # now make matrix numb.matrix.rows x same
  n <- 16 # number of reduction cycles
  print(numb.matrix.rows(n))
  x <- numb.matrix.rows(n)


  # silly having function - it is not quite half
  half <- function(x){
    return((x+1)/2)
  }


  #now build vector of matrix slices
  size.vect <- c(x)
  while(x > 3){
    x <- half(x)
    size.vect <- append(size.vect, x)
  }
  print(size.vect)
