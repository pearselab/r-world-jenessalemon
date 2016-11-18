install.packages('roxygen2')
library(roxygen2)
roxygenize("/Users/jimblotter/Desktop/Grad School/Programming_for_Biologists/r-world-jenessalemon/RWorld")

#' Creates a variable terrain from a matrix of given dimensions
#' @param dimen stands for dimensions, which represent the length of the square sides of the matrix.
#' @author Jenessa Lemon
#' @examples setup.matrix(9)
#' @return a matrix of given dimensions, with the corner values filled in.
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

#' Based off of the dimensions of the matrix, this function finds the four corners of the square matrix, and then finds the middle of the matrix, assigning it a value.
#' @param the matrix created from the setup.matrix above should be passed in here.
#' @author Jenessa Lemon
#' @examples diamond.step(setup)
#' @return return the same matrix from the output of setup.matrix, with the center value now filled in.
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

#' This function uses the dimensions of a matrix to find and assign values to certain target cells.
#' @param the matrix returned by the diamond.step function above should be passed in here.
#' @author Jenessa Lemon
#' @examples square.step(pre.terrain)
#' @return the matrix from the output of diamon.step, but now the square step is completed.
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

#' This function uses the dimensions of a matrix to find and assign values to certain target cells.
#' @param a matrix returned by setup.matrix function above should be passed in here.
#' @author Jenessa Lemon
#' @examples diamond.square.step(terrain)
#' @return a matrix, which represents a "slice" in time, to be added to the array in the following function.
#' @export
diamond.square.step <- function(dimens){
  matrix1 <- setup.matrix(dimens)
  matrix2 <- diamond.step(matrix1)
  mat <- square.step(matrix2)
  for (i in 2^(dimens:1)){ #cuts dimens in half sequentially, for a 9x9 matrix: 512 256 128  64  32  16   8   4   2
    for (j in seq(1, (ncol(mat)-i), by=i)){ #looping through columns
      for (k in seq(1, (nrow(mat)-i), by=i)){ #looping through rows
        mat[k:(k+i),j:(j+i)] <- diamond.step(mat[k:(k+i),j:(j+i)]) #first diamond step
        mat[k:(k+i),j:(j+i)] <- square.step(mat[k:(k+i),j:(j+i)]) #then square step
      }
    }
  } #closes for loop
} #closes function

#' This function is a wrapper that pulls the above functions together.
#' @param a matrix returned by setup.matrix function above should be passed in here.
#' @author Jenessa Lemon
#' @examples diamond.square.step(terrain)
#' @return the final product- the terrain!
#' @export
make.terrain <- function(dimens){
  diamond.square.step(dimens)     #...well that was pointless
}
