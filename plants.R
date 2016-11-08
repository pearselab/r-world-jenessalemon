#Parameters
rep <- c(0.3, 0.7, 0.4)
sur <- c(0.4, 0.6, 0.3)
comp.matrix <- matrix(data = NA, nrow=3, ncol=3)
comp.matrix[1, 1] <- .2
comp.matrix[1, 2] <- .9
comp.matrix[1, 3] <- .4
comp.matrix[2, 1] <- .5
comp.matrix[2, 2] <- .1
comp.matrix[2, 3] <- .3
comp.matrix[3, 1] <- .6
comp.matrix[3, 2] <- .8
comp.matrix[3, 3] <- .7
comp.matrix #just a check
names <- c(shockley, soredium, longilobum)


setup.plants <- function(repro, survive, comp.mat, names = NULL){
  if(is.null(names)){
    names <- letters[seq_along(repro)] #is this just assigning names a, b, c, if it doesn't already have a name?
  }
  if(length(repro) != length(survive)){
    stop("Reproduction and survival parameters needed for all species!")
  }
  if(nrow(comp.mat) != ncol(comp.mat)){
    stop("Please enter a square matrix")
  }
  if(ncol(comp.mat) != length(repro)){
    "Competition and reproduction values needed for all species"
  }
  repro <- setNames(repro, names)
  return(list(repro=repro, survive=survive, comp.mat=comp.mat, names=names))
}
setup.plants(rep, sur, comp.matrix)

survive <- function(cell, info){
  if(is.na(cell) || cell == " "){
    #do something
  }
  if(runif(1) <= info$survive[plant]){
    #i think i need to store the plant in the cell
  }
}


















