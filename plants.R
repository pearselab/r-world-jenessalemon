repro <- c(0.3, 0.7, 0.4)
survive <- c(0.4, 0.6, 0.3)
comp.mat <- matrix(data = NA, nrow=3, ncol=3)
comp.mat[1, 1] <- 2
comp.mat[1, 2] <- 5
comp.mat[1, 3] <- 4
comp.mat[2, 1] <- 5
comp.mat[2, 2] <- 1
comp.mat[2, 3] <- 3
comp.mat[3, 1] <- 6
comp.mat[3, 2] <- 8
comp.mat[3, 3] <- 7
comp.mat #just a check
names <- c(shockley, soredium, longilobum)


setup.plants <- function(repro, survive, comp.mat, names = NULL){
  if(is.null(names)){
    names <- letters[seq_along(repro)] #is this just assigning names a, b, c, if it doesn't already have a name?
  }
  if(length(repro) != length(survive){
    stop("Reproduction and survival parameters needed for all species!")
  }
  #more tests...
  repro <- setNames(repro, names)
  return(list(repro=repro, survive=survive, comp.mat=comp.mat, names=names))
}
