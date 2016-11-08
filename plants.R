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
info <- setup.plants(rep, sur, comp.matrix) #calling to see if it works and saving it as info for later.
info

char.matrix <- matrix(data = NA, nrow=3, ncol=3)
char.matrix[1, 1] <- " "
char.matrix[1, 2] <- " "
char.matrix[1, 3] <- " "
char.matrix[2, 1] <- " "
char.matrix[2, 2] <- " "
char.matrix[2, 3] <- " "
char.matrix[3, 1] <- " "
char.matrix[3, 2] <- " "
char.matrix[3, 3] <- " "
char.matrix #just a check

plant.timestep(plants, terrain, info){
  survive <- function(cell, info){
    if(is.na(cell) || cell == " "){
      break #not sure if I am using break right
   }
    if(runif(1) <= info$survive[plant]){ #we use runif to draw a random number from a uniform distribution. We then compare this random draw to the probability of the other plant surviving, and the one with the highest probability wins. This makes sense because we want the plant to have a random chance of surviving.
      cell[i] <- info[i] #not sure if i am supposed to be using info here
    }
  }
  for (i in plants){
    plants[i] <- survive(plants[i]) #not sure if this is right or if I need "plants[i] <-"
  }
  return(new.plants.matrix)
}
plant.timestep(char.matrix, terrain, info) #calling to see if it works

run.plant.ecosystem <- function(){
  plants <- array("", dim=c(terrain), timesteps+1)) #why timesteps +1?
  for(i in seq_len(dim(plants)[3])){
    plants[,,i][is.na(terrain)] <- NA
  }
}



















