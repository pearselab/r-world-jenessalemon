#Parameters
rep <- runif(n=4, min=0, max=1) #runif because we don't want negative probabilities!
rep #just checking

sur <- survive <- runif(n=length(rep), min=0, max=1) #again we don't want negatives, and we want the same length
sur #just checking

comp.list <- runif(n=length(rep)^2, min = 0, max = 1)
comp.list #just checking
comp.matrix <- matrix(comps, nrow = length(rep), ncol = length(rep))
comp.matrix
#### I manually created a matrix before I saw Carol's cool way to randomly generate one.
#comp.matrix <- matrix(data = NA, nrow=3, ncol=3)
#comp.matrix[1, 1] <- .2
#comp.matrix[1, 2] <- .9
#comp.matrix[1, 3] <- .4
#comp.matrix[2, 1] <- .5
#comp.matrix[2, 2] <- .1
#comp.matrix[2, 3] <- .3
#comp.matrix[3, 1] <- .6
#comp.matrix[3, 2] <- .8
#comp.matrix[3, 3] <- .7
#comp.matrix #just a check

names <- list("shockleyi", "soredium", "longilobum") #does it matter if we use a list or a vector?

char.matrix <- matrix(data=NA, nrow = length(rep), ncol = length(rep)) #initializing a matrix of appropriate size.
char.matrix #just checking

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
  if(any(repro > 1) | any(repro < 0)){
    stop("Invalid probability")
  }
  survive <- setNames(survive, names)
  repro <- setNames(repro, names)
  return(list(repro=repro, survive=survive, comp.mat=comp.mat, names=names))
}
info <- setup.plants(rep, sur, comp.matrix, names) #calling to see if it works and saving it as info for later.
info

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
  plant <- reproduce(row, column, plants, info) #this comes from 6.3, we are calling our reproduce function
  return(new.plants.matrix) #wouldn't this have to return plants?
}
plant.timestep(char.matrix, terrain, info) #calling to see if it works

run.plant.ecosystem <- function(terrain){
  plants <- array("", dim=c(terrain), timesteps+1)) #why timesteps +1?
  for(i in seq_len(dim(plants)[3])){
    plants[,,i][is.na(terrain)] <- NA
  }
}
###Ok now you are at 6.3!###
reproduce <- function(row, col, plants, info){
  possible.locations <- as.matrix(expand.grid(-1,0,1), col+c(-1,0,1)))
  #now filter out which ones are not water-logged and reproduce there...
  #being careful to check you do have somewhere to reproduce to!
  if(is.na){
    
  }
  return(plants)
}


















