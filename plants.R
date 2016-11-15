#Parameters
rep <- runif(n=2, min=0, max=1) #runif because we don't want negative probabilities!
rep #just checking

sur <- survive <- runif(n=length(rep), min=0, max=1) #again we don't want negatives, and we want the same length
sur #just checking

comp.list <- runif(n=length(rep)^2, min = 0, max = 1)
comp.list #just checking
comp.matrix <- matrix(comp.list, nrow = length(rep), ncol = length(rep))
comp.matrix #just checking

names <- list("shockleyi", "soredium") #does it matter if we use a list or a vector?

char.matrix <- matrix(data=NA, nrow = length(rep), ncol = length(rep)) #initializing a matrix of appropriate size.
char.matrix #just checking

#' This function checks that both reproduction and survival vectors are the lenght of the number of species in the simulation. It also checks that that the competition matrix has the dimensions of all of these variables.
#' @param repro is the vector of probabilities that will determine if a plant of a given species will reproduce.
#' @param survive is the vector of probabilities that will determine if a plant of a given species will survive.
#' @param comp.mat is the matrix of competitions created above, and determines which plant will succeed and which will fail should they compete for living space.
#' @param names is a list of the names of the different species (created above.)
#' @author Jenessa Lemon
#' @examples square.step(pre.terrain)
#' @export setup.plants(rep, sur, comp.matrix, names)
setup.plants <- function(repro, survive, comp.mat, names = NULL){
  if(is.null(names)){
    names <- letters[seq_along(repro)] #this is just assigning names a, b, c, if it doesn't already have a name?
  }
  if(length(repro) != length(names)){
    stop("Reproduction values needed for all species!")
  }
  if(length(survive) != length(names)){
    stop("Survival values needed for all species!")
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
  if (!is.matrix(comp.mat)){
    "Please insert competition probabilities as a matrix"
  }
  survive <- setNames(survive, names) #I also want to assign survival values to the names.
  repro <- setNames(repro, names)
  return(list(repro=repro, survive=survive, comp.mat=comp.mat, names=names))
}
info <- setup.plants(rep, sur, comp.matrix, names) #calling to see if it works and saving it as info for later.
info #just checking

#' This function runs one timestep of our simulation across the whole simulated ecosystem.
#' @param plants is
#' @param terrain is the terrain generated in terrain.R
#' @param info is the output from the setup.plants function above
#' @author Jenessa Lemon
#' @examples plant.timestep(char.matrix, terrain, info)
#' @export
plant.timestep(plants, terrain, info){
  #' This function randomly draws from a uniform distribution to determind whether an individual survives according to our parameters.
  #' @param cell
  #' @param info is the output of the setup.plants function above
  #' @author Jenessa Lemon
  #' @examples square.step(pre.terrain)
  #' @export
  survive <- function(cell, info){
    for(cell in terrain){  #do I need a for loop here?
      if(is.na(terrain[cell]) || terrain[cell] != " "){   #if the cell is water or occupied
      return(plants) #or do I want to return terrain?
      }else{ #if the cell is avaialable for a plant to grow here
        if(runif(n=1, min=0, max=1) <= info$survive[plant]){ #we use runif to draw a random number from a uniform distribution. We then compare this random draw to the probability of the other plant surviving, and the one with the highest probability wins. This makes sense because we want the plant to have a random chance of surviving.
        terrain[cell] <- info[i] #not sure if i am supposed to be using info here
        return(terrain) #supposed to return a modified version of "plants"
        #The plant might reproduce
        }
      }
    } #closes the for loop
  } #closes survive function
  for (i in plants){
    plants[i] <- survive(plants[i]) #not sure if this is right or if I need "plants[i] <-"
  }
  plant <- reproduce(row, column, plants, info) #this comes from 6.3, we are calling our reproduce function
  return(new.plants.matrix) #wouldn't this have to return plants?
}
plant.timestep(char.matrix, terrain, info) #calling to see if it works

#' This function uses the dimensions of a matrix to find and assign values to certain target cells.
#' @param the matrix returned by the diamond.step function above should be passed in here.
#' @author Jenessa Lemon
#' @examples square.step(pre.terrain)
#' @export
###Ok now you are at 6.3!###
reproduce <- function(row, col, plants, info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  #possible.locations
  if(!is.na(possible.locations)){ #filtering out water-logged
    reproduce!
  }
  #being careful to check you do have somewhere to reproduce to! ...???
  if(someone is already in the possible location){
    they are going to compete!
  }
  return(plants)
}

fight <- function(names, 1, prob=comp.mat[row, column]){
}
#' This function uses the dimensions of a matrix to find and assign values to certain target cells.
#' @param the matrix returned by the diamond.step function above should be passed in here.
#' @author Jenessa Lemon
#' @examples square.step(pre.terrain)
#' @export
run.plant.ecosystem <- function(terrain){
  plants <- array("", dim=c(terrain), timesteps+1)) #why timesteps +1?
for(i in seq_len(dim(plants)[3])){
  plants[,,i][is.na(terrain)] <- NA
}
}















