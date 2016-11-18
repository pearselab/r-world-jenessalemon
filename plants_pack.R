install.packages('roxygen2')
library(roxygen2)
roxygenize("/Users/jimblotter/Desktop/Grad School/Programming_for_Biologists/r-world-jenessalemon")

#' This function checks that both reproduction and survival vectors are the length of the number of species in the simulation. It also checks that that the competition matrix has the dimensions of all of these variables.
#' @param repro is the vector of probabilities that will determine if a plant of a given species will reproduce.
#' @param survive is the vector of probabilities that will determine if a plant of a given species will survive.
#' @param comp.mat is the matrix of competitions created above, and determines which plant will succeed and which will fail should they compete for living space.
#' @param names is a list of the names of the different species (created above.)
#' @author Jenessa Lemon
#' @return an organized list of parameters.
#' @examples setup.plants(rep, sur, comp.matrix, names)
#' @export
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

#' This function runs one timestep of our simulation across the whole simulated ecosystem.
#' @param plants is the char.matrix from above
#' @param terrain is the terrain generated in terrain.R
#' @param info is the output from the setup.plants function above
#' @author Jenessa Lemon
#' @examples plant.timestep(char.matrix, terrain, info)
#' @export
plant.timestep <- function(plants, terrain, info){
  survive <- function(terrain, info){
    for (i in 1:ncol(terrain)){
      for (j in 1:nrow(terrain)){
        if(is.na(terrain[i,j])){ #water should stay water
          return(NA)
        }
        #if(terrain[i,j] != ' '){
        # return(cell)  #if occupied, compete!
        #compete
      }
      if(terrain[i,j] == ' '){ #Then we're actually going to see if the plants survives
        if(runif(1) >= info$survive[]){ #If a random draw from a uniform distribution is higher than the survival probability,
          return(' ') #Don't change anything
        }else{
          terrain[i,j] <-     #I DONT UNDERSTAND WHY ON GOD'S GREEN EARTH THIS WOULD RETURN A VECTOR. The entire point is that the plant survived, so it needs to be there.
        }
      }
    }
  }
  for (i in plants){
    slice <- plant.timestep(terrain)
    return(slice) #edited plant matrix
  }
}

#' Takes a terrain from the user and runs our plant ecosystem.
#' @param terrain is the matrix returned by terrain.R
#' @author Jenessa Lemon
#' @examples run.plant.ecosystem(terrain)
#' @return the plant ecosystem
#' @export
run.plant.ecosystem <- function(terrain){
  plants <- array("", dim=c(terrain), timesteps+1) #why timesteps +1?
  for(i in seq_len(dim(plants)[3])){
    plants[,,i][is.na(terrain)] <- NA
  }
}
