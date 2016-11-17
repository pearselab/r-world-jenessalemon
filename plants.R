install.packages('roxygen2')
roxygenize("/Users/jimblotter/Desktop/Grad School/Programming_for_Biologists/r-world-jenessalemon/plants.R")
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

char.matrix <- matrix(data=" ", nrow = length(terrain), ncol = length(terrain)) #initializing a matrix of appropriate size (which is the size of the terrain).
plants <- char.matrix #just checking
plants

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
info <- setup.plants(rep, sur, comp.matrix, names) #calling to see if it works and saving it as info for later.
info #just checking

#' This function runs one timestep of our simulation across the whole simulated ecosystem.
#' @param plants is
#' @param terrain is the terrain generated in terrain.R
#' @param info is the output from the setup.plants function above
#' @author Jenessa Lemon
#' @examples plant.timestep(char.matrix, terrain, info)
#' @export
plant.timestep(plants, terrain, info){ #didn't ever use plants
  #' This function randomly draws from a uniform distribution to determind whether an individual survives according to our parameters.
  #' @param cell
  #' @param info is the output of the setup.plants function above
  #' @author Jenessa Lemon
  #' @examples square.step(pre.terrain)
  #' @export
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
          if(runif(1) >= info$survive[shouldnt this be a species name?]){ #If a random draw from a uniform distribution is higher than the survival probability,
            return(' ') #Don't change anything
          }else{
            terrain[i,j] <- NAME OF THE SPECIES TAKING OVER???)    #I DONT UNDERSTAND WHY ON GOD'S GREEN EARTH THIS WOULD RETURN A VECTOR. The entire point is that the plant survived, so it needs to be there.
          }
        }
      }
  }
  for (i in plants){
    slice <- plant.timestep(terrain)
    return(slice) #edited plant matrix
  }
}
plant.timestep(plants, terrain, info) #calling to see if it works


###Ok, I'm out of time to complete this function###
# reproduce <- function(row, col, plants, info){
#   possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
#   #possible.locations
#   if(!is.na(possible.locations)){
#     reproduce!
#   }
#   #being careful to check you do have somewhere to reproduce to! ...???
#   if(cell != " "){
#     they are going to compete!
#   }
#   return(plants)
# }
#
# fight <- function(names, 1, prob=comp.mat[row, column]){
# }

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
#for a given time calculate the next page a slot into our array

output of survive is a vector that goes into
what does plant.timestep do?
it loops over a matrix by row and column, thats how it knows what cell,
and it assigns the survive function to each cell
