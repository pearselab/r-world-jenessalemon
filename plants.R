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

#########
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

##############################
#This function evaluates whether a cell is occupied, under water, or open. If it is open, chance determines whether or not the plant survives and occupies the cell.
survive <- function(cell, info){
  if(is.na(cell)){ #water should stay water
    return(NA)
  }
  #if(cell != ' '){
  # return(cell)  #if occupied, compete!
  if(cell == ' '){ #Then we're actually going to see if the plants survives
    if(runif(1) >= info$survive[plant]){ #If a random draw from a uniform distribution is higher than the survival probability,
      return(' ') #Don't change anything, it didn't survive :(
    }else{ #the plant survives! SO WE NEED TO PUT THAT PLANT INTO THAT SPOT. I can't think of a reason to do anything else. This is the best I can possibly do here. I have to move on.
      cell <- info$names[plant]
    }
  }
}

################################################
#This function applies the survive function (above) to an entire plant matrix.
plant.timestep <- function(plants){
  for (i in 1:nrow(plants)){
    for (j in 1:ncol(plants)){
      plants[i,j] <- survive(plants[i,j]) #this is why in the survive function we can just call it "cell"
    }
  }
  return(terrain) #edited plant matrix
}
plant.timestep(plants)

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

##########################################################
#This function generates "slices" in time (of our terrain) and stores them in a multidimensional array.
run.plant.ecosystem <- function(terrain, timesteps){
  plants <- array("", dim=c(terrain), timesteps+1)
  for (i in 1:timesteps){ #let's do 50 timesteps
    plants[,,i] <- plant.timestep(terrain)        #I'm trying to assign a slice of the array, but I think I'm assigning a cell.
  }
  for(i in seq_len(dim(plants)[3])){
    plants[,,i][is.na(terrain)] <- NA             #Kill the plants growing on water! They're not supposed to do that!
  }
  return(plants)
}
#for a given time calculate the next page a slot into our array
