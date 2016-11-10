###Herbivore has to move!
new.loc(row, col, herbivore) #remarkably similar to plant dispersion. But in this case, the herbivore MOVES, so the previous cell is set to 0.
#Herbivores eat x <- x-1
#Herbivores may kill! runif trick, you have to change the plant matrix to 0.
#Link it all together herbivore.timestep

setup.herbivore <- function(eat, kill, repro){
  if(!is.numeric(eat) | !is.numeric(kill) | !is.numeric(repro)){
    stop()
  }
  if{
    
  }
  if{
    
  }
}
###Herbivore has to move!
new.loc(row, col, herbivore) #remarkably similar to plant reproducting
#Herbivores eat x <- x-1
#Herbivores may kill! runif trick
#Link it all together herbivore.timestep