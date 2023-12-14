# Advent of Code Day 14
# Part 1

## File name
fileName<-"data/day14.txt"
## Read in map
mydata<-read.fwf(fileName,rep(1,100),comment.char="")

## copy map to edit
newmap<-mydata

## loop over each spot looking for "O"s that aren't already in the top row
for(i in 2:nrow(newmap)){
  for(j in(1:ncol(newmap))){
    if(newmap[i,j]=="O"){
      ## find the furtest north it can move to
      k<-0
      while(i-k>1){
        if(newmap[i-k-1,j]=="."){
          k<-k+1
        } else {
          break
        }
      }
      ## change it's old spot to a .
      newmap[i,j]<-"."
      ## move it to the new spot
      newmap[i-k,j]<-"O"
    }
  }
}

## calculate the load
result<-0
for(i in 1:nrow(newmap)){
  result<-result+sum(newmap[i,]=="O")*(nrow(newmap)-i+1)
}
  
## Read out the result
result

###########################
# Part 2
fileName<-"data/day14.txt"
## Read in the map
mydata<-read.fwf(fileName,rep(1,100),comment.char="")

## copy map to edit
newmap<-mydata

## function to run the spin cycle
spincycle<-function(newmap){
  ##north
  for(i in 2:nrow(newmap)){
    for(j in(1:ncol(newmap))){
      if(newmap[i,j]=="O"){
        k<-0
        while(i-k>1){
          if(newmap[i-k-1,j]=="."){
            k<-k+1
          } else {
            break
          }
        }
        newmap[i,j]<-"."
        newmap[i-k,j]<-"O"
      }
    }
  }
  
  ##west
  for(j in 2:ncol(newmap)){
    for(i in 1:nrow(newmap)){
      if(newmap[i,j]=="O"){
        k<-0
        while(j-k>1){
          if(newmap[i,j-k-1]=="."){
            k<-k+1
          } else {
            break
          }
        }
        newmap[i,j]<-"."
        newmap[i,j-k]<-"O"
      }
    }
  }
  
  ##south
  for(i in (nrow(newmap)-1):1){
    for(j in(1:ncol(newmap))){
      if(newmap[i,j]=="O"){
        k<-0
        while(i+k<nrow(newmap)){
          if(newmap[i+k+1,j]=="."){
            k<-k+1
          } else {
            break
          }
        }
        newmap[i,j]<-"."
        newmap[i+k,j]<-"O"
      }
    }
  }
  
  ##east
  for(j in (ncol(newmap)-1):1){
    for(i in 1:nrow(newmap)){
      if(newmap[i,j]=="O"){
        k<-0
        while(j+k<ncol(newmap)){
          if(newmap[i,j+k+1]=="."){
            k<-k+1
          } else {
            break
          }
        }
        newmap[i,j]<-"."
        newmap[i,j+k]<-"O"
      }
    }
  }
  return(newmap)
}

## save the new map in a list
maprecord<-vector("list",1)
maprecord[[1]]<-newmap

## count the spins
spins<-1
while(T){
  ## iterate the number of spins
  spins<-spins+1
  ## run the spin cycle and record the new map
  maprecord[[spins]]<-spincycle(maprecord[[spins-1]])
  ## test to see if the new map is the same as any of the old ones
  cycle<-F
  for(i in 1:(spins-1)){
    if(isTRUE(all.equal(maprecord[[spins]],maprecord[[i]]))){
      cycle<-T
      break
    }
  }
  ## if it is the same then stop
  if(cycle==T){
    break
  }
}

## calculate which map spin 1000000000 corresponds to
cyclelength<-spins-i
checkmap<-(1000000000-(i-1) )%% cyclelength  + i

## calculate the load
result<-0
for(i in 1:nrow(maprecord[[checkmap]])){
  result<-result+sum(maprecord[[checkmap]][i,]=="O")*(nrow(maprecord[[checkmap]])-i+1)
}

## Read out the result
result
