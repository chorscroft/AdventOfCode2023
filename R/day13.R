# Advent of Code Day 13
# Part 1

## File name
fileName<-"data/day13.txt"

## Create a file connection
mydata<-file(fileName,"r")

## find the reflection point and 
## return the point for vertical, 100*the point for horizonal
assessCurrentMap<-function(map){
  ## Vertical
  for(i in 1:(ncol(map)-1)){
    ## if next column is the same..
    if (isTRUE(all.equal(map[,i],map[,i+1]))){
      k<-1
      reflect<-T
      ##.. check all the other columns moving outwards are the same
      while(i-k>0 && i+k+1<=ncol(map)){
        if (isTRUE(all.equal(map[,i-k],map[,i+1+k]))==F){
          reflect<-F
        }
        k<-k+1
      }
      ## if so return the reflection point
      if(reflect==T){
        return(i)
      }
    }
  }
  
  ## Horizontal
  for(i in 1:(nrow(map)-1)){
    ## if next row is the same..
    if (isTRUE(all.equal(map[i,],map[i+1,]))){
      k<-1
      reflect<-T
      ##.. check all the other rows moving outwards are the same
      while(i-k>0 && i+k+1<=nrow(map)){
        if (isTRUE(all.equal(map[i-k,],map[i+1+k,]))==F){
          reflect<-F
        }
        k<-k+1
      }
      ## if so return the reflection point * 100
      if(reflect==T){
        return(i*100)
      }
    }
  }
  return(NA)
}


## Initialise the result
result<-0

## Initialise the first map
currentMap<-NULL

## loop through each line of the data file
while (TRUE){
  ## get line from the data file
  line = readLines(mydata, n = 1)
  ## stop when the end of the file is reached
  if (length(line) == 0){
    ## find the reflection point for the last map
    result<-result+assessCurrentMap(currentMap)
    break
  } else if (line==""){
    ## find the reflection point for the map and add it to the result
    result<-result+assessCurrentMap(currentMap)
    ## initialise the next map
    currentMap<-NULL
  } else {
    ## add the next line to the current map
    currentMap<-rbind(currentMap,unlist(strsplit(line,"")))
  }

}

## close connection
close(mydata)
## Read out the result
result



###########################
# Part 2

## File name
fileName<-"data/day13.txt"

## Create a file connection
mydata<-file(fileName,"r")



## find the reflection point where there is exactly one difference and 
## return the point for vertical, 100*the point for horizonal
assessCurrentMap<-function(map){
  ## Vertical
  for(i in 1:(ncol(map)-1)){
    ## indicator that the single change is found
    found1<-FALSE
    # test if thenext column is the same or only one different
    test<-all.equal(map[,i],map[,i+1])
    if (is.character(test)){
      if(unlist(strsplit(test," "))[1]=="1"){
        found1<-TRUE
        test<-TRUE
      } else {
        test<-FALSE
      }
    }
    ## if it is the same, test each column going outwards making sure there is
    ## only one difference in total
    if (test==TRUE){
      k<-1
      reflect<-T
      while(i-k>0 && i+k+1<=ncol(map)){
        test<-all.equal(map[,i-k],map[,i+1+k])
        if (is.character(test)){
          if(found1==FALSE && unlist(strsplit(test," "))[1]=="1"){
            found1<-TRUE
            test<-TRUE
          } else {
            test<-FALSE
          }
        }
        
        if (isTRUE(test)==F){
          reflect<-F
        }
        k<-k+1
      }
      ## if so return the reflection point
      if(reflect==T && found1==T){
        return(i)
      }
    }
  }
  
  ## Horizontal
  for(i in 1:(nrow(map)-1)){
    ## indicator that the single change is found
    found1<-FALSE
    # test if thenext column is the same or only one different
    test<-all.equal(map[i,],map[i+1,])
    if (is.character(test)){
      if(unlist(strsplit(test," "))[1]=="1"){
        found1<-TRUE
        test<-TRUE
      } else {
        test<-FALSE
      }
    }
    ## if it is the same, test each row going outwards making sure there is
    ## only one difference in total
    if (test==TRUE){
      k<-1
      reflect<-T
      while(i-k>0 && i+k+1<=nrow(map)){
        test<-all.equal(map[i-k,],map[i+1+k,])
        if (is.character(test)){
          if(found1==FALSE && unlist(strsplit(test," "))[1]=="1"){
            found1<-TRUE
            test<-TRUE
          } else {
            test<-FALSE
          }
        }
        
        if (isTRUE(test)==F){
          reflect<-F
        }
        k<-k+1
      }
      ## if so return the reflection point
      if(reflect==T && found1==T){
        return(i*100)
      }
    }
  }
  return(NA)
}


## Initialise the result
result<-0

## Initialise the first map
currentMap<-NULL

## loop through each line of the data file
while (TRUE){
  ## get line from the data file
  line = readLines(mydata, n = 1)
  ## stop when the end of the file is reached
  if (length(line) == 0){
    ## find the reflection point for the last map
    result<-result+assessCurrentMap(currentMap)
    break
  } else if (line==""){
    ## find the reflection point for the map and add it to the result
    result<-result+assessCurrentMap(currentMap)
    ## initialise the next map
    currentMap<-NULL
  } else {
    ## add the next line to the current map
    currentMap<-rbind(currentMap,unlist(strsplit(line,"")))
  }
  
}

## close connection
close(mydata)
## Read out the result
result
