# Advent of Code Day 5
# Part 1

## File name
fileName<-"data/day05.txt"
## Create a file connection
mydata<-file(fileName,"r")

## loop through each line of the data file
while (TRUE){
  ## get line from the data file
  line = readLines(mydata, n = 1)
  ## stop when the end of the file is reached
  if (length(line) == 0){
    break
  } else if (line==""){
    ## If get to a blank line, reset the changed indicators to False
    changed<-rep(F,length(seeds))
  } else {
    ## If it is the first line, get the seeds
    if(substr(line,1,5)=="seeds"){
      ## split line by spaces and remove the first element, and change to numeric
      seeds<-as.numeric(unlist(strsplit(line," "))[-1])
      ## initialise record of if a seed has been changed to false for each seed
      changed<-rep(F,length(seeds))
    } else {
      ## split line by spaces
      map<-unlist(strsplit(line," "))
      if(is.na(as.numeric(map[1]))==F){
        ## get the mapping: destination, source, length
        map<-as.numeric(map)
        ## check each seed to see if it is in the range that is moving
        for(i in 1:length(seeds)){
          if(changed[i]==F && seeds[i]>=map[2] && seeds[i]<=map[2]+map[3]-1){
            ## set new position
            seeds[i]<-seeds[i]+(map[1]-map[2])
            ## record that it has already changed
            changed[i]<-T
          }
        }
      }
    }
  }
}
## close connection
close(mydata)

## Get the lowest location number
result<-min(seeds)
## Read out the result
result

###########################
# Part 2

## File name
fileName<-"data/day05.txt"
## Create a file connection
mydata<-file(fileName,"r")

## loop through each line of the data file
while (TRUE){
  ## get line from the data file
  line = readLines(mydata, n = 1)
  ## stop when the end of the file is reached
  if (length(line) == 0){
    break
  } else if (line==""){
    ## If get to a blank line, reset the changed indicators to False
    changed<-rep(F,nrow(seeds))
  } else {
    ## If it is the first line, get the seeds
    if(substr(line,1,5)=="seeds"){
      ## split line by spaces and remove the first element, and change to numeric
      seeds<-as.numeric(unlist(strsplit(line," "))[-1])
      ## put seeds into matrix format
      seeds<-matrix(seeds,ncol=2,byrow = T)
      ## change the second column to be end of the range (rather than the range)
      for (i in 1:nrow(seeds)){
        seeds[i,2]<-seeds[i,1]+seeds[i,2]-1
      }
      ## initialise record of if a seed has been changed to false for each seed
      changed<-rep(F,nrow(seeds))
    } else {
      ## split line by spaces
      map<-unlist(strsplit(line," "))
      if(is.na(as.numeric(map[1]))==F){
        ## get the mapping: destination, source, length
        map<-as.numeric(map)
        ## check each range of seeds to see if it is in the range that is moving
        for(i in 1:nrow(seeds)){
          if(changed[i]==F){
            ## start of range of seeds is in map
            if(seeds[i,1]>=map[2] && seeds[i,1]<=map[2]+map[3]-1){
              ## if range of seeds is totally inside
              if(seeds[i,2]<=map[2]+map[3]-1){
                ## map total seed range to destination
                seeds[i,]<-seeds[i,]+(map[1]-map[2])
                changed[i]<-T
              } else {
                ## only overlaps start of range of seeds
                ## map subsection of seeds to new destination
                newrange<-c(seeds[i,1],map[2]+map[3]-1)+map[1]-map[2]
                ## reduce size of seed range
                seeds[i,1]<-map[2]+map[3]
                ## add new, moved range to the end of the seed range list
                seeds<-rbind(seeds,newrange)
                changed<-c(changed,T)
              }
              ## if only overlaps end of range of seeds
            } else if (seeds[i,2]>=map[2] && seeds[i,2]<=map[2]+map[3]-1){
              ## map subsection of seeds to new destination
              newrange3<-c(map[2],seeds[i,2])+map[1]-map[2]
              ## reduce size of seed range
              seeds[i,2]<-map[2]-1
              ## add new, moved range to the end of the seed range list
              seeds<-rbind(seeds,newrange3)
              changed<-c(changed,T)
              ## if map to the middle of range of seeds
            } else if(seeds[i,1]<map[2] && seeds[i,2]>=map[2]+map[3]){
              ## map subsection of seeds to new destination
              newrange<-c(map[2],map[2]+map[3]-1)+map[1]-map[2]
              ## create new range with the reduced seed range at the end of the range
              newrange2<-c(map[2]+map[3],seeds[i,2])
              ## reduce size of seed range from the start of the range
              seeds[i,2]<-map[2]-1
              ## add new, moved range to the end of the seed range list, and the
              ## end of the seed range (that did not move)
              seeds<-rbind(seeds,newrange,newrange2)
              changed<-c(changed,T,F)
            }
          }
        }
      }
    }
  }
}
## close connection
close(mydata)

## Get the lowest location number
result<-min(seeds)
## Read out the result
result
