# Advent of Code Day 11
# Part 1

## File name
fileName<-"data/day11.txt"
## Read in directions
mydata<-read.fwf(fileName,rep(1,140),comment.char="")

## Find which rows are all .
row0<-apply(mydata,1,function(x)sum(x=="#"))
## Get indices of rows in descending order
row0<-rev(which(row0==0))

## fill in extra rows of dots from the bottom up
for(i in row0){
  if(i == 1){
    mydata<-rbind(rep(".",ncol(mydata)),mydata)
  } else if(i == nrow(mydata)){
    mydata<-rbind(mydata,rep(".",ncol(mydata)))
  } else {
    mydata<-rbind(mydata[1:i,],rep(".",ncol(mydata)),mydata[(i+1):nrow(mydata),])
  }
}

## Find which columns are all .
col0<-apply(mydata,2,function(x)sum(x=="#"))
## Get indices of columns in descending order
col0<-rev(which(col0==0))

## fill in extra columns of dots from the right
for(i in col0){
  if(i == 1){
    mydata<-cbind(rep(".",nrow(mydata)),mydata)
  } else if(i == nrow(mydata)){
    mydata<-cbind(mydata,rep(".",nrow(mydata)))
  } else {
    mydata<-cbind(mydata[,1:i],rep(".",nrow(mydata)),mydata[,(i+1):ncol(mydata)])
  }
}

## count the number of galaxies
numberGalaxies<-sum(mydata=="#")
## get the coordinates of the galaxies
coords<-which(mydata=="#",arr.ind = T)

## caluculate the distances between each pair of galaxies
result<-0
for(i in 1:(numberGalaxies-1)){
  for(j in (i+1):numberGalaxies){
    result<-result+abs(coords[i,1]-coords[j,1])+abs(coords[i,2]-coords[j,2])
  }
}

## Read out the result
result

###########################
# Part 2


## File name
fileName<-"data/day11.txt"
## Read in directions
mydata<-read.fwf(fileName,rep(1,140),comment.char="")

## find rows with all .
row0<-apply(mydata,1,function(x)sum(x=="#"))
row0<-which(row0==0)
## find columns with all .
col0<-apply(mydata,2,function(x)sum(x=="#"))
col0<-which(col0==0)

## count the number of galaxies
numberGalaxies<-sum(mydata=="#")
## get the coordinates of the galaxies
coords<-which(mydata=="#",arr.ind = T)

## initialise result
result<-0

## check each pair of galaxies
for(i in 1:(numberGalaxies-1)){
  for(j in (i+1):numberGalaxies){
    minx<-min(coords[i,1],coords[j,1])
    maxx<-max(coords[i,1],coords[j,1])
    miny<-min(coords[i,2],coords[j,2])
    maxy<-max(coords[i,2],coords[j,2])
    
    ## get the Manhattan distance between galaxies
    result<-result+maxy-miny+maxx-minx
    
    ## add 999999 every time a zero row or column was crossed
    emptyLines<-length(which(row0>minx & row0<maxx))
    if(emptyLines>0){
      result<-result+999999*emptyLines
    }
    emptyLines<-length(which(col0>miny & col0<maxy))
    if(emptyLines>0){
      result<-result+999999*emptyLines
    }
    
  }
}

## Read out the result
result
