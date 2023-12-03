# Advent of Code Day 3
# Part 1

## Read in the file
fileName<-"data/day03.txt"
mydata<-read.fwf(fileName,rep(1,140),comment.char="")

## assess if the number is next to a symbol
assessNumber<-function(start,end,row,newnumber){
  ## if the number isn't at the start of the row, check for a symbol
  ## directly to the left
  if (start>1){
    if(mydata[row,start-1]!="." && is.na(as.numeric(mydata[row,start-1]))){
      return(as.numeric(newnumber))
    }
    ## reduce start by one so diagonals will be checked in next step
    start<-start-1
  }
  ## if the number isn't at the end of the row, check for a symbol
  ## directly to the right
  if (end<ncol(mydata)){
    if(mydata[row,end+1]!="." && is.na(as.numeric(mydata[row,end+1]))){
      return(as.numeric(newnumber))
    }
    ## increase end by one so diagonals will be checked in next step
    end<-end+1
  }
  ## if number is not on the first row then check the row above for symbols
  if(row>1){
    for(i in start:end){
      if(mydata[row-1,i]!="." && is.na(as.numeric(mydata[row-1,i]))){
        return(as.numeric(newnumber))
      }
    }
  }
  ## if number is not on the last row then check the row below for symbols
  if(row<nrow(mydata)){
    for(i in start:end){
      if(mydata[row+1,i]!="." && is.na(as.numeric(mydata[row+1,i]))){
        return(as.numeric(newnumber))
      }
    }
  }
  ## if there are no symbols then return zero
  return(0)
}

## Initialise result
result<-0

## nostart records the column the number starts in, initialise at zero
nostart<-0
## initialise the next number
newnumber<-""
## loop over the characters in each row and column
for(i in 1:nrow(mydata)){
  for(j in 1:ncol(mydata)){
    ## check if the character is numeric
    if(is.na(as.numeric(mydata[i,j]))==F){
      if(nostart==0){
        ## if first digit, record the column
        nostart<-j
      }
      ## record the digit in the number
      newnumber<-paste0(newnumber,mydata[i,j])
    } else {
      ## if character is first non-digit after a number, then assess if the 
      ## number has a symbol adjacent to it, and record if it does
      if(nostart>0){
        result<-result+assessNumber(nostart,j-1,i,newnumber)
        ## reset the nostart and newnumber variables
        nostart<-0
        newnumber<-""
      }
    }
  }
  ## if number is at the end of the row, then assess if the 
  ## number has a symbol adjacent to it, and record if it does
  if(nostart>0){
    result<-result+assessNumber(nostart,j,i,newnumber)
    ## reset the nostart and newnumber variables
    nostart<-0
    newnumber<-""
  }
}

## Read out the result
result

###########################
# Part 2

## Read in the file
fileName<-"data/day03.txt"
mydata<-read.fwf(fileName,rep(1,140),comment.char="")

## initialise the star locations matrix
starlocs<-NULL
## look for star locations and record them in a matrix
## first column is row, second column is column
for(i in 1:nrow(mydata)){
  for(j in 1:ncol(mydata)){
    if(mydata[i,j]=="*"){
      starlocs<-rbind(starlocs,c(i,j))
    }
  }
}

## Initialise gears matrix to the same size as star location matrix
## This will store the two numbers next to each star (or stay as a zero)
gears<-matrix(0,nrow=nrow(starlocs),2)

## assess if a number is next to a star, if it is return the number and the location
## of the adjacent star
assessStarNumber<-function(start,end,row,newnumber){
  ## if the number is not at the start of a row, check the
  ## character directly to the left
  if (start>1){
    if(mydata[row,start-1]=="*"){
      return(c(as.numeric(newnumber),row,start-1))
    }
    ## reduce start by one so diagonals will be checked in next step
    start<-start-1
  }
  ## if the number is not at the end of a row, check the
  ## character directly to the right
  if (end<ncol(mydata)){
    if(mydata[row,end+1]=="*"){
      return(c(as.numeric(newnumber),row,end+1))
    }
    ## reduce end by one so diagonals will be checked in next step
    end<-end+1
  }
  ## if number is not on the first row then check the row above for stars
  if(row>1){
    for(i in start:end){
      if(mydata[row-1,i]=="*"){
        return(c(as.numeric(newnumber),row-1,i))
      }
    }
  }
  ## if number is not on the last row then check the row below for symbols
  if(row<nrow(mydata)){
    for(i in start:end){
      if(mydata[row+1,i]=="*"){
        return(c(as.numeric(newnumber),row+1,i))
      }
    }
  }
  ## if there are no adjacent stars return zero
  return(0)
}

## nostart records the column the number starts in, initialise at zero
nostart<-0
## initialise the next number
newnumber<-""
## loop over the characters in each row and column
for(i in 1:nrow(mydata)){
  for(j in 1:ncol(mydata)){
    ## check if the character is numeric
    if(is.na(as.numeric(mydata[i,j]))==F){
      ## if first digit then record column
      if(nostart==0){
        nostart<-j
      }
      ## record the digit in the number
      newnumber<-paste0(newnumber,mydata[i,j])
    } else {
      ## if character is first non-digit after a number, then assess if the 
      ## number has a star next to it
      if(nostart>0){
        found<-assessStarNumber(nostart,j-1,i,newnumber)
        ## if the number has a star next to it, find the star in the star locations
        ## matrix, and record the number in the corresponding gears matrix
        if(length(found)>1){
          for(x in 1:nrow(starlocs)){
            if(starlocs[x,1]==found[2] && starlocs[x,2]==found[3]){
              if(gears[x,1]==0){
                gears[x,1]<-found[1]
              } else if(gears[x,2]==0){
                gears[x,2]<-found[1]
              }
            }
          }
        }
        ## reset the nostart and newnumber variables
        nostart<-0
        newnumber<-""
      }
    }
  }
  ## if number is at the end of the row, then assess if the 
  ## number has a star next to it
  if(nostart>0){
    found<-assessStarNumber(nostart,j,i,newnumber)
    ## if the number has a star next to it, find the star in the star locations
    ## matrix, and record the number in the corresponding gears matrix
    if(length(found)>1){
      for(x in 1:nrow(starlocs)){
        if(starlocs[x,1]==found[2] && starlocs[x,2]==found[3]){
          if(gears[x,1]==0){
            gears[x,1]<-found[1]
          } else if(gears[x,2]==0){
            gears[x,2]<-found[1]
          }
        }
      }
    }
    ## reset the nostart and newnumber variables
    nostart<-0
    newnumber<-""
  }
}

## Initialise result
result<-0
## for each star, multiply the two numbers next to it together
## and sum all the products
for(i in 1:nrow(gears)){
  result<-result+gears[i,1]*gears[i,2]
}

## Read out the result
result