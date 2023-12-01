# Advent of Code Day 1
# Part 1

## Read in the file
mydata<-read.table("data/day01.txt",col.names=c("line"))

## Split line into a vector of individual characters; store in a list
split_line<-apply(mydata,1,strsplit,"")

## Initialise the final calibration value
result<-0

## Loop over each line
for (vec in split_line){
  ## Get the vector from the list
  vec<-unlist(vec)
  ## Get the length of the vector
  len<-length(vec)
  ## Search from the start until you find a number
  for(i in 1:len){
    if(is.na(as.numeric(vec[i]))==FALSE){
      ## add 10 times the number to the result
      result<-result+10*as.numeric(vec[i])
      break
    }
  }
  ## search from the end until you find a number
  for(i in len:1){
    if(is.na(as.numeric(vec[i]))==FALSE){
      ## add the number to the result
      result<-result+as.numeric(vec[i])
      break
    }
  }
}


## Read out the result
result

###########################
# Part 2

## Read in the file
mydata<-read.table("data/day01.txt",col.names=c("line"))

## Split line into a vector of individual characters; store in a list
split_line<-apply(mydata,1,strsplit,"")

## Initialise the final calibration value
result<-0

## Create vector of all the possible numbers in the data
numbers<-c("one","two","three","four","five","six","seven","eight","nine")

## Find the earliest written number in the line
## Return a vector where the first element is the position
## and the second element is the number found
findEarliestWrittenNumber<-function(x){
  LocNum<-c(1000,0)
  for(i in 1:9){
    loc<-regexpr(numbers[i],x,fixed=T)
    if(loc>0 && loc<LocNum[1]){
      LocNum[1]<-loc
      LocNum[2]<-i
    }
  }
  return(LocNum)
}

## Find the last written number in the line
## Return a vector where the first element is the position
## and the second element is the number found
findLatestWrittenNumber<-function(x){
  LocNum<-c(0,0)
  for(i in 1:9){
    loc<-unlist(gregexpr(numbers[i],x,fixed=T))
    loc<-loc[length(loc)]
    if(loc>LocNum[1]){
      LocNum[1]<-loc
      LocNum[2]<-i
    }
  }
  return(LocNum)
}

## Loop over each line
for (ind in 1:length(split_line)){
  ## Get the vector from the list
  vec<-unlist(split_line[[ind]])
  
  ## Find the earliest and latest written number
  earliestWrittenNumber<-findEarliestWrittenNumber(mydata$line[ind])
  latestWrittenNumber<-findLatestWrittenNumber(mydata$line[ind])
  
  ## Get the length of the vector
  len<-length(vec)
  
  ## flag if there is no digit in the line
  nonumber<-T
  ## Search from the start until you find a number
  for(i in 1:len){
    if(is.na(as.numeric(vec[i]))==FALSE){
      ## record that a number was found
      nonumber<-F
      ## increase the result by 10 times the number found
      ## using the first of the digit or written number
      if(i<earliestWrittenNumber[1]){
        result<-result+10*as.numeric(vec[i])
      } else {
        result<-result+10*earliestWrittenNumber[2]
      }
      break
    }
  }
  
  ## If there are no digits in the line, use the earliest and
  ## latest written number to get the result
  if(nonumber){
    result<-result+10*earliestWrittenNumber[2]
    result<-result+latestWrittenNumber[2]
  } else {
    ## Search from the end until you find a number
    for(i in len:1){
      if(is.na(as.numeric(vec[i]))==FALSE){
        ## increase the result by 10 times the number found
        ## using the first of the digit or written number
        if(i>latestWrittenNumber[1]){
          result<-result+as.numeric(vec[i])
        } else {
          result<-result+latestWrittenNumber[2]
        }
        break
      }
    }
  }
  
}

## Read out the result
result