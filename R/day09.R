# Advent of Code Day 9
# Part 1

## File name
fileName<-"data/day09.txt"
## Create a file connection
mydata<-file(fileName,"r")

## Initialise the result
result<-0

## loop through each line of the data file
while (TRUE){
  ## get line from the data file
  line = readLines(mydata, n = 1)
  ## stop when the end of the file is reached
  if (length(line) == 0){
    break
  } else {
    ## Split the line into a vector of numbers
    history<-as.numeric(unlist(strsplit(line," ")))
    
    ## Initialise the forecast with the last number in the history
    forecast<-history[length(history)]
    while(T){
      ## Get the difference between the history values
      history<-diff(history)
      ## if the differences are all zero, stop
      if(history[length(history)]==0){
        break
      } else {
        ## increase the forecast by the last number in the differences vector
        forecast<-forecast+history[length(history)]
      }
    }
  }
  ## add the forecasted value to the other results
  result<-result+forecast
}
## close connection
close(mydata)
## Read out the result
result

###########################
# Part 2

## File name
fileName<-"data/day09.txt"
## Create a file connection
mydata<-file(fileName,"r")

## Initialise the result
result<-0

## loop through each line of the data file
while (TRUE){
  ## get line from the data file
  line = readLines(mydata, n = 1)
  ## stop when the end of the file is reached
  if (length(line) == 0){
    break
  } else {
    ## Split the line into a vector of numbers
    history<-as.numeric(unlist(strsplit(line," ")))
    
    ## Initialise the forecast with the first number in the history
    forecast<-history[1]
    ## Indicator for subtracting or adding
    minusInd<-T
    while(T){
      ## Get the difference between the history values
      history<-diff(history)
      ## if the differences are all zero, stop
      if(history[length(history)]==0){
        break
      } else {
        if(minusInd==T){
          ## decrease the forecast by the first number in the differences vector
          forecast<-forecast-history[1]
          minusInd<-F
        } else {
          ## increase the forecast by the first number in the differences vector
          forecast<-forecast+history[1]
          minusInd<-T
        }
        
      }
    }
  }
  ## add the forecasted value to the other results
  result<-result+forecast
}
## close connection
close(mydata)
## Read out the result
result