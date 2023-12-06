# Advent of Code Day 6
# Part 1

## File name
fileName<-"data/day06.txt"
## Read in file
mydata<-read.table(fileName)
## Drop first column
mydata<-mydata[,-1]

## Initialise result
result<-1

## Loop over each race
for(race in 1:ncol(mydata)){
  ## Initialise count of how many times you can beat the record for this race
  noBeatRecord<-0
  ## Test for each possible lengths of time you can hold the button (skipping 0)
  for(i in 1:mydata[1,race]){
    ## calculate distance travelled 
    ## (total length of time for race - button push time) * button push time
    totalDistance<-(mydata[1,race]-i)*i
    ## If the record is beaten, record it
    if(totalDistance>mydata[2,race]){
      noBeatRecord<-noBeatRecord+1
    } else if(noBeatRecord>0){
      ## if the button is now being pressed for too long, stop searching
      break
    }
  }
  ## Multiply the count of beaten records together
  result<-result*noBeatRecord
}

## Read out the result
result

###########################
# Part 2

## Concatenate the times
time<-as.numeric(paste(mydata[1,],collapse = ""))
## Concatenate the distances
distance<-as.numeric(paste(mydata[2,],collapse = ""))

## Initialise count of how many times you can beat the record for this race
noBeatRecord<-0
## Test for each possible lengths of time you can hold the button (skipping 0)
for(i in 1:time){
  ## calculate distance travelled 
  ## (total length of time for race - button push time) * button push time
  totalDistance<-(time-i)*i
  ## If the record is beaten, record it
  if(totalDistance>distance){
    noBeatRecord<-noBeatRecord+1
  } else if(noBeatRecord>0){
    ## if the button is now being pressed for too long, stop searching
    break
  }
}

## Read out the result
noBeatRecord
