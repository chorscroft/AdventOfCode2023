# Advent of Code Day 2
# Part 1

## Read in the file
fileName<-"data/day02.txt"

## Create a file connection
mydata<-file(fileName,"r")

## Initislise list of games
games<-vector("list",100)

## Start counting games
nogames<-0

## loop through each line of the data file
while (TRUE){
  ## get line from the data file
  line = readLines(mydata, n = 1)
  ## stop when the end of the file is reached
  if (length(line) == 0){
    break
  } else {
    ## iterate count
    nogames<-nogames+1
    ## remove ; and ,
    line<-gsub(";","",line)
    line<-gsub(",","",line)
    ## split line into separate elements and record in list
    games[[nogames]]<-unlist(strsplit(line," "))
  }
}
## close connection
close(mydata)

## Initialise result
result<-0
## min 12 red cubes, 13 green cubes, and 14 blue cubes
## loop over each game
for(i in 1:nogames){
  game<-games[[i]]
  fail<-F
  ## loop over each word in game
  for(j in 1:length(game)){
    ## if too many reds then fail
    if(game[j]=="red"){
      if(as.numeric(game[j-1])>12){
        fail<-T
        break
      }
    ## if too many greens then fail
    } else if (game[j]=="green"){
      if(as.numeric(game[j-1])>13){
        fail<-T
        break
      }
    ## if too many blues then fail
    } else if (game[j]=="blue"){
      if(as.numeric(game[j-1])>14){
        fail<-T
        break
      }
    }
  }
  ## record the game number if it didn't fail
  if (fail==F){
    result<-result+i
  }
}

## Read out the result
result

###########################
# Part 2

## Initialise the result
result<-0

## Loop over each game
for(i in 1:nogames){
  game<-games[[i]]
  
  ## initilise counts for each colour
  minred<-0
  mingreen<-0
  minblue<-0
  
  ## loop over each word in game
  for(j in 1:length(game)){
    ## Find minimum reds
    if(game[j]=="red"){
      if(as.numeric(game[j-1])>minred){
        minred<-as.numeric(game[j-1])
      }
    ## find minimum greens
    } else if (game[j]=="green"){
      if(as.numeric(game[j-1])>mingreen){
        mingreen<-as.numeric(game[j-1])
      }
    ## find minimum blues
    } else if (game[j]=="blue"){
      if(as.numeric(game[j-1])>minblue){
        minblue<-as.numeric(game[j-1])
      }
    }
  }
  
  ## iterate result
  result<-result+minred*mingreen*minblue
}

## Read out the result
result
