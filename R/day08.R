# Advent of Code Day 8
# Part 1

## File name
fileName<-"data/day08.txt"
## Read in directions
instructions<-read.table(fileName,col.names=c("instructions"),nrows=1)
## Read in network
mydata<-read.table(fileName,skip=2,col.names = c("position","equals","left","right"))
## clean up columns so it's just the 3 character location
mydata$left<-substr(mydata$left,2,4)
mydata$right<-substr(mydata$right,1,3)

## split the instructions into a vector for each character
instructions<-unlist(strsplit(instructions$instructions,""))

## initialise the index for the instruction
i <- 1
## find the first row
row<-match("AAA",mydata$position)
## initilaise the count of steps
steps<-0

while(T){
  ## iterate the step count
  steps<-steps+1
  
  if(instructions[i]=="L"){
    ## go left
    nextLoc<-mydata$left[row]
  } else {
    ## go right
    nextLoc<-mydata$right[row]
  }
  if(nextLoc=="ZZZ"){
    ## stop if you reach the end
    break
  } else {
    ## iterate instructions index
    i<-i+1
    if(i>length(instructions)){
      i<-1
    }
    ## find row of the next location
    row<-match(nextLoc,mydata$position)
  }
}

## Read out the result
steps

###########################
# Part 2

## find which rows end in an A
rows<-which(sapply(mydata$position,substr,3,3)=="A")
## initialise the step count for each starting location
startsteps<-rep(0,length(rows))
## loop over each starting location
for(r in 1:length(rows)){
  ## get the starting row for the current starting location
  row<-rows[r]
  ## initialise the step count
  steps<-0
  ## initialise the index for the instruction
  i <- 1
  while(T){
    ## iterate the step count
    steps<-steps+1
    
    if(instructions[i]=="L"){
      ## go left
      nextLoc<-mydata$left[row]
    } else {
      ## go right
      nextLoc<-mydata$right[row]
    }
    if(substr(nextLoc,3,3)=="Z"){
      # stop if you reach the end
      break
    } else {
      ## iterate instructions index
      i<-i+1
      if(i>length(instructions)){
        i<-1
      }
      ## find row of the next location
      row<-match(nextLoc,mydata$position)
    }
  }
  
  ## Record the step count for this starting location
  startsteps[r]<-steps
}

## finds the greatest common denominator (Euclid's algorithm)
gcd<-function(a,b){
  if(a==0){
    return(b)
  } else {
    return(gcd(b %% a,a))
  }
}
## finds the lowest common multiple
lcm<-function(a,b){
  return(a*b/gcd(a,b))
}

## find the lowest common multiple of all the step counts
result<-startsteps[1]
for(i in 2:length(startsteps)){
  result<-lcm(result,startsteps[i])
}

## Read out the result
result