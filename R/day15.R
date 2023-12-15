# Advent of Code Day 15
# Part 1

## File name
fileName<-"data/day15.txt"

## Read in the data
mydata<-read.table(fileName,col.names=c("line"),comment.char = "")
## Split the steps by commas
steps<-unlist(strsplit(mydata$line,","))

## initialise results vector
results<-rep(0,length(steps))

## loop over each step
for(s in 1:length(steps)){
  ## inialise the current value
  currentValue<-0
  ## split step into individual characters
  vecString<-unlist(strsplit(steps[s],""))
  ## loop over each character
  for(i in 1:length(vecString)){
    ## do HASH algorithm
    currentValue<-currentValue+utf8ToInt(vecString[i])
    currentValue<-currentValue*17
    currentValue<-currentValue %% 256
  }
  ## store value
  results[s]<-currentValue
}

## sum value for each step
result<-sum(results)
## read out result
result

###########################
# Part 2

## File name
fileName<-"data/day15.txt"

## Read in the data
mydata<-read.table(fileName,col.names=c("line"),comment.char = "")
## Split the steps by commas
steps<-unlist(strsplit(mydata$line,","))

## initialise the 256 boxes
boxes<-vector("list",256)

## loop over each step
for(s in 1:length(steps)){
  ## initialise current value
  currentValue<-0
  ## split step into individual characters
  vecString<-unlist(strsplit(steps[s],""))
  ## loop over each character in the string
  for(i in 1:length(vecString)){
    ## if the character is a # or - then stop looping
    if(vecString[i] %in% c("-","=")){
      break
    } else {
      ## run HASH algorithm
      currentValue<-currentValue+utf8ToInt(vecString[i])
      currentValue<-currentValue*17
      currentValue<-currentValue %% 256
    }
  }
  ## if the character is a dash
  if(vecString[i]=="-"){
    ## extract the label in the step
    label<-paste(vecString[1:(i-1)],collapse = "")
    ## check the box to see it has thing in it, and if so, if the label is in it
    if(length(boxes[[currentValue+1]])>0){
      for(j in 1:nrow(boxes[[currentValue+1]])){
        if(boxes[[currentValue+1]]$label[j]==label){
          ## if it is then remove the label from the box
          boxes[[currentValue+1]]<-boxes[[currentValue+1]][-j,]
          break
        }
      }
      ## if the box is now empty, make the list element NULL
      if(nrow(boxes[[currentValue+1]])==0){
        boxes[currentValue+1]<-list(NULL)
      }
    }
  } else {
    ## if the character is an equals sign
    ## extract the label in the step
    label<-paste(vecString[1:(i-1)],collapse = "")
    ## extract the focal length
    focal<-as.numeric(vecString[length(vecString)])
    ## if the box already has things in it..
    if(length(boxes[[currentValue+1]])>0){
      ## ..look for the label in the box
      found<-F
      for(j in 1:nrow(boxes[[currentValue+1]])){
        ## if the label is already in the box then replace it with the new focal length
        if(boxes[[currentValue+1]]$label[j]==label){
          found<-T
          boxes[[currentValue+1]]$focal[j]<-focal
          break
        }
      }
      ## if the label wasn't in the box then add it in
      if(found==F){
        boxes[[currentValue+1]]<-rbind(boxes[[currentValue+1]],data.frame(label,focal))
      }
    } else {
      ## if the box was empty then add it in
      boxes[[currentValue+1]]<-data.frame(label=label,focal=focal)
    }
  }
}

## initialise result
result<-0
## loop over each box
for(b in 1:length(boxes)){
  ## calculate focusing power and sum them
  if(length(boxes[[b]])>0){
    for(j in 1:nrow(boxes[[b]])){
      result<-result+b*j*boxes[[b]]$focal[j]
    }
  }
}

## read out result
result