# Advent of Code Day 7
# Part 1

## File name
fileName<-"data/day07.txt"
## Read in file
mydata<-read.table(fileName,col.names=c("hand","bid"))
## Add a column for the hand type
mydata$type<-0
## Add columns for each card
mydata$card1<-0
mydata$card2<-0
mydata$card3<-0
mydata$card4<-0
mydata$card5<-0

## Create vector of cards in rank order
cards<-c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")

## Loop over each hand
for(i in 1:nrow(mydata)){
  ## Split into a vector where each element is a card
  hand<-unlist(strsplit(mydata$hand[i],""))
  ## record the rank of each card in the hand
  mydata[i,4:8]<-match(hand,cards)
  ## Get the count of each card
  countCards<-apply(sapply(1:5,function(x)hand[x]==cards),1,sum)
  if(sum(countCards==5)==1){
    ## 5 of a kind
    mydata$type[i] <- 1
  } else if(sum(countCards==4)==1){
    ## 4 of a kind
    mydata$type[i] <- 2
  } else if(sum(countCards==3)==1 && sum(countCards==2)==1){
    ## Full house
    mydata$type[i] <- 3
  } else if(sum(countCards==3)==1){
    ## Three of a kind
    mydata$type[i] <- 4
  } else if(sum(countCards==2)==2){
    ## Two pair
    mydata$type[i] <- 5
  } else if(sum(countCards==2)==1){
    ## One pair
    mydata$type[i] <- 6
  } else {
    ## High card
    mydata$type[i] <- 7
  }
}

## Sort hands by type, then by each card in order
mydata<-mydata[order(mydata$type,mydata$card1,mydata$card2,mydata$card3,mydata$card4,mydata$card5),]

## Initialise result
result<-0
## For each hand, calculate the winnings and add together
for(i in 1:nrow(mydata)){
  result<-result+mydata$bid[i]*(nrow(mydata)-i+1)
}

## Read out the result
result

###########################
# Part 2

## File name
fileName<-"data/day07.txt"
## Read in file
mydata<-read.table(fileName,col.names=c("hand","bid"))
## Add a column for the hand type
mydata$type<-0
## Add columns for each card
mydata$card1<-0
mydata$card2<-0
mydata$card3<-0
mydata$card4<-0
mydata$card5<-0

## Create vector of cards in rank order
cards<-c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J")

## Loop over each hand
for(i in 1:nrow(mydata)){
  ## Split into a vector where each element is a card
  hand<-unlist(strsplit(mydata$hand[i],""))
  ## record the rank of each card in the hand
  mydata[i,4:8]<-match(hand,cards)
  ## Get the count of each card
  countCards<-apply(sapply(1:5,function(x)hand[x]==cards),1,sum)
  if(sum(countCards==5)==1 || (max(countCards[1:12]) + countCards[13])==5){
    ## 5 of a kind
    mydata$type[i] <- 1
  } else if(sum(countCards==4)==1 || (max(countCards[1:12]) + countCards[13])==4){
    ## 4 of a kind
    mydata$type[i] <- 2
  } else if((sum(countCards==3)==1 && sum(countCards==2)==1) || (sum(countCards==2)==2 && countCards[13])==1) {
    ## Full house
    mydata$type[i] <- 3
  } else if(sum(countCards==3)==1 || (max(countCards[1:12]) + countCards[13])==3){
    ## Three of a kind
    mydata$type[i] <- 4
  } else if(sum(countCards==2)==2){
    ## Two pair
    mydata$type[i] <- 5
  } else if(sum(countCards==2)==1 || countCards[13]==1){
    ## One pair
    mydata$type[i] <- 6
  } else {
    ## High card
    mydata$type[i] <- 7
  }
}

## Sort hands by type, then by each card in order
mydata<-mydata[order(mydata$type,mydata$card1,mydata$card2,mydata$card3,mydata$card4,mydata$card5),]

## Initialise result
result<-0
## For each hand, calculate the winnings and add together
for(i in 1:nrow(mydata)){
  result<-result+mydata$bid[i]*(nrow(mydata)-i+1)
}

## Read out the result
result