# Advent of Code Day 4
# Part 1

## Read in the file
fileName<-"data/day04.txt"
mydata<-read.table(fileName)

## find pipe
pipe<-match("|",mydata[1,])

## Initialise result
result<-0

## Loop over each card
for(card in 1:nrow(mydata)){
  ## Extract the winning numbers
  winningNos<-mydata[card,3:(pipe-1)]
  ## Extract my numbers
  myNos<-mydata[card,(pipe+1):ncol(mydata)]
  ## Initialise the card score
  cardScore<-0
  ## check each number I have against each winning number
  for(i in 1:length(myNos)){
    for(j in 1:length(winningNos)){
      if(myNos[i]==winningNos[j]){
        ## if first winning number then score is 1
        if(cardScore==0){
          cardScore<-1
        } else {
          ## if more winning numbers then double the score
          cardScore<-cardScore*2
        }
        ## stop if winning number is identified
        break
      }
    }
  }
  ## add the card scores together
  result<-result+cardScore
}

## Read out the result
result

###########################
# Part 2

## Initialise result
result<-0

## Initialse count of copies of each card
copies<-rep(1,nrow(mydata))

## Loop over each card
for(card in 1:nrow(mydata)){
  ## Extract the winning numbers
  winningNos<-mydata[card,3:(pipe-1)]
  ## Extract my numbers
  myNos<-mydata[card,(pipe+1):ncol(mydata)]
  ## Initialise card score
  cardScore<-0
  ## check each number I have against each winning number
  for(i in 1:length(myNos)){
    for(j in 1:length(winningNos)){
      if(myNos[i]==winningNos[j]){
        ## If winning number found, increase score by one
        cardScore<-cardScore+1
        break
      }
    }
  }
  ## If the card is a winning card, increase the count of copies for each
  ## of the next x cards (where x is the score for this card) by the number of 
  ## copies of this card
  if(cardScore>0){
    for(k in 1:cardScore){
      if(card+k<=length(copies)){
        copies[card+k]<-copies[card+k]+copies[card]
      } else {
        break
      }
    }
  }

}

## Sum the copies of every card
result<-sum(copies)

## Read out the result
result
