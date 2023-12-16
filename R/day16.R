# Advent of Code Day 16
# Part 1

## File name
fileName<-"data/day16.txt"
## Read in map
mydata<-read.fwf(fileName,rep(1,110),comment.char="")

## create a blank map with just dots
mymap<-matrix(".",nrow=nrow(mydata),ncol=ncol(mydata))

## create a function for executing the route
myfunction<-function(loc,going){
  ## loop until the function is exited
  while(T){
    ## get the symbol on the map
    symbol<-mydata[loc[1],loc[2]]
    ## if the location is off the map or the location has been visited from the same direction before, stop 
    if(loc[1]>nrow(mymap) || loc[1]<1 || loc[2]>ncol(mymap) || loc[2]<1 || mymap[loc[1],loc[2]]==going){
      return(NULL)
    } else {
      ## record the direction of travel on the new map
      mymap[loc[1],loc[2]]<<-going
      ## keep going in the same direction
      if(symbol=="." || (symbol=="|" && going %in% c("U","D")) || (symbol=="-" && going %in% c("L","R"))){
        if(going=="R"){
          loc[2]<-loc[2]+1
        } else if(going=="L"){
          loc[2]<-loc[2]-1
        } else if(going=="U"){
          loc[1]<-loc[1]-1
        } else if(going=="D"){
          loc[1]<-loc[1]+1
        } 
        ## go at a 90 degree angle
      } else if(symbol == "/"){
        if(going=="R"){
          loc[1]<-loc[1]-1
          going<-"U"
        } else if(going=="L"){
          loc[1]<-loc[1]+1
          going<-"D"
        } else if(going=="U"){
          loc[2]<-loc[2]+1
          going<-"R"
        } else if(going=="D"){
          loc[2]<-loc[2]-1
          going<-"L"
        } 
      } else if(symbol == "\\"){
        if(going=="R"){
          loc[1]<-loc[1]+1
          going<-"D"
        } else if(going=="L"){
          loc[1]<-loc[1]-1
          going<-"U"
        } else if(going=="U"){
          loc[2]<-loc[2]-1
          going<-"L"
        } else if(going=="D"){
          loc[2]<-loc[2]+1
          going<-"R"
        } 
        ## go both ways
      } else if(symbol == "|"){
        myfunction(c(loc[1]+1,loc[2]),"D")
        myfunction(c(loc[1]-1,loc[2]),"U")
      } else if(symbol == "-"){
        myfunction(c(loc[1],loc[2]-1),"L")
        myfunction(c(loc[1],loc[2]+1),"R")
      } 
    }
  }
}

## run the function starting at the top right hand corner
myfunction(c(1,1),"R")

## sum the places visited recorded on the new map
result<-sum(mymap!=".")
## return the result
result

###########################
# Part 2

## initialise the result
result<-0

for(i in 1:nrow(mydata)){
  ## for each row come in going right
  mymap<-matrix(".",nrow=nrow(mydata),ncol=ncol(mydata))
  myfunction(c(i,1),"R")
  tempresult<-sum(mymap!=".")
  ## if new result is the highest, record it
  if(tempresult>result){
    result<-tempresult
  }
  ## for each row come in going left
  mymap<-matrix(".",nrow=nrow(mydata),ncol=ncol(mydata))
  myfunction(c(i,ncol(mydata)),"L")
  tempresult<-sum(mymap!=".")
  ## if new result is the highest, record it
  if(tempresult>result){
    result<-tempresult
  }
}

for(i in 1:ncol(mydata)){
  ## for each column come in going down
  mymap<-matrix(".",nrow=nrow(mydata),ncol=ncol(mydata))
  myfunction(c(1,i),"D")
  tempresult<-sum(mymap!=".")
  ## if new result is the highest, record it
  if(tempresult>result){
    result<-tempresult
  }
  ## for each column come in going up
  mymap<-matrix(".",nrow=nrow(mydata),ncol=ncol(mydata))
  myfunction(c(nrow(mydata),i),"U")
  tempresult<-sum(mymap!=".")
  ## if new result is the highest, record it
  if(tempresult>result){
    result<-tempresult
  }
}

## return the result
result
