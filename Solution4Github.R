#Author: Saran Mishra

#Project: Optimal path for two node disjoint 

####################################################################################

#In case you do not have these packages, please install:

# install.packages("fields")
# install.packages("sp")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("data.table")
# install.packages("fields")
# install.packages("leaflet")


library(leaflet)
library(fields)
library(dplyr)
library(tidyverse)
library(tidyr)
library(data.table)
library(sp)

#Load Data 
#This is WHERE YOU INSERT YOUR DATA
#Data should have 6 columns -> Name of location, Address, Latitude of Location, Longitude of location, 
#Latitude of home base (same number), Longitude of home base (same number). 


MyData = read.csv('')
MyData = as.data.frame(MyData)
colnames(MyData) = c("Name","Address","LatAway","LongAway", "latHome", "longHome")

#Convert Longitude and Latitude into Miles using rdist.earth function from the fields Package 

distance_from_home = as.data.frame(rdist.earth(matrix(c(MyData$longHome, MyData$latHome),
                                                      ncol=2),matrix(c(MyData$LongAway,MyData$LatAway), ncol=2),miles=TRUE))

#Obtain single vector of all distances from home 
Ordered_distances = distance_from_home[1,]
Ordered_distances = as.data.frame(gather(Ordered_distances))

#Rank distances from home 
Ordered_distances$Rank = NA
colnames(Ordered_distances) = c("Name","Distance_in_Miles","Rank")
Ordered_distances$Name = MyData$Name
Order = order(Ordered_distances$Distance_in_Miles,Ordered_distances$Name)
Ordered_distances$Rank[Order] = 1:nrow(Ordered_distances)

#Prepare Data to find distances between nodes
##MyData Final prior to finding distance between nodes

My_Data_Final = merge(Ordered_distances,MyData,by="Name")
My_Data_Final = My_Data_Final[,-c(7,8)]
My_Data_Final = unique(My_Data_Final)
My_Data_Final = as.data.frame(My_Data_Final)
My_Data_Final = My_Data_Final[order(My_Data_Final$Rank),]

# Test data to see characteristics:

# summary(My_Data_Final)
# Total_distance = sum(My_Data_Final[,"Distance_in_Miles"])

##Calculate distance between points

distance_between_nodes = as.data.frame(rdist.earth(matrix(c(My_Data_Final$LongAway, My_Data_Final$LatAway),
                                                          ncol=2),matrix(c(My_Data_Final$LongAway,My_Data_Final$LatAway), ncol=2),miles=TRUE))

#If distance are [e - 5] apart, then replace with 0 as the distance is super small and can be ignored

distance_between_nodes[distance_between_nodes < .0001] <- 0

#Export matrix to view data in excel to see diagonal = 0 

write.csv(distance_between_nodes, file = "kioskMatrix.csv")

# *******************************************************Part 2: Optimization************************************************************************#

# The Full explanantion of this is provided in the readMe file 

file = "kioskMatrix.csv"
theData = read.csv(file)
theData = theData[,-1]
nodesToTravel = nrow(theData)
theDataCols = colnames(theData)[-1]


theSampleSplit = matrix(0, nrow = 50, ncol = 50)
colnames(theSampleSplit) = theDataCols

for (i in (2:nodesToTravel)) {
  
  startNode = i
  theChoices = theData[startNode,-1]
  theChoices = sort(theChoices)
  theChoices = theChoices[1:25]
  theChoices = colnames(theChoices)
  
  theSampleSplit[startNode-1,colnames(theSampleSplit) %in% theChoices] = 1
  
}



theHeaders = colnames(theData)
theNumbers = 1:length(theHeaders)
theMatch = cbind(theHeaders, theNumbers)

colnames(theData) = paste0("N",1:nodesToTravel)
rownames(theData) = paste0("N",1:nodesToTravel)

trackingOutput = NULL
possibleStarts = (nodesToTravel-1)/2

for (k in (1:nrow(theSampleSplit))) {
  
  for (i in (1:(possibleStarts))) {
    
    theTrackingMatrix = matrix(0, nrow = 2, ncol = nodesToTravel+3)
    colnames(theTrackingMatrix) = c(
      paste0("D1: L", 1:25), 
      "D1: Home", 
      paste0("D2: L", 1:25), 
      "D2: Home", 
      "Total", 
      "Split"
    )
    rownames(theTrackingMatrix) = c("Path", "Distance")
    
    
    # DRIVER 1
    theIndex = as.numeric(theSampleSplit[k,])==1
    theIndex = which(theIndex==TRUE)+1
    
    dataWithTracking = theData[c(1, theIndex),c(1, theIndex)]
    dataWithTracking[1,1:ncol(dataWithTracking)] = NA
    
    for (j in (1:ncol(dataWithTracking))) {
      
      if (j == 1) {
        
        startNode = i+1
        theTrackingMatrix[1,j] = theIndex[startNode-1]
        theTrackingMatrix[2,j] = dataWithTracking[startNode,1]
        dataWithTracking[startNode,1:ncol(dataWithTracking)] = NA
        
      } else if (j < ncol(dataWithTracking)) {
        
        priorNode = theTrackingMatrix[1,j-1]
        theTempData = dataWithTracking[is.na(dataWithTracking[,paste0("N",priorNode)])==FALSE, paste0("N",priorNode)]
        theMin = min(theTempData)
        startNode = which(dataWithTracking[,paste0("N",priorNode)]==theMin)
        startNode = startNode[1]
        
        theTrackingMatrix[1,j] = theIndex[startNode-1]
        theTrackingMatrix[2,j] = theMin
        dataWithTracking[startNode,1:ncol(dataWithTracking)] = NA
        
      } else {
        
        theTrackingMatrix[1,j] = 1
        theTrackingMatrix[2,j] = theData[priorNode,1]
        
      }
      
    }
    
    # DRIVER 2
    theIndex = as.numeric(theSampleSplit[k,])==0
    theIndex = which(theIndex==TRUE)+1
    
    dataWithTracking = theData[c(1, theIndex),c(1, theIndex)]
    dataWithTracking[1,1:ncol(dataWithTracking)] = NA
    
    for (j in (1:ncol(dataWithTracking))) {
      
      if (j == 1) {
        
        startNode = i+1
        theTrackingMatrix[1,j+26] = theIndex[startNode-1]
        theTrackingMatrix[2,j+26] = dataWithTracking[startNode,1]
        dataWithTracking[startNode,1:ncol(dataWithTracking)] = NA
        
      } else if (j < ncol(dataWithTracking)) {
        
        priorNode = theTrackingMatrix[1,j+26-1]
        theTempData = dataWithTracking[is.na(dataWithTracking[,paste0("N",priorNode)])==FALSE, paste0("N",priorNode)]
        theMin = min(theTempData)
        startNode = which(dataWithTracking[,paste0("N",priorNode)]==theMin)
        startNode = startNode[1]
        
        theTrackingMatrix[1,j+26] = theIndex[startNode-1]
        theTrackingMatrix[2,j+26] = theMin
        dataWithTracking[startNode,1:ncol(dataWithTracking)] = NA
        
      } else {
        
        theTrackingMatrix[1,j+26] = 1
        theTrackingMatrix[2,j+26] = theData[priorNode,1]
        
      }
      
    }
    
    
    # Final results
    theTrackingMatrix[1,j+1+26] = sum(theTrackingMatrix[2,])
    theTrackingMatrix[,"Split"] = k
    trackingOutput = rbind(trackingOutput, theTrackingMatrix[1,])
    
  }
  
}

View(trackingOutput)

bestChoice = trackingOutput[which(trackingOutput[,'Total'] == min(trackingOutput[,'Total'])), ]
bestPathofChoice = as.data.frame(bestChoice)

#Comments:

##D1 refers to driver 1 and D2 refers to driver 2 (Both drivers return home after completing their drop off points)
##Best Choices refer to the nodes they are traveling to 
##L1-25 are the choices that the drivers have a total of ... for example, D1:L7 means Driven 1 on his/her 7th move will go to node 20. 
## The split refers to the first node the algorithm looked to in order to find the nearest 25 neighbors 

View(bestPathofChoice)



# *******************************************************Part 3: Visualization************************************************************************#

# Now, I'm going to visualize all locations using the Leaflet Javascript library to solidify our findings 
# We cannot draw a path as any map api would have multiple paths to one location; finds are based on distance



# Let us match the nodes to a location which gives us their longitude/latitude points. 
PathTograph = matrix(0, nrow = 52, ncol = 4)
colnames(PathTograph) = c("Name","Path","Longitude","Latitude")
PathTograph = as.data.frame(PathTograph)



PathTograph[,'Path'] = bestPathofChoice[1:52,]

for (i in (1:52)) {
  
  id = PathTograph$Path[i] 
  myindex = My_Data_Final$Rank==id
  
  PathTograph[i,'Longitude'] = as.numeric(as.character(My_Data_Final[myindex, "LongAway"]))
  PathTograph[i,'Latitude'] = as.numeric(as.character(My_Data_Final[myindex, "LatAway"]))
  PathTograph[i,'Name'] = as.character(My_Data_Final[myindex, "Name"])
}


#check that Long and Lat are not characters 
#summary(PathTograph)

PathTograph.SP = SpatialPointsDataFrame(PathTograph[,c(3,4)], PathTograph[,-c(3:4)])



PathGraph <- leaflet() %>%
  addTiles() %>%
  addMarkers(data= PathTograph, lng = ~Longitude, lat = ~Latitude, popup = ~Name)



PathGraph


