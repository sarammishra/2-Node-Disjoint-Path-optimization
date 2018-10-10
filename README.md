# Nearest-Neighbor-Path-optimization
Using a nearest neighbor approach, this method runs 1250 scenarios to find a near-optimal combination of 2 disjoint paths. 

-------------------
GENERAL INFORMATION
-------------------
Utilizing a nearest-neighbor approach, I sought to find the most optimized path for a two separate trucks (2-node disjoint) which not only cover all of the 50 drop-off points but also return home. This was written in R. 

To run: please open OptimalSolution.R in (preferable RStudio) or any R Ide … select all and run script. The outputs should be two additional tables: bestPathofChoice and trackingOutput as well as a simple Leaflet map identifying all points. 

Packages/Libraries Used	
-------------------
library(leaflet)
library(fields)
library(dplyr)
library(tidyverse)
library(tidyr)
library(data.table)
library(sp)
-------------------
Methodology	
-------------------
Utilizing loops, I was able to first split the location data amongst the two drivers and then send them in their routes. The logic being: if a driver goes to any arbitrary location as their second node, based on how many other locations are nearby, which combination would result in the shortest distance traveled.   For example, if a driver goes from home base to node 2, or the shortest distance away from home, then what area could they cover most efficiently and then return home while the other driver goes to another arbitrary location and covers the nodes that driver 1 did not cover in the route? The path no longer becomes necessarily linear as a Dijkstra algorithm would suggest but is based entirely on nearest neighbors within an arbitrary radius constrained by always finding the shortest total distance.

I utilized a trackingMatrix to keep track of which node at what iteration did each driver go to. All in all, my tracking output ran 1250 different iterations to find the shortest path traveled in total by both truckers. 

Finally, I sought to do a little bit of visualization to logically check my findings utilizing the Leaflet JS library. 




