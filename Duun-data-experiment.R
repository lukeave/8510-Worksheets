library(tidytext)
library(tidyverse)
library(readtext)
library(tm)
library(topicmodels)
library(ggplot2) 
library(DigitalMethodsData)
library(ggmap)
library(tidygeocoder)
library(leaflet)
library(visNetwork)
library(igraph) #load the necessary libraries
library(dplyr)
library(tidyr)

bio.data <- read.csv("~/Bio Table, March 2023.csv")
service.data <- read.csv("~/Service Table March 2023.csv")
relatives.data <- read.csv("~/Relatives Table March 2023.csv")
unnamed.data <- read.csv("~/Unnamed LIW Table, March 2023.csv")

##Preprocessing the data

#subset the bio data to include only ID, surname, and forename
nodes <- bio.data %>% 
  select(LIWID, Surname.Std, Forename.Std)
#subset the service data to include only the ID, household type, and roayalty
edges <- service.data %>% 
  select(LIWID, HouseholdifRoyal, ServiceType)

#change the class of LIWID in edges from numeric to character
nodes$LIWID <- as.character(nodes$LIWID)
edges$LIWID <- as.character(edges$LIWID)

#format the nodes
nodes$id <- paste(nodes$Forename.Std, nodes$Surname.Std, sep = " ")
nodes <- nodes %>% 
  select(LIWID, id)

#create a new operator because I forgot the one that already exists for this
'%!in%' <- function(x,y)!('%in%'(x,y))
#find every ID in edges that does not exist in nodes
for (x in 1:length(edges$LIWID)) {
  if (edges$LIWID[x] %!in% nodes$LIWID) {
    print(edges$LIWID[x]) 
  }
}
#result was: 313.01, 314.01, 734, and 1115.01 are in edges but not in nodes
##get rid of mismatches
which(edges=="313.01",arr.ind=TRUE)
edges[1803,1] = 313 #confirmed 313 and 313.01 both referred to Margaret Hevenyngham;  replaced 313.01 for 313
which(edges=="314.01",arr.ind=TRUE)
edges[1805,1] = 314 #confirmed that 314 and 314.01 both referred to Agnes Martyn; replaced 314.01 for 314
which(edges=="734",arr.in=TRUE)
edges <- edges[-2541,] #ID 734 referred to Matilda de Waltham in edges, but Matilda de Waltham does not show up in nodes data; removed row.
which(edges=="1115.01",arr.ind=TRUE)
edges <- edges[-3531,] #ID1115.01 referred to uxori Rog Mortuamari in edges, but individual does not show up in nodes data; removed row.

#format the edges
##customize the edges by color
edges <- edges %>% 
  mutate(title = edges$ServiceType[1:3972]) %>% 
  mutate(color = ifelse(edges$ServiceType == "RewardorGrant", "#FF0000",
                        ifelse(edges$ServiceType == "Duty", "#66FFFF",
                               ifelse(edges$ServiceType == "Placement", "#33FF33",
                                      ifelse(edges$ServiceType == "Travel", "#FF9933",
                                             ifelse(edges$ServiceType == "Livery", "#FFFF00",
                                                    ifelse(edges$ServiceType == "pp", "#9933FF",
                                                           ifelse(edges$ServiceType == "Other", "#CCCCCC", "#666666")))))))) 

edges <- edges %>% 
  left_join(nodes, edges, by=c("LIWID"="LIWID"), multiple = "all")
edges <- edges[, c(6, 2, 3, 4, 5, 1)]

colnames(edges)[1] <- "from"
colnames(edges)[2] <- "to"

#include the names of the royalties (edges[, 2]) in new nodes list
new.nodes <- c(edges$to, nodes$id) %>% 
  as.data.frame()

colnames(new.nodes)[1] <- "id"

new.nodes <- new.nodes[!duplicated(new.nodes$id), ] %>% 
  as.data.frame()

colnames(new.nodes)[1] <- "id"

new.nodes <- new.nodes %>% 
  filter(id != "" & id != " ")

#count the amount of service types per id/person
edges <- edges %>% 
group_by(LIWID, ServiceType) %>% 
mutate(count = n())

#exclude the NA values in both nodes and edges data
edges <- na.exclude(edges)
new.nodes <- na.exclude(new.nodes)

#customize the nodes by shape 
##I wish the data provided the gender for each person so we could symbolize them differently, but for now the only representation for the nodes will be general dots.
new.nodes <- new.nodes %>%
  mutate(title = (new.nodes$id[1:1239])) %>%
  mutate(shape = "circle") 

#make data frames for node and edge legends  
ledges <- data.frame(color = c("#FF0000", "#66FFFF", "#33FF33", "#FF9933", "#FFFF00", "#9933FF", "#CCCCCC"),
                     label = c("Reward or Grant", "Duty", "Placement", "Travel", "Livery", "pp", "Other"))
lnodes <- data.frame(label = new.nodes$id)

#create network visualization
visNetwork(new.nodes, edges, height = 650, width = 1100) %>%
  visIgraphLayout(layout = "layout_with_fr", randomSeed = 12) %>% #layout by family structure
  visNodes(size = 6, shape = new.nodes$shape, 
           label = new.nodes$id,
           color = "orange",
           title = new.nodes$id,
           font = list("size" = 10)) %>%
  visEdges(color = list(color = edges$color, opacity = 0.2),
           selectionWidth = 6,
           scaling = list(count = edges$count)) %>%
  visOptions(highlightNearest = list("degree" = 1),
             nodesIdSelection = TRUE) %>%
  visInteraction(dragNodes = TRUE,
                 hover = TRUE,
                 hoverConnectedEdges = TRUE,
                 multiselect = TRUE) %>%
  visLegend(addEdges = ledges, addNodes = lnodes, useGroups = FALSE, width = 0.12, position = "left")
