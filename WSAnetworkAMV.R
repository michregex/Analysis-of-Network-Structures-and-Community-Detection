library(dplyr)
library(purrr)
library(data.table)

setwd("/Users/micheledefanis/Desktop/network/esamenetwork")

#karatelinks is just for testing 
# edgeTable <- read.table("Karate_Links.txt", col.names = c('source', 'target'))
# edgeTable$weight <- rep(1, 78)

#load edge table 
edgeTable <- read.csv("edgeTableStatsUnlabel.csv", head = TRUE)
edgeTable <- arrange(edgeTable, source) 

#this function returns vertices as vector given an edge table 
#to find the vertices we find all unique nodes in the edge table
getVertices <- function(edgeTable) {
  
  edgeTable <- data.frame(edgeTable)
  
  sources <- edgeTable[,1] 
  targets <- edgeTable[,2] 
  
  vertices <- sort(union(sources, targets))
  
  return(vertices)
  
}
vertices <- getVertices(edgeTable)

#this function returns degrees of each node as vector given an edge table 
#to find the degrees we count how many times each node appears on the edgetable
getDegrees <- function(edgeTable) {
  
  edgeTable <- data.frame(edgeTable)
  
  sources <- edgeTable[,1] 
  targets <- edgeTable[,2]  
  totalNodes <- c(table(c(sources, targets)))
  
  return(totalNodes)
  
}
degrees <- getDegrees(edgeTable)

#extract a random community to test modularity function
communities <- sample(0:1, size = length(vertices), replace = TRUE) 

#This function returns the weighted degrees of each node
#it works like the getDegree() function, but instead we sum the weights for that node
getWeightedDegree <- function(edgeTable, vertices) {

edgeTable <- data.frame(edgeTable)
  
degreeW <- vector (mode = 'numeric', length = length(vertices))

  for (i in 1:length(vertices)) {
  
    degreeW[i] <- sum(subset(edgeTable, source == vertices[i] | target == vertices[i])$weight)
  
    }

return(degreeW)

}
degreeW <- getWeightedDegree(edgeTable, vertices)
weightedDF <- data.table(vertices, degreeW)

#this function computes weighted modularity given an edge table, a vector with the communities
#for each node, vector of vertices and the weighted degree vector and the sum of all weights
#the object of this function is to calculate 2 quantities named erW and arW2
#erW =  total weights within that community / totalWeight (this quantity considers only the links between nodes of that community)
#arW2 = [total weights of that community / (2 * totalWeight)]^2 (this quantity takes into account also the link with nodes of other communities)
modularityWeighted <- function(edgeTable,community, vertices, totalWeight, degreeW) {
  
  weightedDF <- data.table(vertices, degreeW, community = community)
  
  cNames <- sort(unique(community))
  
  erW <- vector(mode = 'numeric', length = length(cNames))
  
  #in this part we filter the edgetable with the community subnodes
  for (i in 1:length(cNames)) {
    
    erW[i] <- sum(
      subset(edgeTable, source %in% weightedDF$vertices[weightedDF$community==cNames[i]] 
             & target %in% weightedDF$vertices[weightedDF$community==cNames[i]])$weight
    ) / (totalWeight)
    
  }
  
  arW2 <- vector(mode = 'numeric', length = length(cNames))
  
  for (i in 1:length(cNames)) {
    
    arW2[i] <- (sum(subset(weightedDF, community == cNames[i])[,2]) / (2 * totalWeight))^2
    
  }
  
  sum(erW - arW2) #modularity
  
}

totalWeight <- sum(edgeTable$weight)
mod0 <- modularityWeighted(edgeTable, communities, vertices, totalWeight, degreeW)
modularityWeighted(edgeTable, communities, vertices, totalWeight, degreeW)
######compute multiple modularities to estimate cooling parameters######
modularities <- c()

totalWeight <- sum(edgeTable$weight)
degreeW <- getWeightedDegree(edgeTable, vertices)
vertices = getVertices(edgeTable)

for (i in 1:100) {
  
  communities <- sample(0:1, size = length(vertices), replace = TRUE) 
  
  modularities[i] <- modularityWeighted(edgeTable, communities, getVertices(edgeTable), totalWeight,degreeW)
  
}

allDiff <- combn(modularities,2, FUN = \(x) abs(diff(x)) ) #find all pairwise combination of 2 and do the absolute of the differences
hist(allDiff) #histogram of the absolute values of the differences

#######set parameters to test simulated annealing#########
nSims <-  10000
startingTemp <- 10
finalTemp <- mean(allDiff) / 10000
beta <- -log(finalTemp / startingTemp) / nSims

#define function to decrease temperature (exponential cooling)
exponentialDump <- function(startingTemp, beta, n) { #n is the n-th iteration in the loop for
  
  temp = startingTemp * exp(-beta * n)
  
  return(temp)
  
}

#This function applies the simulated annealing to partition one community
simAnn2Comm <- function(edgeTable, startingTemp = 10, finalTemp, nSims) {
  
  edgeTable <- data.table(edgeTable)
  
  allMod <- c() #this vector will store optimal modularities
  
  vertices <- getVertices(edgeTable) 
  totalWeight <- sum(edgeTable$weight)
  degreeW <- getWeightedDegree(edgeTable, vertices)
  weightedDF <- data.table(vertices, degreeW)
  
  community <- sample(0:1, size = length(vertices), replace = TRUE) #generate first random community (0, 1 labels)
  
  mod0 <- modularityWeighted(edgeTable, community, vertices, totalWeight, degreeW) #starting modularity
  
  beta <- -log(finalTemp / startingTemp) / nSims
  
  #main for loop
  for (i in 1:nSims) {
    
    randomNode <- sample(length(vertices), size = 1) #node index to replace to generate the nearby solution
    commNode <- community[randomNode] #community label to swap
    
    #swap the value of the selected node
    commNode <- ifelse(commNode == 0, 1,0)
    
    newCommunity <- community #define the vector of the nearby solution        
    newCommunity[randomNode] <- commNode #swap one the node
    
    mod1 <- modularityWeighted(edgeTable, newCommunity, vertices, totalWeight, degreeW) #new modularity
    
    if (mod1 >= mod0) { #if mod1 is better we switch to the newCommunity
      community <- newCommunity
      mod0 <- mod1
    } else { #else we switch to the worst community with a certain probability
      
      temp = exponentialDump(startingTemp, beta, n = i)
      modDiff = mod0 - mod1 
      prob = exp(-modDiff / temp)
      
      randomNum <- runif(1, min = 0, max = 1)
      
      if (randomNum < prob) {
        
        community <- newCommunity
        mod0 <- mod1
        
      }
      
    }
    
    allMod[i] <- mod0 #store mod
    
    cat("\r", i, "of", nSims) #keep track of the iterations
    
  }
  
  #final output, all modularities, best partition and final modularity
  outputSA <- list(modularities = allMod, optCommunity = community, optMod = mod0)
  
  return(outputSA)
  
}

#test SA
outputSA <- simAnn2Comm(edgeTable, startingTemp = 10, finalTemp = finalTemp, nSims = 1000)

plot(x = 1:length(outputSA$modularities), y = outputSA$modularities, type = 'l')
outputSA$optMod

#we now adapt for more than 2 communities 
#this function gives the number of iterations needed for the simulated annealing given the number of nodes 
shortNsims <- function(Nnodes) {
  
  if (Nnodes<=50) {return(2500)}
  
  else if (Nnodes<=100) {return(5000)} else {return(Nnodes * 50)}
  
}

#this function is applied on the output of SA, if there are isolated nodes in the community 0 it
# swaps them in community 1, then we repeat for community 1 by swapping the isolated nodes in community 0
#to do this, first we detect the nodes that are not isolated, then to find the isolated one we 
#apply the difference (set operation) between al the nodes of that community with the vector of nodes that are connected
#the inputs are an edge table and the vector denoting the community of each node (comSA)
swapIsolatedW <- function(subEdgeTable, comSA) {
  
  subEdgeTable <- data.frame(subEdgeTable)
    
  nodesCom <- data.frame(nodes = getVertices(subEdgeTable), comSA = comSA)
  
  sourceCom <- left_join(subEdgeTable, nodesCom, join_by(source == nodes))
  targetCom <- left_join(subEdgeTable, nodesCom, join_by(target == nodes))
  
  sourceCom$targetCom <- targetCom[,4] #new column
  
  dfIsolated <- sourceCom #dataframe with source/target/weight/sourceCommunity/targetCommunity columns
  
  colnames(dfIsolated) <-  c('source', 'target','weight', 'sourceCom', 'targetCom')
  
  dfConnected <- dfIsolated[dfIsolated$sourceCom == dfIsolated$targetCom,] #vector of connected nodes
  
  IsolatedCandidates <- setdiff(getVertices(subEdgeTable), unique(c(dfConnected[,1],dfConnected[,2])) ) #vector of isolated nodes (diff between al nodes and isolated ones)
  
  #the next line swap the isolated nodes into the opposite community
  nodesCom[nodesCom$nodes %in% IsolatedCandidates,]$comSA <- ifelse(nodesCom[nodesCom$nodes %in% IsolatedCandidates,]$comSA == 1,0,1)
  
  
  #we repeat the same operation for the other community
  sourceCom <- left_join(subEdgeTable, nodesCom, join_by(source == nodes))
  targetCom <- left_join(subEdgeTable, nodesCom, join_by(target == nodes))
  
  sourceCom$targetCom <- targetCom[,4]
  
  dfIsolated <- sourceCom
  
  colnames(dfIsolated) <- c('source', 'target','weight', 'sourceCom', 'targetCom')
  
  dfConnected <- dfIsolated[dfIsolated$sourceCom == dfIsolated$targetCom,]
  
  IsolatedCandidates <- setdiff(getVertices(subEdgeTable), unique(c(dfConnected[,1],dfConnected[,2])) )
  
  nodesCom[nodesCom$nodes %in% IsolatedCandidates,]$comSA <- ifelse(nodesCom[nodesCom$nodes %in% IsolatedCandidates,]$comSA == 0,1,0)
  
  return(nodesCom$comSA)
  
} 

#this is the main function used to optimize modularity, in the first part before the 
#while loop we define some object used to carry information through the code
modularityOptimization <- function(edgeTable) {
  
  modPre <- -10000
  modNext <- 0 
  
  optModul <- c() #this will store the current best modularity
  completeMod <- NULL #list containing lists of modularities
  
  communityDF <- list() #will store best partions of each community that we want to compare
  newCommunities <- c(0) #will store the labels of the new communities [IMPORTANT]
  oldSA <- list() #will store each community obtained through the SA [IMPORTANT]
  
  vertices = getVertices(edgeTable)
  totalWeight <- sum(edgeTable$weight)
  degreeW <- getWeightedDegree(edgeTable, vertices)
  
  C = 0 #initial number of communities 
  communityRaw <- rep(0, length(degreeW)) #initial community labels (all 0s), this vector is used to connect various part of the code
  communitiesUpdated <- communityRaw #this is the vector that we will update after SA
  
  while (TRUE) { #the condition to break the while loop is later on
    
    modPre <- modNext
    
    for (c in 0:C) { #we start the for loop to iterate over each community
      
      #in this part we build the subedgetable by subsetting for the c community
      subNodes <- which(communityRaw == c) #get c subset community indexes 
      
      sourceIndex <- which(edgeTable[, 'source'] %in% subNodes) 
      targetIndex <- which(edgeTable[, 'target'] %in% subNodes) 
      
      subEdgeTable <- edgeTable[intersect(sourceIndex, targetIndex),] 
      subEdgeTable <- arrange(subEdgeTable, source) 
      
      #if c is in newCommunities we apply the simulated annealing    
      if (c %in% newCommunities) {
        
        cat('\n', 'Apply simulated annealing for community', c,'\n') 
        
        nSims <- shortNsims(length(getVertices(subEdgeTable))) #choose the number of iterations is the SA
        
        #Apply SA
        outputSAc <- simAnn2Comm(subEdgeTable, startingTemp = 10, finalTemp = finalTemp, nSims)
        
        #swap isolated nodes
        swappedSA <- swapIsolatedW(subEdgeTable = subEdgeTable, comSA =  outputSAc$optCommunity)
        
        #stores all modularities using nested list
        completeMod <- append(completeMod, list(list())) #surely there are better solutions to do this but it works for us and there are no reason to change it
        completeMod[[C+1]][[which(newCommunities %in% c)]] <-outputSAc$modularities #the which() is used to index the list in the correct way (1 and then 2)
        
        oldSA[[c+1]] <- swappedSA #we store the best partition (vector of 0s and 1s) so we don't need to do it again
        
        communityReLabeled <-  swappedSA #vector to relabel
        
        for (i in 1:length(communityReLabeled)) {
          
          condition <- communityReLabeled[i]==0
          
          communityReLabeled[i] <- ifelse(condition,c,C+1) #if the the i-th node is 0 then we label it as c else as C+1 
          
        }
        
        communitiesUpdated <- communityRaw 
        
        communitiesUpdated[subNodes] <- communityReLabeled #substitute the subnodes with the subnodes relabeled
        
        communityDF[[c+1]] <- communitiesUpdated #store the c solution [c + 1 because our labels start from 0]
        
      } else { #do this if c doesn't belong to new communities
        
        cat('\n', 'Adapt previous partition of',c)
        
        #in this case we've already found the best partition for community c and it's stored in oldSA
        communityReLabeled <-  swapIsolatedW(subEdgeTable, oldSA[[c+1]])
        
        #this part is the same as before
        for (i in 1:length(communityReLabeled)) {
          
          condition <- communityReLabeled[i]==0
          
          communityReLabeled[i] <- ifelse(condition,c,C+1)
        }
        
        communitiesUpdated <- communityRaw
        
        communitiesUpdated[subNodes] <- communityReLabeled
        
        communityDF[[c+1]] <- communitiesUpdated
        
        
      }
      
    }
    
    #we compute the modularity for al solutions obtained by the partition of each community
    modVector <- map_dbl(1:length(communityDF), 
                         \(i) modularityWeighted(edgeTable, community = communityDF[[i]], vertices, totalWeight, degreeW))
    
    #store the best modularity
    modNext <- max(modVector)
    
    #this is what breaks the while loop, if the previous modularity is better than the new one
    if (modPre >= modNext) {break}
    
    cat('\n')
    cat('-----------------------------', '\n',
        'Number of communities:',C+2, '\n', #C starts from 0 so after the first partition we have 2 communities (C+2)
        'Current modularity:',modNext, '\n',
        '-----------------------------', '\n')
    
    optimalCommunityNext <- communityDF[[which.max(modVector)]] #store the best partition (vector)
    
    C <- C + 1 #update C
    
    communityRaw <- optimalCommunityNext #update
    
    newCommunities <- c(which.max(modVector) - 1,C) #Labels of the newCommunities [IMPORTANT]
    
    optModul[C] <- modNext #best modularity to output
    
  } 
  
  #output: best modulairty, all modularity, best partition
  outputModOpt <- list(optModul, completeMod, optimalCommunityNext)
  
  return(outputModOpt)
  
}

result <- modularityOptimization(edgeTable) #332s on my pc

#Modularity plot
plot(x = 1:length(result[[2]][[1]][[1]]), y =result[[2]][[1]][[1]], type = 'l') #plot for the first partition
plot(x = 1:length(result[[2]][[3]][[2]]), y =result[[2]][[3]][[2]], type = 'l') #plot for the partion of 1 group while we have 3 communities

####Network statistics with igraph package####
library(igraph)
library(ggplot2)

#load the nodeTable
node <- read.csv("nodeTableStats.csv",  head = TRUE)

edgeTable <- left_join(edgeTable, node, by = join_by(source == Id))
edgeTable <- left_join(edgeTable, node, by = join_by(target == Id))
edgeTable <- edgeTable %>% rename('sourceID' = 'label.x', 'targetID' = 'label.y')

#build igraph object
statsGraph <- graph_from_data_frame(edgeTable[,1:2], directed = FALSE, vertices = node)

######Centrality measures study#######
#eigenvector centrality (weighted)
eigenCentrality <- eigen_centrality(statsGraph, directed = FALSE, scale = TRUE, weights = edgeTable[,3] )
eigenCentrality <- data.frame(eigenCentrality = eigenCentrality$vector)

#make a ranking according to Weighted degree and Eigenvector centrality
rankEG <- cbind(eigenCentrality,label1 = node$label) %>% arrange(desc(eigenCentrality)) %>% mutate(rankingEG = c(1:1000))
rankDW <- data.frame(degreeW, label2 = node$label) %>% arrange(desc(degreeW)) %>% mutate(rankingDW = c(1:1000))

#make differences between ranking
rank <- merge(rankEG, rankDW, by.x = "label1", by.y = "label2",suffixes = c("_1", "_2")) %>% 
  mutate(changes = rankingDW - rankingEG) %>% 
  arrange(desc(changes))

rank %>% arrange(rankingDW) #ranking sorted by weighted degree

#mean degree
mean(getDegrees(edgeTable))
#mean weighted degree
mean(getWeightedDegree(edgeTable,vertices = getVertices(edgeTable)))

#global clustering coefficient
transitivity(statsGraph, type = 'global') #we used this

#graph density
edge_density(statsGraph) 

#histogram degrees with custom bins 
degrees <- getDegrees(edgeTable)
degrees <- data.frame(degree = degrees)

plotDegrees <- ggplot(degrees, aes(x = degree)) + geom_histogram(breaks = c(seq(1,150, by = 15), 151, 200, 361),color = "#000000", fill = "#0099F8") +
  xlab('Degrees') + ylab('Number of nodes') +
  theme_bw()

plotDegrees

####Comparison of our partitions with the gephi one####
partitions <- read.csv("partitions.csv",  head = TRUE)

modularityWeighted(edgeTable[,1:3],partitions$sacom, vertices,totalWeight,getWeightedDegree(edgeTable,vertices)) #out modularity
modularityWeighted(edgeTable[,1:3],partitions$modularity_class, vertices,totalWeight,getWeightedDegree(edgeTable,vertices)) #gephi modularity

partitions$sacom <- factor(partitions$sacom, levels = c(0:6))
partitions$modularity_class <- factor(partitions$modularity_class,levels = c(0:6))

library(aricode)
#gephi comparison
measures <- clustComp(partitions$sacom,partitions$modularity_class)
randIndex <- measures$RI
mutualInformation <- measures$MI

####top 10 nodes for each community plot####
partitions %>% mutate(weights = degreeW) %>% 
  group_by(modularity_class) %>%
  slice_max(weights,n=10) %>% 
  mutate(Label = forcats::fct_reorder(Label, weights)) %>% 
  ggplot(aes(x=Label, y=weights)) +
  geom_segment(aes(x=Label, xend=Label, y=0, yend=weights), color="grey") +
  geom_point(color="orange", size=4) +
  facet_wrap(~modularity_class, scales = 'free') +
  coord_flip() + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  xlab("") +
  ylab("Weighted Degree")


