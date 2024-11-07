library(httr)
library(jsonlite)
library(purrr)
library(widyr)

test_movies <-GET("https://api.stackexchange.com/2.3/questions?page=25&pagesize=100&order=desc&sort=activity&site=movies")

test_movies$status_code
test_movies_content <- content(test_movies)

#need for the following while loop
statusCode = 200 
completeContent <- list()
index = 1 #page number, max 25 without api key

###while loop to fetch the data, this version is without our api key for obvious reasons, so there is a limit on the requests
while (statusCode == 200 & index <=20) {
    
  url = paste("https://api.stackexchange.com/2.3/questions?page=", index, "&pagesize=100&order=desc&sort=activity&site=stats", sep = "")
  getCall <-  GET(url)
  
  statusCode <- getCall$status_code
  
  print(statusCode)
  print(index)
  
  moviesContent <- content(getCall)
  
  completeContent[[index]] <- moviesContent 
    
  index = index + 1
  
  Sys.sleep(1)
  
}

#this function extract all the tags from the questions
fetchTagsLong <- function(completeContent) {

  tagsAsList <- list()
  for (l in 1:((length(completeContent)-1))) {
  
    tagsAsList[[l]] <- map(1:length(completeContent[[l]]$items), function(i) do.call(cbind, completeContent[[l]]$items[[i]]$tags))
  
                  }

  tagsAsList <- do.call(base::c,tagsAsList)

  numberTags <- map_vec(1:length(tagsAsList), function(i) length(tagsAsList[[i]]))
  
  tagsAsVector <- do.call(base::c,map(1:length(tagsAsList), function(i) c(tagsAsList[[i]])))
  
  tagsDf <- data.frame(question = rep(1:length(tagsAsList),numberTags), tags = tagsAsVector)

  return(tagsDf)

}

question <- fetchTagsLong(completeContent)

#this function count how many times 2 tags appear in one question
returnPairs <- function(tagsDf) {
  
  pairDf <- pairwise_count(tagsDf, tags, question, sort = TRUE)
  
  pairDf <- pairDf[!duplicated(t(apply(pairDf, 1, sort))), ]
  
  colnames(pairDf) <- c('Source', 'Target', 'Weight')
  
  return(pairDf)
  
}

networkData <- returnPairs(fetchTagsLong(completeContent))  

#we collected 30 pages for out network

#build node table
fromEdgetoNode <- function(edgeTable) {

  sourceCol <- edgeTable$Source
  targetCol <- edgeTable$Target

  tags <- c(sourceCol,targetCol)
  tags <- unique(tags)

  nodeTable <- data.frame(node = 1:length(tags), tags = tags)

  return(nodeTable)

}


nodeTable <- fromEdgetoNode(networkData)

fromLabelsToNumbers <- function(edgeTable,nodeTable) {

  part1 <- left_join(edgeTable, nodeTable, by = join_by(Source == tags))
  part2 <- left_join(part1, nodeTable, by = join_by(Target == tags))
  
  edgeTableNum <- part2 %>% select(node.x, node.y, Weight) %>% rename('Source' = node.x, 'Target' = node.y)
  
  return(edgeTableNum)
  
}

EdgetableNumber <- fromLabelsToNumbers(networkData,nodeTable)

# if we gather the data today it will be different than ours