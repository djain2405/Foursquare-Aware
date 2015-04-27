#########################################################################################################
# DATASETS NEEDED FOR RUNNING SCRIPT

# Processed data 1 - user ID - Sub categories.txt
# Processed_original_data.csv
# Processed data 2 - user ID Categories.csv
# Categories.csv
#########################################################################################################

setwd("/Users/rickyh/Documents/HWNotes/CSC522/Project")

library(fpc)
library(mclust)
library(kknn)
library(flexclust)
library(cluster)
library(class)
library(rpart)
library(lsa)
library(plyr)
library(RCurl)
library(RJSONIO)

#########################################################################################################
# load original check in data to find frequencies of each venue
#
#########################################################################################################

preprocessOriginalData <- function(){
  
  #load original data
  originalData <- read.csv("Processed_original_data.csv",head=TRUE)
  names(originalData) <- c("UserID","VenueID","VenueCategoryID","Subcategory","Category")
  #View(originalData)
  return(originalData)
}


preprocessFrequencies <- function(originalData){
  # sorting venues according to frequencies
  frequencies <- table(originalData$VenueID)
  #View(frequencies)
  frequencies <-as.data.frame(frequencies)
  frequencies<-frequencies[order(-frequencies$Freq),]
  return(frequencies)
}


#########################################################################################################
# Pre-processing, separating training and test data, clustering, building model using training data
# and predicting class labels for test data
#########################################################################################################

obtainClusters <- function(){
  
  #load the data (had user ID and 9 sub categories)
  grouped_data <- read.table("Processed data 2 - user ID Categories.csv",sep=",",head=TRUE, fileEncoding="UTF-8")
  
  #preprocessing: To get better clusters, set value to 0 or 1 based on median value
  for(j in 2:ncol(grouped_data))
  {
    for (i in 1:nrow(grouped_data))
    {
      grouped_data[i,j] <- if(grouped_data[i,j] > mean(grouped_data[,j], trim = 0.1)) 1 else 0;
    }
  }
  
  #extract last 13 users for testing and delete them from dataset
  test_data <- grouped_data[c(1071:1083),]
  training_data <- grouped_data[-c(1071:1083),]
  
  
  #kmeans clustering with 6 clusters
  #remove user ID column before running clustering algorithm
  kmeans_result <- kmeans(x = scale(training_data[,-c(1)]),centers = 6, nstart = 1, )
  clusplot(training_data, kmeans_result$cluster, col.p = kmeans_result$cluster)
  
  #add the class label to the grouped data to run a classification algorithm
  training_data$class <- as.factor(kmeans_result$cluster)
  
  #build a decision tree classifier
  class_tree <- rpart(training_data$class ~., data = training_data[,c(2:10)],
                      method = "class", parms = list(split='information'),
                      control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
  
  #predict a cluster for the 3 test points
  predicted_cluster <- predict(class_tree, test_data )
  classes <- apply(predicted_cluster, 1, which.max)
  return(list(grouped_data,training_data,classes))
}


#####################################################################################################
# using sub category data finding similar user from users in the test user's predicted class
#
#####################################################################################################

findSimUser <- function(grouped_data, training_data, classes){
  
  #load the user dataset with the 251 attribs (sub category data)
  grouped_data_sub_cat <- read.table("Processed data 1 - user ID - Sub categories.txt",sep=",",head=TRUE)
  
  #TODO:
  #get the records of all users belonging to predicted cluster, and also the test user's record
  #use some distance metric and find a similar user (or few similar users?)
  #recommend place
  
  
  #extract last 13 users for testing and delete them from dataset
  test_data_sub_cat <- grouped_data_sub_cat[c(1071:1083),]
  grouped_data_sub_cat <- grouped_data_sub_cat[-c(1071:1083),]
  
  
  
  # data frame that holds similarities and similar users for test users
  cosineTop <- data.frame(numeric(0), numeric(0), numeric(0))
  colnames(cosineTop) <- c("SimUser","Sim","User")
  
  for (j in 1:nrow(test_data_sub_cat)){
    # subset of users in the same class/cluster
    groupedData_subset <- grouped_data[training_data$class==classes[j],1]
    groupedData_subset <- grouped_data_sub_cat[groupedData_subset,]
    
    # vector to hold similarities with all users
    a <- rep(NA, nrow(groupedData_subset))
    groupedData_subset
    
    for(i in 1:nrow(groupedData_subset)){
      a[i] <- cosine(unlist(groupedData_subset[i,-1]),unlist(test_data_sub_cat[j,-1]))
    }
    
    answer<-data.frame(groupedData_subset[,1],a)
    answer <- answer[order(-a),]
    answer <- answer[1:3,]
    answer$User <- j
    cosineTop <- rbind(cosineTop,answer)
  }
  return(cosineTop)
}


###############################################################################################
# Identify top 3 similar users in predicted cluster/class
# Identify top 3 venues which were visitedby similar users but not by test user
# Recommend those
###############################################################################################

recommend <- function(userNum, originalData, cosineTop, frequencies, category) {
  
  j <- userNum
  # places visited by test user
  testUserData <- originalData[originalData$UserID==1070+j & originalData$Category==category,]
  testUserData <- testUserData$VenueID
  testUserData <- unique(testUserData)
  #View(testUserData)
  # places visited by similar user
  similarUserData <- data.frame()
  for(i in 1:3){
    similarUserData2 <- originalData[originalData$UserID==cosineTop[3*(j-1)+i,1] & originalData$Category==category,]
    similarUserData2 <- similarUserData2$VenueID
    similarUserData <- rbind(similarUserData,as.data.frame(similarUserData2))
  }
  
  # keep only unique data
  #View(similarUserData2)
  similarUserData <- unique(similarUserData)
  #View(similarUserData)
  
  # venues where test user has not visited
  unvisited <- subset(similarUserData, !similarUserData2 %in% testUserData)
  unvisited <- frequencies[frequencies$Var1 %in% unvisited$similarUserData2,]
  #View(frequencies)
  cat_subcat <- unique(originalData[,4:5])
  #View(cat_subcat[cat_subcat$Category==category,])
  
  #View(unique(cat_subcat$Category))
  #View(unvisited)
  unvisited <- unvisited[0:0,]
  unvisited <- unvisited[complete.cases(unvisited),]
  
  #if less than 3 suggestions
  if(nrow(unvisited) < 3){
    #all venues of that category
    allVenues <- unique(originalData[originalData$Category==category,]$VenueID)
    allVenues <- frequencies[frequencies$Var1 %in% allVenues,]
    # all venues of the category that are not visited by testUser
    allVenues <- allVenues[!allVenues$Var1 %in% testUserData,]
    # all venues of the category that are not already added to suggestions
    allVenues <- allVenues[!allVenues$Var1 %in% unvisited$Var,]
    # number of venues to be added
    x <- 3-nrow(unvisited)
    # add new venues to unvisited
    unvisited <- rbind(unvisited,allVenues[1:x,])
  }
  # contains the three top recommendations
  return(unvisited)
}




#####################################################################################################
# Query foursquare server to resolve name from venue ID
#
#####################################################################################################

getNames <- function(venues) {
  names <- NA
  for(i in 1:nrow(unvisit)){
    vId <- unvisit[i,]$Var1
    req <- paste("https://api.foursquare.com/v2/venues/",vId,"?oauth_token=XBVDFTILQDZ4TTQWF2HX0STKAP05W21SWSSL3RZ5KEV23KQA&v=20150411",sep="")
    res <- getURL(req)
    obj <- fromJSON(res)
    names[i] = obj$response$venue$name
  }
  return(names)
}


#####################################################################################################
# Main running code
#
#####################################################################################################

originalData <- preprocessOriginalData()
frequencies <- preprocessFrequencies(originalData)
someList <- obtainClusters()
grouped_data <- as.data.frame(someList[[1]])
training_data <- as.data.frame(someList[[2]])
classes <- as.vector(someList[[3]])
cosineTop <- findSimUser(grouped_data,training_data, classes)
categories <- read.csv("Categories.csv",head=FALSE)
categories <- as.character(categories[,1])
again = 'y'

while(again != 'n'){
  cat("\nEnter user ID: ")
  userNum <- scan(what=numeric(),nmax=1,quiet=TRUE)
  cat("\n
      1:  Shop & Service
      2:  Outdoors & Recreation
      3:  Residence
      4:  Professional & Other Places
      5:  Food
      6:  Travel & Transport
      7:  Arts & Entertainment
      8:  College & University
      9:  Nightlife Spot
      10: Athletic & Sport")
  
  cat("\nEnter category: ")
  category <- scan(what=numeric(),nmax=1,quiet=TRUE)
  category <- categories[category]
  unvisit <- recommend(userNum,originalData,cosineTop,frequencies,category)
  
  #Note: If you get SSL errors, comment this line 
  #also commment the line in the for loop.
  #and Uncomment the other line in for loop.
  withNames <- getNames(unvisit)
  
  cat("Top suggestions:\n")
  for(i in 1:nrow(unvisit)){
    cat(i,"] ",withNames[i],"\n",sep="")
    #cat(i,"] ",as.character(unvisit[i,]$Var1),"\n",sep="")
  }
  
  cat("\nDo you wish to continue (y/n): ")
  again <- scan(what=character(),nmax=1,quiet=TRUE)
}

