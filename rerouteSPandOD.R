## This program finds the duration, in minutes, and distance, in miles,
## to all reported I-90 destinations from Snoqualmie Pass given a
## departure time via the Google Maps Distance Matrix API

setwd("/Users/Bradley/Dropbox/...")

library(bitops)
library(httr)
library(XML)
library(RCurl)
library(rJava)
library(xlsxjars)
library(xlsx)
library(progress)

# read in data
input <- read.csv("inputRoutes.csv",header=F)
allData <- as.matrix(input)
colnames(allData) <- NULL
numRow <- nrow(allData)
snoqPass <- "Snoqualmie+Pass+WA"
drive <- "best_guess"
key <- "sign_up_for_a_key"

# takes the prepared city+state data and returns duration from origin to SP
originSP <- matrix(nrow=numRow,ncol=1)
pb <- progress_bar$new(total = numRow)
for(i in 1:numRow){
  k <- 0
  while(k<4){
    url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',allData[i,2],
                  '&destinations=',snoqPass,'&units=imperial')
    Sys.sleep(1)
    tie <- xmlParse(GET(url))
    tryCatch(originSP[i,1] <- as.numeric(xpathApply(tie,"//duration/value",xmlValue))/60, error=function(e) NULL)
    k <- k+1
  }
  pb$tick()
}

# takes the prepared city+state data and returns duration from SP to destination
SPdest <- matrix(nrow=numRow,ncol=1)
pb <- progress_bar$new(total = numRow)
for(i in 1:numRow){
  k <- 0
  while(k<4){
    url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',snoqPass,
                  '&destinations=',allData[i,3],'&units=imperial')
    Sys.sleep(1)
    tie <- xmlParse(GET(url))
    tryCatch(SPdest[i,1] <- as.numeric(xpathApply(tie,"//duration/value",xmlValue))/60, error=function(e) NULL)
    k <- k+1
  }
  pb$tick()
}

# takes the prepared city+state data and returns duration from Snoqualmie Pass given departure time
SPhour <- matrix(nrow=numRow,ncol=1)
pb <- progress_bar$new(total = numRow)
for(i in 1:numRow){
  k <- 0
  while(k<4){
    url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',snoqPass,
                  '&destinations=',allData[i,3],'&units=imperial&departure_time=',allData[i,4],
                  '&traffic_model=',drive,'&key=',key)
    Sys.sleep(1)
    tie <- xmlParse(GET(url))
    tryCatch(SPhour[i,1] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue))/60, error=function(e) NULL)
    k <- k+1
  }
  pb$tick()
}

# export to csv file
originalRoute <- cbind(allData,originSP,SPdest,SPhour)
colnames(originalRoute) <- c("west","origin","destination","epochtime","orginSP","SPdest","SPhour")
write.csv(originalRoute,file="originalRoute.csv")
