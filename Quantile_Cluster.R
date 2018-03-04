
rm(list=ls())

library(data.table)
library(magrittr)
library(ggplot2)

METHOD = 'quantile'
TRANSFORM <- F
CENTER <- F
SCALE <- F
NUMBER_OF_CLUSTERS <- 2


df <- readRDS("G:/Privat2Work/data.R") %>% as.data.table()
df <- df[,.(idCustomerOrigin, Sales, MargeP)]


getNumericCols <- function(df){
  which(sapply(df, is.numeric))
}

transformDF <- function(df, transform=T, center=T, scale=T){
  df[,1] <- lapply(df[,1, with=FALSE], as.character)
  df[,2:ncol(df)] <- lapply(df[,2:ncol(df),with=FALSE], as.numeric)
  
  if (transform){
    df[, `:=` (MargeP= MargeP*1, Sales = Sales*1)]
  }
  
  if (any(center, scale)){
    numericCols <- getNumericCols(df)
    df[, (numericCols) := lapply(.SD, function(x)
      scale(x, center=center, scale=scale)), .SDcols = numericCols]
  }
  df
}


createCuts <- function(x, numberOfClusters){
  quantiles <- quantile(x, seq(0,1,1/numberOfClusters), na.rm = TRUE)
  cuts <- cut(x, quantiles)
  as.integer(cuts)
}

combineClusters <- function(x){
  clusters <- paste0(x)
  clusters <- factor(clusters)
  as.integer(clusters)
}

createClusterCols <- function(df){
  numericCols <- getNumericCols(df)
  paste(names(numericCols), "Cluster")
}

getClusterCols <- function(df){
  which(endsWith(colnames(df), "Cluster"))
}

getIdOrCluster <- function(df){
  which(colnames(df) == "Cluster" | sapply(df, is.character))
}

quantileClustering <- function(df){
  numericCols <- getNumericCols(df)

  clusterCols <- createClusterCols(df)
  df[ ,c(clusterCols) := lapply(.SD, function(x)
    createCuts(x, numberOfClusters = NUMBER_OF_CLUSTERS)), .SDcols = numericCols]
  
  df[, Cluster := Reduce(function(...) paste0(...), .SD), .SDcols=clusterCols]
  df[, Cluster := as.factor(Cluster) %>% as.integer()]
  
  df  
}

kmeanClustering <- function(df){
  df
}


applyClustering <- function(df, method){
  switch(method,
         quantile = quantileClustering(df),
         kmean    = kmeanClustering(df))
}


DT <- transformDF(df, transform=TRANSFORM, scale=SCALE, center=CENTER)
applyClustering(DT, METHOD %>% tolower)


DT %>%
 ggplot(aes(x=MargeP, y=Sales, color=factor(Cluster))) +
 geom_point() +
 scale_y_log10()


DT <- DT[,getIdOrCluster(df), with=FALSE]
