
rm(list=ls())

library(data.table)
library(magrittr)
library(ggplot2)

transform <- T
center <- F
scale <- F
numberOfClusters <- 3

df <- readRDS("G:/Privat2Work/data.R") %>% as.data.table()
df <- df[,.(idCustomerOrigin, Sales, MargeP)]

df[,1] <- lapply(df[,1, with=FALSE], as.character)
df[,2:ncol(df)] <- lapply(df[,2:ncol(df),with=FALSE], as.numeric)

numericCols <- which(sapply(df, is.numeric))

if (transform){
  df[, `:=` (MargeP= MargeP*1, Sales = Sales*1)]
}

if (any(center, scale)){
  df[, (numericCols) := lapply(.SD, function(x)
    scale(x, center=center, scale=scale)), .SDcols = numericCols]
}


createCuts <- function(x){
  quantiles <- quantile(x, seq(0,1,1/numberOfClusters))
  cuts <- cut(x, quantiles)
  as.integer(cuts)
}


clusterCols <- paste(names(numericCols), "Cluster")
df[ ,c(clusterCols) := lapply(.SD, createCuts), .SDcols = numericCols]


combineClusters <- function(x){
  clusters <- paste0(x)
  clusters <- factor(clusters)
  as.integer(clusters)
}

clusterCols <- which(endsWith(colnames(df), "Cluster"))
df[, C := Reduce(function(...) paste0(...), .SD), .SDcols=clusterCols]
df[, C := as.factor(C) %>% as.integer()]



df %>% 
  ggplot(aes(x=MargeP, y=Sales, color=factor(C))) +
  geom_point() +
  scale_y_log10()
