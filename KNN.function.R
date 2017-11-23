setwd("c:/Users/Á¤Àº/Desktop/new_bigdata_set/above 2000")
life <- read.csv("life.rm.csv")

#data should be standard form of data.frame, year should be key column #.
KNN <- function(data, year){
  na.row <- which(is.na(data[, year]))
  for(i in 1:length(na.row)){
    col <- !is.na(data[na.row[i],])
    collected.col <- data[, col]
    key <- collected.col[na.row[i],]
    a <- apply(as.data.frame(collected.col[,-c(1,2)]), 1, "-", key[,-c(1,2)])
    b <- unlist(a) ^ 2
    c <- as.data.frame(matrix(b, length(b)/(length(collected.col)-2), 
                              length(collected.col)-2, byrow = T))
    colnames(c) <- colnames(collected.col)[-c(1, 2)]
    p.length <- apply(c, 1, mean, na.rm = T)
    n.point <- order(p.length)[1:6]
    d <- data[n.point, year]
    data[na.row[i],year] <- mean(d, na.rm = T)
  }
  return(data[,c(1, 2, year)])
}
shit <- KNN(life, 18)

#################################################
na.row <- which(is.na(life[,"X2015"]))
for(i in 1:length(na.row)){
  col <- !is.na(life[na.row[i],])
  collected.col <- life[, col]
  key <- collected.col[na.row[i],]
  a <- apply(as.data.frame(collected.col[,-c(1,2)]), 1, "-", key[,-c(1,2)])
  b <- unlist(a) ^ 2
  c <- as.data.frame(matrix(b, length(b)/(length(collected.col)-2), 
                            length(collected.col)-2, byrow = T))
  colnames(c) <- colnames(collected.col)[-c(1, 2)]
  p.length <- apply(c, 1, mean, na.rm = T)
  n.point <- order(p.length)[1:6]
  d <- life[n.point, "X2015"]
  life[na.row[i],"X2015"] <- mean(d, na.rm = T)
}
life[,"X2015"]
###################################################