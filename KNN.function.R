setwd("c:/Users/정은/Desktop/new_bigdata_set/above 2000")
#data should be standard form of data.frame, year should be key column #.
KNN <- function(data, year){
  na.row <- which(is.na(data[, year])) #기준 년도에서 NA인 row추출
  for(i in 1:length(na.row)){
    col <- !is.na(data[na.row[i],]) #key observation에서 NA가 아닌 col 추출
    collected.col <- data[, col] #data에서 col column들을 추출
    key <- collected.col[na.row[i],] #key observation의 row 추출
    a <- apply(as.data.frame(collected.col[,-c(1,2)]), 1, "-", key[,-c(1,2)]) #두 값의 차
    b <- unlist(a) ^ 2 #차의 제곱
    c <- as.data.frame(matrix(b, length(b)/(length(collected.col)-2), 
                              length(collected.col)-2, byrow = T)) #vector b를 data.frame으로 형변환
    colnames(c) <- colnames(collected.col)[-c(1, 2)]
    p.length <- apply(c, 1, mean, na.rm = T) #각 년도별로 구해진 길이들의 평균.
    n.point <- order(p.length)[2:6] #첫번째 요소는 자기 자신이라서 제외해줬다. key와 가장 가까운 5개의 point 추출
    d <- data[n.point, year] #key와 가까운 5개의 point의 점수 추출
    data[na.row[i], year] <- mean(d, na.rm = T) # 추출된 근처 값들의 평균을 넣어줌.
  }
  return(data[,c(1, 2, year)])
}

#################################################
na.row <- which(is.na(life[,"X2015"]))
i = 1L
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
  n.point <- order(p.length)[2:6]
  d <- life[n.point, "X2015"]
  life[na.row[i],"X2015"] <- mean(d, na.rm = T)
}
life[,"X2015"]
###################################################

life <- read.csv("life.rm.csv")
life.0 <- KNN(life, 18)
gdp <- read.csv("gdp.rm.csv")
gdp.0 <- KNN(gdp, 19)
co2 <- read.csv("co2.rm.csv")
co2.0 <- KNN(co2, 17)

##################################################
ter <- read.csv("ter.rm.csv")
ter.0 <- KNN(ter, 18)
i <- 1L
na.row <- NULL
na.row <- which(ter.0[,3] == "NaN")
for(i in 1:length(na.row)){
  col <- !is.na(ter[na.row[i],])
  collected.col <- ter[, col]
  key <- collected.col[na.row[i],]
  a <- apply(as.data.frame(collected.col[,-c(1,2)]), 1, "-", key[,-c(1,2)])
  b <- unlist(a) ^ 2
  c <- as.data.frame(matrix(b, length(b)/(length(collected.col)-2), 
                            length(collected.col)-2, byrow = T))
  colnames(c) <- colnames(collected.col)[-c(1, 2)]
  p.length <- apply(c, 1, mean, na.rm = T)
  n.point <- order(p.length)[2:6]
  d <- ter[n.point, ]
  ter.0[na.row[i], 18] <- mean(d, na.rm = T)
}
order(p.length)[7]
ter[190,]
write.csv(d, "ter.error.csv")
##KNN을 적용시 1~5번째로 가까운 점들 중에서 2015년이 모두 NA가 떴다.

smo <- read.csv("smo.rm.2.CSV")
which(is.na(smo[,3]))
which(is.na(smo[,4]))
avg <- apply(smo[,c(3,4)], 2, mean, na.rm = T)
smo[which(is.na(smo[,3])),3] <- avg[1]
smo[which(is.na(smo[,4])),4] <- avg[2]
