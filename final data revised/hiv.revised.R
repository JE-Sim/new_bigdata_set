setwd("C:/Users/정은/Desktop/new_bigdata_set/above 2000")
hiv1 <- read.csv("hiv.rm.csv")
setwd("C:/Users/정은/Desktop/new_bigdata_set/final_hiv_revised")
hiv2 <- read.csv("hiv2.raw.data.csv")
###########################################
hiv.com <- intersect(as.character(hiv1[[2]]), as.character(hiv2[[2]]))
hiv2[!(hiv2[[2]] %in% hiv.com), ]
hiv.1 <- cbind(hiv2[!(hiv2[[2]] %in% hiv.com), ], x2015 = NA, x2016 = NA)
colnames(hiv.1) <- colnames(hiv1)
hiv <- rbind(hiv1, hiv.1)
h.code <- as.character(hiv$Country.Code)
order.code <- order(h.code)
hiv <- hiv[order.code,]

KNN <- function(data, year){
  if(length(which(is.na(data[,year]))) == 0) return(data[,c(1,2, year)])
  na.row <- which(is.na(data[, year])) #기준 년도에서 NA인 row추출
  
  for(i in 1:length(na.row)){
    col <- !is.na(data[na.row[i],]) #key observation에서 NA가 아닌 col 추출
    collected.col <- data[, col] #data에서 col column들을 추출
    key <- collected.col[na.row[i],] #key observation의 row 추출
    index <- complete.cases(collected.col) #NA가 하나도 없는 row 추출
    non.na <- collected.col[index,] #NA를 제거함.
    a <- apply(as.data.frame(non.na[,-c(1,2)]), 1, "-", key[,-c(1,2)]) #두 값의 차
    b <- unlist(a) ^ 2 #차의 제곱
    c <- as.data.frame(matrix(b, length(b)/(length(collected.col)-2), 
                              length(collected.col)-2, byrow = T)) #vector b를 data.frame으로 형변환
    colnames(c) <- colnames(non.na)[-c(1, 2)]
    p.length <- apply(c, 1, sum) #각 년도별로 구해진 길이들의 합
    o.p <- order(p.length) #길이들의 합을 최소 순서로 배열
    n.point <- o.p[which(!is.na(data[o.p, year]))][1:5] #year 칼럼에 NA가 아닌 것들 중에서 작은거 5개
    d <- data[n.point, year] #key와 가까운 5개의 point의 점수 추출
    data[na.row[i], year] <- mean(d, na.rm = T) # 추출된 근처 값들의 평균을 넣어줌.
  }
  return(data[,c(1, 2, year)])
}

hiv.0 <- KNN(hiv, 19)
setwd("C:/Users/정은/Desktop/new_bigdata_set/final data revised")
write.csv(hiv.0, "hiv.rv.csv", row.names = F)
