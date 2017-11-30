setwd("c:/Users/정은/Desktop/new_bigdata_set/Final Total Data")
DF <- read.csv("Sleeping princess in penguin room.csv")
infl.id <- c(44, 106, 145)
DF.1 <- DF[-as.numeric(infl.id),]
life.1 <- DF.1[[3]]
gdp.1 <- DF.1[[4]]; sani.1 <- DF.1[[5]]; pre.1 <- DF.1[[6]]; pri.1 <- DF.1[[7]]
sec.1 <- DF.1[[8]]; ter.1 <- DF.1[[9]]; smo.1 <- DF.1[[11]]; ob.1 <- DF.1[[12]]
al.1 <- DF.1[[13]]; co2.1 <- DF.1[[14]]; hiv.1 <- DF.1[[15]]

n.1 <- nrow(DF.1); p <- ncol(DF.1) - 4
library(corrplot); library(DAAG)
#### cp ...... c pal
reg.r <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+ter.1+smo.1+ob.1+hiv.1)
DF.r <- as.data.frame(cbind(life = life.1, gdp = gdp.1, sani = sani.1, pre = pre.1,
              pri = pri.1, ter = ter.1, smo = smo.1, ob = ob.1, hiv = hiv.1))
plot(DF.r[,-1])
cor1.matrix <- matrix(0,8,8)
colnames(cor1.matrix) <- rownames(cor1.matrix) <- colnames(DF.r)[-1]
for(i in 1:8){
  for(j in 1:8){
    a <- cor(DF.r[,i+1], DF.r[,j+1])
    cor1.matrix[i,j] <- a
  }
}
round(cor1.matrix, 4)
vif(reg.r)

#### cp ...... log c pal
log.gdp <- log(gdp.1, 10); log.co2 <- log(co2.1, 2); log.hiv <- log(hiv.1, 2)
reg.logr <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+ter.1+smo.1+log.co2+log.hiv)
DF.logr<- as.data.frame(cbind(life = life.1, log.gdp = log.gdp, sani = sani.1, pre = pre.1,
                              pri = pri.1, ter = ter.1, smo = smo.1, log.co2 = log.co2, log.hiv = log.hiv))
plot(DF.logr[,-1])

corrplot(DF.r, method="number")
cor.matrix <- matrix(0,8,8)
colnames(cor.matrix) <- rownames(cor.matrix) <- colnames(DF.logr)[-1]
cor(sani.1, log.co2)
for(i in 1:8){
  for(j in 1:8){
    a <- cor(DF.logr[,i+1], DF.logr[,j+1])
    cor.matrix[i,j] <- a
  }
}
round(cor.matrix, 4)
cor.matrix[which(cor.matrix > 0.5)]
##10 or 4를 넘으면 공선성 있다고 생각한다.
vif(reg.logr) 
#why log transformation's the vif is lower than the vif?
###########################################################
anova(reg.r, reg.logr)
AIC(reg.r, reg.logr)
BIC(reg.r, reg.logr)
summary(reg.r); summary(reg.logr)
plot(DF[,-c(1,2,10)])
