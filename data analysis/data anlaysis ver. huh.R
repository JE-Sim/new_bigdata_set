setwd("c:/Users/정은/Desktop/new_bigdata_set/Final Total Data")
DF <- read.csv("Sleeping princess in penguin room.csv")
life <- DF[[3]]
gdp <- DF[[4]]; sani <- DF[[5]]; pre <- DF[[6]]; pri <- DF[[7]]
sec <- DF[[8]]; ter <- DF[[9]]; smo <- DF[[11]]; ob <- DF[[12]]
al <- DF[[13]]; co2 <- DF[[14]]; hiv <- DF[[15]]

reg <- lm(life~gdp+sani+pre+pri+sec+ter+smo+ob+al+co2+hiv)


#residual plot / 완전히 homoscadesity인 것 같지는 않는다. - WLS를 써야할 것 같다.
par(mfrow = c(1,2))
plot(reg$residuals, type = "n", main = "residual plot", ylab = "residuals"); abline(h=0, lty="dotted")
text(reg$residuals, names(reg$residuals), cex = 0.7)
plot(reg$fitted.values, reg$residuals, type = "n", main = "residual plot", xlab = "y.hat", ylab = "residuals")
abline(h = 0, lty = "dotted")
text(reg$fitted.values, reg$residuals, names(reg$fitted.values), cex = 0.7)
DF$Country.Name[which(reg$fitted.values >= 75)]


#ANOVA
anova.reg <- anova(reg); anova.reg



#이거는 각 변수별 scatter plot
DF1 <- DF[,-c(1, 2, 10)]
list.obj <- list()
for (j in c(2:13)){
  obj <- lm(life ~ DF1[,j])
  i <- j-1
  list.obj[i] <- obj
}

par(mfrow = c(3, 4))
for (j in c(2:13)){
  plot(DF1[,j], life, xlab=colnames(DF1)[j], main=paste("x", j))
  abline(lm(life~DF1[,j]), col="red")
}

####this part I just try to omit the 'mean' point that I think it is mode.
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

# gdp 로그변환, sani, pre, smo, ob(two group), al, co2 로그 변환, hiv 로그 변환
log.gdp <- log(gdp, 10); log.co2 <- log(co2, 2); log.hiv <- log(hiv, 2)
DFlog <- as.data.frame(cbind(log.gdp = log.gdp, sani = sani, pre = pre, pri = pri, 
                             sec = sec, ter = ter, smo = smo, ob = ob, al = al, 
                             log.co2 = log.co2, log.hiv = log.hiv))
par(mfrow = c(3, 4))
for (j in c(1:11)){
  plot(DFlog[,j], life, xlab=colnames(DFlog)[j], main=paste("x", j))
  abline(lm(life~DFlog[,j]), col="red")
}
j <- 1L

answer <- NULL
for (j in c(1:7)){
  answer <- c(answer, cor(life, DFlog[,j]))
}
answer
plot(smo ~ ob)
plot(log(gdp, 10) ~ ob)
plot(al ~ ob)
