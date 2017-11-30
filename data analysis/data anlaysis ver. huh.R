setwd("c:/Users/정은/Desktop/new_bigdata_set/Final Total Data")
df <- read.csv("Sleeping princess in penguin room.csv")
life <- df[[3]]
gdp <- df[[4]]; sani <- df[[5]]; pre <- df[[6]]; pri <- df[[7]]
sec <- df[[8]]; ter <- df[[9]]; smo <- df[[11]]; ob <- df[[12]]
al <- df[[13]]; co2 <- df[[14]]; hiv <- df[[15]]

reg <- lm(life~gdp+sani+pre+pri+sec+ter+smo+ob+al+co2+hiv)


#residual plot / 완전히 homoscadesity인 것 같지는 않는다. - WLS를 써야할 것 같다.
par(mfrow = c(1,2))
plot(reg$residuals, type = "n", main = "residual plot", ylab = "residuals"); abline(h=0, lty="dotted")
text(reg$residuals, names(reg$residuals), cex = 0.7)
plot(reg$fitted.values, reg$residuals, type = "n", main = "residual plot", xlab = "y.hat", ylab = "residuals")
abline(h = 0, lty = "dotted")
text(reg$fitted.values, reg$residuals, names(reg$fitted.values), cex = 0.7)
df$Country.Name[which(reg$fitted.values >= 75)]


#ANOVA
anova.reg <- anova(reg); anova.reg



#이거는 각 변수별 scatter plot
df1 <- df[,-c(1, 2, 10)]
list.obj <- list()
for (j in c(2:13)){
  obj <- lm(life ~ df1[,j])
  i <- j-1
  list.obj[i] <- obj
}

par(mfrow = c(3, 4))
for (j in c(2:13)){
  plot(df1[,j], life, color=c("white","gray"), xlab=colnames(df1)[j], main=paste("x", j))
  abline(lm(life~df1[,j]), col="red")
}

####this part I just try to omit the 'mean' point that I think it is mode.
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

# gdp 로그변환, sani, pre, smo, ob(two group), al, co2 로그 변환, hiv 로그 변환

df2 <- cbind(log(gdp, 10), sani, pre, smo, al, log(co2, 2), log(hiv, 2))
colnames(df2) <- c("log.gdp", "sani", "pre", "smo", "al", "log.co2", "log.hiv")
par(mfrow = c(3, 3))
for (j in c(1:7)){
  plot(df2[,j], life, color=c("white","gray"), xlab=colnames(df2)[j], main=paste("x", j))
}

answer <- NULL
for (j in c(1:7)){
  answer <- c(answer, cor(life, df2[,j]))
}
answer
plot(smo ~ ob)
plot(log(gdp, 10) ~ ob)
plot(al ~ ob)
