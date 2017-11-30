setwd("c:/Users/정은/Desktop/new_bigdata_set/Final Total Data")
DF <- read.csv("Sleeping princess in penguin room.csv")
life <- DF[[3]]
gdp <- DF[[4]]; sani <- DF[[5]]; pre <- DF[[6]]; pri <- DF[[7]]
sec <- DF[[8]]; ter <- DF[[9]]; smo <- DF[[11]]; ob <- DF[[12]]
al <- DF[[13]]; co2 <- DF[[14]]; hiv <- DF[[15]]

n <- nrow(DF); p <- ncol(DF) - 4

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

########## Assumption Test #########
#independence
library(lmtest)
dwtest(life ~ gdp+sani+pre+pri+sec+ter+smo+ob+al+co2+hiv) # H0이 독립.
  #결과 independence함. p.value 겁나 큼
#normality test
# residual
r <- reg$residuals
# MSE
mse <- t(r) %*% r / (n - p - 1) 
# standardized residual
e <- r/c(sqrt(mse))
par(mfrow = c(1,1))
qqnorm(e)
abline(0, 1, col=2)
   #결과 앗 노말 다이스키
#####################################
######outlier, influence points#########
# studentized residual
alpha <- 0.05
temp <- (n - p - 2)/ (n - p - 1 - r^2)
t <- r * sqrt(abs(temp)) * sign(temp)
t.r <- rstudent(reg)
out.id <- which(abs(t.r) > qt(1-alpha/2, n - p - 2)); out.id
#plot
y.hat <- reg$fitted.values
plot(t.r, type = "n", xlab = "index", ylab = "studentized residual", main="studentized residual")
text(t.r, cex = 0.7)
text(out.id, t.r[out.id], out.id, col=2, cex = 0.7)
abline(h = 0, col = 2, lty = 2)
points(out.id, t.r[out.id], col = 4, cex = 3)
  #outlier가 outlier가 아닐 지경으로 너무 많다.
plot(t)
abline(h = c(qt(1-alpha/2, n - p - 2), -qt(1-alpha/2, n - p - 2)), col = 2)
#

#1.5 IQR
summary(y.hat)
IQR <- quantile(r, prob = 0.75) - quantile(r, prob = 0.25)
lower.bound <- quantile(r, prob = 0.25) - 1.5*IQR
upper.bound <- quantile(r, prob = 0.75) + 1.5*IQR
#out.shit <- which(r >= quantile(r, prob = 0.975) | r <= quantile(r, prob = 0.025))
out.IQR <- which(r <= lower.bound | r >= upper.bound)
#plot IQR
y.hat <- reg$fitted.values
plot(r, type = "n", xlab = "index", ylab = "residual", main = "1.5 IQR outlier")
text(r, cex = 0.7)
text(out.IQR, r[out.IQR], out.IQR, col=2, cex = 0.7)
abline(h = 0, col = 2, lty = 2)
points(out.IQR, r[out.IQR], col = 4, cex = 3)
   ## 22, 58, 106
#box plot
boxplot(r, main = "residual box plot", horizontal = T)

#####influential point#####
#Cook's distance
X1 <- as.matrix(DF[,-c(1, 2, 3, 10)])
X1[,1] <- X1[,1]/(10^9)  #gdp 너무 커서 R이 못견뎌요. 근데 double로 형변환하기는 싫어요.
X <- cbind(rep(1, n), X1)
H <- X %*% solve(t(X) %*% X) %*% t(X)
h <- diag(H)
C <- ((r^2)/(p+1)) * (h/(1-h))
infl.id <- names(C)[C > 1]
plot(C, type = "n", main = "Cook's distance, influential point")
text(C, cex = 0.7)
abline(h = 1, col = 2, lty = 2)
points(infl.id, C[infl.id], col = 4, cex = 3)
   ## 44, 106, 145
#DFFITS
DFFITS <- t * sqrt(h / (1 - h))
plot(DFFITS, ylim = c(-0.5, 3.5), type = "n", main = "DFFITS, influential points")
text(abs(DFFITS), cex = 0.7)
abline(h = 2 * sqrt((p + 1)/(n - p - 1)), col = 2, lty = 2)
   #파탄~ 우리는 쿡이 좋아요~ / studentized t가 파탄나서 DFFITS도 파탄 잼
#############################################
################assumption test##################


##############influential point delete fitted model ################
infl.id <- c(44, 106, 145) # 44, 106, 145
DF.1 <- DF[-as.numeric(infl.id),]
life.1 <- DF.1[[3]]
gdp.1 <- DF.1[[4]]; sani.1 <- DF.1[[5]]; pre.1 <- DF.1[[6]]; pri.1 <- DF.1[[7]]
sec.1 <- DF.1[[8]]; ter.1 <- DF.1[[9]]; smo.1 <- DF.1[[11]]; ob.1 <- DF.1[[12]]
al.1 <- DF.1[[13]]; co2.1 <- DF.1[[14]]; hiv.1 <- DF.1[[15]]

n.1 <- nrow(DF.1); p <- ncol(DF.1) - 4

reg.1 <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+co2.1+hiv.1)
obj <- summary(reg.1)
names(obj)
round(obj$coefficients, 4)
###########################로그변환########################
log.gdp <- log(gdp.1, 10); log.co2 <- log(co2.1, 2); log.hiv <- log(hiv.1, 2)
reg.log <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+log.co2+log.hiv)
obj.log <- summary(reg.log)
round(obj.log$coefficients, 4)
par(mfrow=c(1,2))
plot(reg.log)
plot(reg)
plot(reg.1)