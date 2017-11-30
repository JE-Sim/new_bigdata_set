setwd("c:/Users/정은/Desktop/new_bigdata_set/Final Total Data")
df <- read.csv("Sleeping princess in penguin room.csv")
life <- df[[3]]
gdp <- df[[4]]; sani <- df[[5]]; pre <- df[[6]]; pri <- df[[7]]
sec <- df[[8]]; ter <- df[[9]]; smo <- df[[11]]; ob <- df[[12]]
al <- df[[13]]; co2 <- df[[14]]; hiv <- df[[15]]

n <- nrow(df); p <- ncol(df) - 4

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
out.id <- which(abs(t) > qt(1-alpha/2, n - p - 2)); out.id
#plot
y.hat <- reg$fitted.values
plot(e, type = "n", xlab = "index", ylab = "residual", main="studentized residual")
text(e, cex = 0.7)
text(out.id, e[out.id], out.id, col=2, cex = 0.7)
abline(h = 0, col = 2, lty = 2)
points(out.id, e[out.id], col = 4, cex = 2)
  #outlier가 ourlier가 아닐 지경으로 너무 많다.

#1.5 IQR
summary(y.hat)
IQR <- quantile(r, prob = 0.75) - quantile(r, prob = 0.25)
lower.bound <- quantile(r, prob = 0.25) - 1.5*IQR
upper.bound <- quantile(r, prob = 0.75) + 1.5*IQR
out.shit <- which(r >= quantile(r, prob = 0.975) | r <= quantile(r, prob = 0.025))
out.IQR <- which(r <= lower.bound | r >= upper.bound)
#plot IQR
y.hat <- reg$fitted.values
plot(r, type = "n", xlab = "index", ylab = "residual", main = "1.5 IQR outlier")
text(r, cex = 0.7)
text(out.IQR, r[out.IQR], out.IQR, col=2, cex = 0.7)
abline(h = 0, col = 2, lty = 2)
points(out.IQR, r[out.IQR], col = 4, cex = 2)
   ## 22, 58, 106
#box plot
boxplot(r, main = "residual box plot")

#####influential point#####
#Cook's distance
X1 <- as.matrix(df[,-c(1, 2, 3, 10)])
X1[,1] <- X1[,1]/(10^9)  #gdp 너무 커서 R이 못견뎌요. 근데 double로 형변환하기는 싫어요.
X <- cbind(rep(1, n), X1)
H <- X %*% solve(t(X) %*% X) %*% t(X)
h <- diag(H)
C <- ((r^2)/(p+1)) * (h/(1-h))
infl.id <- names(C)[C > 1]
plot(C, type = "n")
text(C, cex = 0.7)
abline(h = 1, col = 2, lty = 2)
points(infl.id, C[infl.id], col = 4, cex = 2.2)
   ## 44, 106, 145
#DFFITS
DFFITS <- t * sqrt(h / (1 - h))
plot(DFFITS, ylim = c(-0.5, 3.5), type = "n")
text(abs(DFFITS), cex = 0.7)
abline(h = 2 * sqrt((p + 1)/(n - p - 1)), col = 2, lty = 2)
   #파탄~ 우리는 쿡이 좋아요~ / studentized t가 파탄나서 DFFITS도 파탄 잼
#############################################
################assumption test##################


##############influential point delete fitted model ################
infl.id # 44, 106, 145
df.1 <- df[-as.numeric(infl.id),]
life.1 <- df.1[[3]]
gdp.1 <- df.1[[4]]; sani.1 <- df.1[[5]]; pre.1 <- df.1[[6]]; pri.1 <- df.1[[7]]
sec.1 <- df.1[[8]]; ter.1 <- df.1[[9]]; smo.1 <- df.1[[11]]; ob.1 <- df.1[[12]]
al.1 <- df.1[[13]]; co2.1 <- df.1[[14]]; hiv.1 <- df.1[[15]]

n.1 <- nrow(df.1); p <- ncol(df.1) - 4

reg.1 <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+co2.1+hiv.1)
obg <- summary(reg.1)
names(obg)
round(obg$coefficients, 4)
###########################로그변환########################
log.gdp <- log(gdp.1, 10); log.co2 <- log(co2.1, 2); log.hiv <- log(hiv.1, 2)
reg.0 <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+log.co2+log.hiv)
obg.0 <- summary(reg.0)

round(obg.0$coefficients, 4)
