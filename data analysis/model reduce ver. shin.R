setwd("c:/Users/정은/Desktop/new_bigdata_set/Final Total Data")
DF <- read.csv("Sleeping princess in penguin room.csv")
life <- DF[[3]]
gdp <- DF[[4]]; sani <- DF[[5]]; pre <- DF[[6]]; pri <- DF[[7]]
sec <- DF[[8]]; ter <- DF[[9]]; smo <- DF[[11]]; ob <- DF[[12]]
al <- DF[[13]]; co2 <- DF[[14]]; hiv <- DF[[15]]
n <- nrow(DF); p <- ncol(DF) - 4
reg <- lm(life~gdp+sani+pre+pri+sec+ter+smo+ob+al+co2+hiv)
######################################################################
infl.id <- c(44, 106, 145)
DF.1 <- DF[-as.numeric(infl.id),]
life.1 <- DF.1[[3]]
gdp.1 <- DF.1[[4]]; sani.1 <- DF.1[[5]]; pre.1 <- DF.1[[6]]; pri.1 <- DF.1[[7]]
sec.1 <- DF.1[[8]]; ter.1 <- DF.1[[9]]; smo.1 <- DF.1[[11]]; ob.1 <- DF.1[[12]]
al.1 <- DF.1[[13]]; co2.1 <- DF.1[[14]]; hiv.1 <- DF.1[[15]]

n.1 <- nrow(DF.1); p <- ncol(DF.1) - 4
reg.f <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+co2.1+hiv.1)
#######################################################################
###########################로그변환########################
log.gdp <- log(gdp.1, 10); log.co2 <- log(co2.1, 2); log.hiv <- log(hiv.1, 2)
reg.log <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+log.co2+log.hiv)
obg.log <- summary(reg.log)
round(obg.log$coefficients, 4)
####1. backward deletion ver. my 고집#####################
obj <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+co2.1+hiv.1)
all((summary(obj)$coefficients)[-1,4] <= 0.15)
which.max((summary(obj)$coefficients)[-1,4])
obj <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+hiv.1)
all((summary(obj)$coefficients)[-1,4] <= 0.15)
which.max((summary(obj)$coefficients)[-1,4])
obj <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+hiv.1)
all((summary(obj)$coefficients)[-1,4] <= 0.15)
which.max((summary(obj)$coefficients)[-1,4])
obj <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+ter.1+smo.1+ob.1+hiv.1)
all((summary(obj)$coefficients)[-1,4] <= 0.15)

####2. log transforamtion backward deleteion #############
obj <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+log.co2+log.hiv)
all((summary(obj)$coefficients)[-1,4] <= 0.15)
which.max((summary(obj)$coefficients)[-1,4])
obj <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+ter.1+smo.1+ob.1+al.1+log.co2+log.hiv)
all((summary(obj)$coefficients)[-1,4] <= 0.15)
which.max((summary(obj)$coefficients)[-1,4])
obj <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+ter.1+smo.1+al.1+log.co2+log.hiv)
all((summary(obj)$coefficients)[-1,4] <= 0.15)
which.max((summary(obj)$coefficients)[-1,4])
obj <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+ter.1+smo.1+log.co2+log.hiv)
all((summary(obj)$coefficients)[-1,4] <= 0.15)
##### 1. forward addition #######
library(leaps)
best.obj <- regsubsets(life.1 ~ ., DF.1[,-c(1, 2, 3, 10)])
obj <- summary(best.obj)
obj$which
obj$rsq
which.max(obj$adjr2)
which.min(obj$cp)
which.min(obj$bic)
bic.1 <- lm(life.1~gdp.1+sani.1+pre.1+hiv.1)
summary(bic.1)
###### 2. log transformation forward addition ######
df2 <- as.data.frame(cbind(log.gdp, sani.1, pre.1, pri.1, sec.1, ter.1, smo.1, ob.1, al.1 ,log.co2 ,log.hiv))
best.obj <- regsubsets(life.1 ~ ., df2)
obj <- summary(best.obj)
obj$which
obj$rsq
which.max(obj$adjr2)
which.min(obj$cp)
which.min(obj$bic)
bic.log <- lm(life.1~log.gdp+sani.1+pre.1+log.hiv)
summary(bic.log)
