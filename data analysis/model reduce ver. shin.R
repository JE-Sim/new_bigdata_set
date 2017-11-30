setwd("c:/Users/정은/Desktop/new_bigdata_set/Final Total Data")
df <- read.csv("Sleeping princess in penguin room.csv")
life <- df[[3]]
gdp <- df[[4]]; sani <- df[[5]]; pre <- df[[6]]; pri <- df[[7]]
sec <- df[[8]]; ter <- df[[9]]; smo <- df[[11]]; ob <- df[[12]]
al <- df[[13]]; co2 <- df[[14]]; hiv <- df[[15]]
n <- nrow(df); p <- ncol(df) - 4
reg <- lm(life~gdp+sani+pre+pri+sec+ter+smo+ob+al+co2+hiv)
######################################################################
infl.id <- c(44, 106, 145)
df.1 <- df[-as.numeric(infl.id),]
life.1 <- df.1[[3]]
gdp.1 <- df.1[[4]]; sani.1 <- df.1[[5]]; pre.1 <- df.1[[6]]; pri.1 <- df.1[[7]]
sec.1 <- df.1[[8]]; ter.1 <- df.1[[9]]; smo.1 <- df.1[[11]]; ob.1 <- df.1[[12]]
al.1 <- df.1[[13]]; co2.1 <- df.1[[14]]; hiv.1 <- df.1[[15]]

n.1 <- nrow(df.1); p <- ncol(df.1) - 4
reg.1 <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+co2.1+hiv.1)
#######################################################################
###########################로그변환########################
log.gdp <- log(gdp.1, 10); log.co2 <- log(co2.1, 2); log.hiv <- log(hiv.1, 2)
reg.0 <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+log.co2+log.hiv)
obg.0 <- summary(reg.0)
round(obg.0$coefficients, 4)
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
best.obj <- regsubsets(life.1 ~ ., df.1[,-c(1, 2, 3, 10)])
obj <- summary(best.obj)
obj$which
obj$rsq
which.max(obj$adjr2)
which.min(obj$cp)
which.min(obj$bic)
bic.1 <- lm(life.1~gdp.1+sani.1+pre.1+hiv.1)
###### 2. log transformation forward addition ######
df2 <- as.data.frame(cbind(log.gdp, sani.1, pre.1, pri.1, sec.1, ter.1, smo.1, ob.1, al.1 ,log.co2 ,log.hiv))
best.obj <- regsubsets(life.1 ~ ., df2)
obj <- summary(best.obj)
obj$which
obj$rsq
which.max(obj$adjr2)
which.min(obj$cp)
which.min(obj$bic)
bic.2 <- lm(life.1~log.gdp+sani.1+pre.1+log.hiv)
