setwd("c:/Users/정은/Desktop/new_bigdata_set/Final Total Data")
DF <- read.csv("Sleeping princess in penguin room.csv")
life <- DF[[3]]
gdp <- DF[[4]]; sani <- DF[[5]]; pre <- DF[[6]]; pri <- DF[[7]]
sec <- DF[[8]]; ter <- DF[[9]]; smo <- DF[[11]]; ob <- DF[[12]]
al <- DF[[13]]; co2 <- DF[[14]]; hiv <- DF[[15]]
n <- nrow(DF); p <- ncol(DF) - 4
reg.full <- lm(life~gdp+sani+pre+pri+sec+ter+smo+ob+al+co2+hiv)
######################################################################
infl.id <- c(44, 106, 145)
DF.1 <- DF[-as.numeric(infl.id),]
life.1 <- DF.1[[3]]
gdp.1 <- DF.1[[4]]; sani.1 <- DF.1[[5]]; pre.1 <- DF.1[[6]]; pri.1 <- DF.1[[7]]
sec.1 <- DF.1[[8]]; ter.1 <- DF.1[[9]]; smo.1 <- DF.1[[11]]; ob.1 <- DF.1[[12]]
al.1 <- DF.1[[13]]; co2.1 <- DF.1[[14]]; hiv.1 <- DF.1[[15]]

n.1 <- nrow(DF.1); p <- ncol(DF.1) - 4
reg.1 <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+co2.1+hiv.1)
obg.reg <- summary(reg.1)
obg.reg
#######################################################################
###########################로그변환########################
log.gdp <- log(gdp.1, 10); log.co2 <- log(co2.1, 2); log.hiv <- log(hiv.1, 2)
reg.log <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+log.co2+log.hiv)
obg.log <- summary(reg.log)
round(obg.log$coefficients, 4)
##non transformation#####################################################
bic.1 <- lm(life.1~gdp.1+sani.1+pre.1+hiv.1)
reg.1 <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+co2.1+hiv.1)
anova(reg.1); anova(bic.1)
SSE.f <- anova(reg.1)[12,2]; DF.Ef <- 135
SSE.r <- anova(bic.1)[5,2]; DF.Er <- 139
F.stat <- ((SSE.r-SSE.f)/(DF.Er - DF.Ef))/(SSE.f/DF.Ef)  ##p.value (0.002)
anova(bic.1, reg.1) #H0 : reduced / 기각이 되어서 reduced one을 쓸수가 없다.
###log transformation######################################################
bic.log <- lm(life.1~log.gdp+sani.1+pre.1+log.hiv)
reg.log <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+log.co2+log.hiv)
anova(bic.log, reg.log)
SSE.f <- anova(reg.log)[12,2]; DF.Ef <- 135
SSE.r <- anova(bic.log)[5,2]; DF.Er <- 139
F.stat <- ((SSE.r-SSE.f)/(DF.Er - DF.Ef))/(SSE.f/DF.Ef)  ##p.value (0.003)
##########################################################################
#### cp ...... c pal
reg.1
reg.cp <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+ter.1+smo.1+ob.1+hiv.1)
anova(reg.cp, reg.1)
#### cp ...... log c pal
reg.log
reg.logcp<- lm(life.1~log.gdp+sani.1+pre.1+pri.1+ter.1+smo.1+log.co2+log.hiv)
anova(reg.logcp, reg.log)

