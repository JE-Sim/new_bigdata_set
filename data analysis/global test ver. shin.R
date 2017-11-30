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
##non transformation#####################################################
bic.1 <- lm(life.1~gdp.1+sani.1+pre.1+hiv.1)
reg.1 <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+co2.1+hiv.1)
anova(reg.1); anova(bic.1)
SSE.f <- anova(reg.1)[12,2]; df.Ef <- 135
SSE.r <- anova(bic.1)[5,2]; df.Er <- 139
F.stat <- ((SSE.r-SSE.f)/(df.Er - df.Ef))/(SSE.f/df.Ef)  ##p.value (0.002)
anova(bic.1, reg.1)
###log transformation######################################################
bic.2 <- lm(life.1~log.gdp+sani.1+pre.1+log.hiv)
reg.0 <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+log.co2+log.hiv)
anova(bic.2, reg.0)
SSE.f <- anova(reg.0)[12,2]; df.Ef <- 135
SSE.r <- anova(bic.2)[5,2]; df.Er <- 139
F.stat <- ((SSE.r-SSE.f)/(df.Er - df.Ef))/(SSE.f/df.Ef)  ##p.value (0.003)
##########################################################################
#### cp ...... c pal
reg.1
a <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+ter.1+smo.1+ob.1+hiv.1)
anova(a, reg.1)
#### cp ...... log c pal
reg.0
b <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+ter.1+smo.1+log.co2+log.hiv)
anova(b, reg.0)


