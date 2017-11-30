setwd("c:/Users/정은/Desktop/new_bigdata_set/Final Total Data")
DF <- read.csv("Sleeping princess in penguin room.csv")
######################################################################
infl.id <- c(44, 106, 145)
DF.1 <- DF[-as.numeric(infl.id),]
life.1 <- DF.1[[3]]
gdp.1 <- DF.1[[4]]; sani.1 <- DF.1[[5]]; pre.1 <- DF.1[[6]]; pri.1 <- DF.1[[7]]
sec.1 <- DF.1[[8]]; ter.1 <- DF.1[[9]]; smo.1 <- DF.1[[11]]; ob.1 <- DF.1[[12]]
al.1 <- DF.1[[13]]; co2.1 <- DF.1[[14]]; hiv.1 <- DF.1[[15]]

n.1 <- nrow(DF.1); p <- ncol(DF.1) - 4
reg.1 <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+co2.1+hiv.1)
#######################################################################
###########################로그변환########################
log.gdp <- log(gdp.1, 10); log.co2 <- log(co2.1, 2); log.hiv <- log(hiv.1, 2)
reg.log <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+sec.1+ter.1+smo.1+ob.1+al.1+log.co2+log.hiv)
obg.log <- summary(reg.log)
round(obg.log$coefficients, 4)
###########################################################
cp.X <- as.data.frame(cbind(gdp = gdp.1, sani = sani.1, pre = pre.1, pri = pri.1, 
              ter = ter.1, smo = smo.1, ob = ob.1, hiv = hiv.1))
reg.1
reg.cp <- lm(life.1~gdp.1+sani.1+pre.1+pri.1+ter.1+smo.1+ob.1+hiv.1)
anova(reg.cp, reg.1)
#### cp ...... log c pal
cp.logX <- as.data.frame(cbind(log.gdp = log.gdp, sani = sani.1, pre = pre.1, pri = pri.1,
                               ter = ter.1, smo = smo.1, log.co2 = log.co2, log.hiv = log.hiv))
reg.cplog <- lm(life.1~log.gdp+sani.1+pre.1+pri.1+ter.1+smo.1+log.co2+log.hiv)
anova(reg.cplog, reg.0)


#########cp.X scatterplot ###########
list.obj <- list()
for (j in c(1:8)){
  obj <- lm(life.1 ~ cp.X[,j])
  list.obj[j] <- obj
}
par(mfrow = c(3, 3))
for (j in c(1:8)){
  plot(cp.X[,j], life.1, color=c("white","gray"), xlab=colnames(cp.X)[j], main=paste("x", j))
  abline(lm(life.1~cp.X[,j]), col="red")
}

####this part I just try to omit the 'mean' point that I think it is mode.
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}


######## log transformation scatterplot
list.objlog <- list()
for (j in c(1:8)){
  objlog <- lm(life.1 ~ cp.logX[,j])
  list.objlog[j] <- objlog
}
par(mfrow = c(3, 3))
for (j in c(1:8)){
  plot(cp.logX[,j], life.1, color=c("white","gray"), xlab=colnames(cp.logX)[j], main=paste("x", j))
  abline(lm(life.1~cp.logX[,j]), col="red")
}
########
