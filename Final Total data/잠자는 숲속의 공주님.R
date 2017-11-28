setwd("c:/Users/정은/Desktop/new_bigdata_set/final data revised")
co2 <- read.csv("co2.rv.csv")
gdp <- read.csv("gdp.rv.csv")
hiv <- read.csv("hiv.rv.csv")
life <- read.csv("life.rv.csv")
ob <- read.csv("ob.rv.csv")
pre <- read.csv("pre.rv.csv")
pri <- read.csv("pri.rv.csv")
sani <- read.csv("sani.rv.csv")
sec <- read.csv("sec.rv.csv")
smo <- read.csv("smo.rv.csv")
ter <- read.csv("ter.rv.csv")
al <- read.csv("al.rv.csv")

co2 <- co2[order(co2$Country.Code),]
gdp <- gdp[order(gdp$Country.Code),]
hiv <- hiv[order(hiv$Country.Code),]
life <- life[order(life$Country.Code),]
ob <- ob[order(ob$Country.Code),]
pre <- pre[order(pre$Country.Code),]
pri <- pri[order(pri$Country.Code),]
sani <- sani[order(sani$Country.Code),]
sec <- sec[order(sec$Country.Code),]
smo <- smo[order(smo$Country.Code),]
ter <- ter[order(ter$Country.Code),]
al <- al[order(al$Country.Code),]
###########################################
common <- intersect(as.character(co2[[2]]), as.character(gdp[[2]]))
common <- intersect(common, as.character(hiv[[2]]))
common <- intersect(common, as.character(life[[2]]))
common <- intersect(common, as.character(ob[[2]]))
common <- intersect(common, as.character(pre[[2]]))
common <- intersect(common, as.character(pri[[2]]))
common <- intersect(common, as.character(sec[[2]]))
common <- intersect(common, as.character(sani[[2]]))
common <- intersect(common, as.character(smo[[2]]))
common <- intersect(common, as.character(ter[[2]]))
common <- intersect(common, as.character(al[[2]]))

x.9 <- al[al[[2]] %in% common, 3]
x.10 <- co2[co2[[2]] %in% common, 3]
x.1 <- gdp[gdp[[2]] %in% common, 3]
x.11 <- hiv[hiv[[2]] %in% common, 3]
y <- life[life[[2]] %in% common, ]
x.8 <- ob[ob[[2]] %in% common, 3]
x.3 <- pre[pre[[2]] %in% common, 3]
x.4 <- pri[pri[[2]] %in% common, 3]
x.2 <- sani[sani[[2]] %in% common, 3]
x.5 <- sec[sec[[2]] %in% common, 3]
x.7 <- smo[smo[[2]] %in% common, c(3, 4)]
x.6 <- ter[ter[[2]] %in% common, 3]

df <- cbind(y, gdp = x.1, sani = x.2, pre = x.3, pri = x.4, sec = x.5,
            ter = x.6, smo = x.7, ob = x.8, al = x.9, co2 = x.10, hiv = x.11)

setwd("c:/Users/정은/Desktop/new_bigdata_set/Final Total Data")
write.csv(df, "Sleeping princess in penguin room.csv", row.names = F)

setwd("c:/Users/정은/Desktop/new_bigdata_set/Final Total Data")
df <- read.csv("Sleeping princess in penguin room.csv")
y <- df[[3]]
gdp <- df[[4]]; sani <- df[[5]]; pre <- df[[6]]; pri <- df[[7]]
sec <- df[[8]]; ter <- df[[9]]; smo <- df[[11]]; ob <- df[[12]]
al <- df[[13]]; co2 <- df[[14]]; hiv <- df[[15]]

reg <- lm(y~gdp+sani+pre+pri+sec+ter+smo+ob+al+co2+hiv)
plot(reg$residuals, type = "n", main = "residual plot", ylab = "residuals"); abline(h=0, lty="dotted")
text(reg$residuals, names(reg$residuals), cex = 0.7)
plot(reg$fitted.values, reg$residuals, type = "n", main = "residual plot", xlab = "y.hat", ylab = "residuals")
abline(h = 0, lty = "dotted")
text(reg$fitted.values, reg$residuals, names(reg$fitted.values), cex = 0.7)