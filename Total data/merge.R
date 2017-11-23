setwd("c:/Users/Á¤Àº/Desktop/new_bigdata_set/data revised")
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
p.name <- as.character(pri$Country.Code)
s.name <- as.character(sec$Country.Code)
common <- intersect(p.name, s.name)
pri.0 <- pri[pri$Country.Code %in% common,]
sec.0 <- sec[sec$Country.Code %in% common,]
merge(pri.0, sec.0)
row.names(pri.0)
c <- cbind(pri.0, sec.0$X2014)

a <- merge(life, gdp, key = Country.Code)
b <- merge(sani, pre, key = Country.Code)
d <- merge(ter, smo, key = Country.Code)
e <- merge(ob, al, key = Country.Code)
f <- merge(co2, hiv, key = Country.Code)

common1 <- intersect(as.character(a$Country.Code), as.character(b$Country.Code))
common1 <- intersect(common1, as.character(c$Country.Code))
common1 <- intersect(common1, as.character(d$Country.Code))
common1 <- intersect(common1, as.character(e$Country.Code))

a.0 <- a[a$Country.Code %in% common1,]
b.0 <- b[b$Country.Code %in% common1,]
c.0 <- c[c$Country.Code %in% common1,]
d.0 <- d[d$Country.Code %in% common1,]
e.0 <- e[e$Country.Code %in% common1,]

df <- cbind(a.0, b.0[,3:4], c.0[,3:4], d.0[,3:5], e.0[,3:4])
common1 <- intersect(as.character(df$Country.Code), as.character(f$Country.Code))
f.0 <- f[f$Country.Code %in% common1,]
df1 <- cbind(df[df$Country.Code %in% common1,], f.0[,3:4])
colnames(df1) <- c("Country.Name", "Country.Code","life", "gdp", "sani", "pre", "pri", "sec", 
                           "ter", "smo.tob", "smo.cig", "ob", "al", "co2", "hiv")
write.csv(df1, "dataframe.non.race.csv")
