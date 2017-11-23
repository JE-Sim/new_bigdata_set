setwd("C:/Users/dlgpw/Desktop")
Smoking <- read.csv("smo.rm.csv", header = T)
country.code <- read.csv("country.code.csv", header = T)

Smoking$Daily.tobacco[Smoking$Daily.tobacco=="Not available"] <- NA
Smoking$Daily.cigarettes[Smoking$Daily.cigarettes=="Not available"] <- NA

Smoking <- Smoking[order(Smoking$Country.Name),]

c.name <- as.character(country.code$Country.Name)
o.name <- as.character(Smoking$Country.Name)
common <- intersect(c.name, o.name)
country.code <- country.code[order(country.code$Country.Name),]

o.name1 <- country.code[country.code$Country.Name %in% common,]
Smoking0 <- Smoking[Smoking $ Country.Name %in% common,]
Smoking1 <- cbind(o.name1, Smoking0[,-1])


write.csv(Smoking1, "smo.rm.2.CSV", row.names = F)
