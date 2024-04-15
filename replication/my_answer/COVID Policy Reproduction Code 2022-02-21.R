## Daniel J. Mallinson and Saahir Shafi
## COVID Policy Response Analysis
## 2020-08-06
## 2020-09-14
## 2020-12-17
## 2021-09-22
#####
## Criomhhtann Morrison
## Replication Project
## 2024-04-08
## Find additional code from line 1194

rm(list=ls()) #clear workspace

install.packages(c("readxl", "quantmod", "tseries", "plm", "punitroots", "tidyr", "kader"))

library(foreign)
library(readxl)
library(quantmod)
library(tseries)
library(plm) #https://cran.r-project.org/web/packages/plm/vignettes/plmPackage.html
library(reshape2)
library(tidyr)
library(kader)
library(stargazer)
library(ggplot2)

#https://towardsdatascience.com/var-and-panel-data-models-the-powerhouse-of-multivariate-forecasting-techniques-22b8d8888141
#https://www.aptech.com/blog/introduction-to-the-fundamentals-of-panel-data/
#https://cran.r-project.org/web/packages/plm/vignettes/A_plmPackage.html

data <- read.csv("CovidData_9.16.21.csv", fileEncoding="UTF-8-BOM")
data.week <- read.csv("CovidData_2.17.22.csv", fileEncoding="UTF-8-BOM")

data <- data[which(data$Date>20200121),] # Remove observations before Jan 22, 2020

countries <- as.character(unique(data$CountryName))

#add day count to data for plotting
days <- seq(1,345,1)

for(i in 1:length(unique(data$CountryName))){
  temp <- data[which(data$CountryName == countries[i]),]
  temp <- cbind(temp, days)
  names(temp)[length(temp)] <- "day"
  if(i == 1){
    newdata <- temp
  }else{
    newdata <- rbind(newdata, temp)
  }
}

data <- newdata

#Determine countries with missing data
missing <- data[rowSums(is.na(data)) > 0,] #Check for remaining NAs
100-sum(unique(missing$totalpop), na.rm=TRUE)/7800000000*100 #World's population represented by included countries

unique(missing$CountryName)

data <- drop_na(data)
data.week <- drop_na(data.week)

countries <- as.character(unique(data$CountryName))


############################## Figures 1 and 2 ###########################

## Plot Stringency Over Time
pdf("figure1.pdf", height=8.5, width=11)
#tiff("figure1.tiff", height=8.5, width=11, units="in", res=600)
plot.new()
par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.window(xlim=c(0,345), ylim=c(0,100))
for(i in 1:length(countries)){
  country <- countries[i]
  lines(data$day[data$CountryName==country],data$StringencyIndex[data$CountryName==country], col="gray50")
}
lines(data$day[data$CountryName=="United States"],data$StringencyIndex[data$CountryName=="United States"], col="dodgerblue4", lwd=3)
text(150, 20, labels="United States", font=2, col="dodgerblue4")
lines(data$day[data$CountryName=="France"],data$StringencyIndex[data$CountryName=="France"], col="darkred", lwd=3)
text(150, 95, labels="France", font=2, col="darkred")
axis(1, at=c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315), labels=c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1"))
axis(2, at=seq(0,100,10), labels=seq(0,100,10), las=2)
title(ylab="Stringency Index")
#dev.off()

#pdf("govtresponse_allcountries.pdf", height=4, width=8)
#plot.new()
par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.window(xlim=c(0,345), ylim=c(0,100))
for(i in 1:length(countries)){
  country <- countries[i]
  lines(data$day[data$CountryName==country],data$GovernmentResponseIndex[data$CountryName==country], col="gray50")
}
lines(data$day[data$CountryName=="United States"],data$GovernmentResponseIndex[data$CountryName=="United States"], col="dodgerblue4", lwd=3)
text(150, 20, labels="United States", font=2, col="dodgerblue4")
lines(data$day[data$CountryName=="France"],data$StringencyIndex[data$CountryName=="France"], col="darkred", lwd=3)
text(150, 95, labels="France", font=2, col="darkred")
axis(1, at=c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315), labels=c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1"))
axis(2, at=seq(0,100,10), labels=seq(0,100,10), las=2)
title(ylab="Government Response Index")
#dev.off()

#pdf("containment_allcountries.pdf", height=4, width=8)
#plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(0,345), ylim=c(0,100))
for(i in 1:length(countries)){
  country <- countries[i]
  lines(data$day[data$CountryName==country],data$ContainmentHealthIndex[data$CountryName==country], col="gray50")
}
lines(data$day[data$CountryName=="United States"],data$ContainmentHealthIndex[data$CountryName=="United States"], col="dodgerblue4", lwd=3)
text(150, 20, labels="United States", font=2, col="dodgerblue4")
lines(data$day[data$CountryName=="France"],data$StringencyIndex[data$CountryName=="France"], col="darkred", lwd=3)
text(150, 95, labels="France", font=2, col="darkred")
axis(1, at=c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315), labels=c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1"))
axis(2, at=seq(0,100,10), labels=seq(0,100,10), las=2)
title(ylab="Containment Health Index")
#dev.off()

#pdf("econsupport_allcountries.pdf", height=4, width=8)
#plot.new()
par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.window(xlim=c(0,345), ylim=c(0,100))
for(i in 1:length(countries)){
  country <- countries[i]
  lines(data$day[data$CountryName==country],data$EconomicSupportIndex[data$CountryName==country], col="gray50")
}
lines(data$day[data$CountryName=="United States"],data$EconomicSupportIndex[data$CountryName=="United States"], col="dodgerblue4", lwd=3)
text(150, 20, labels="United States", font=2, col="dodgerblue4")
lines(data$day[data$CountryName=="France"],data$StringencyIndex[data$CountryName=="France"], col="darkred", lwd=3)
text(150, 95, labels="France", font=2, col="darkred")
axis(1, at=c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315), labels=c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1"))
axis(2, at=seq(0,100,10), labels=seq(0,100,10), las=2)
title(ylab="Economic Support Index")
dev.off()

pdf("figure2.pdf", height=8, width=8)
#tiff("figure2.tiff", height=8, width=8, units="in", res=600)
plot.new()
par(fig=c(0,1,0,0.5), new=TRUE)
plot.window(xlim=c(0,345), ylim=c(0,18000))
for(i in 1:length(countries)){
  country <- countries[i]
  lines(data$day[data$CountryName==country],data$fiscalmeasures[data$CountryName==country], col="gray50")
}
lines(data$day[data$CountryName=="United States"],data$fiscalmeasures[data$CountryName=="United States"], col="dodgerblue4", lwd=3)
text(150, 10000, labels="United States", font=2, col="dodgerblue4")
lines(data$day[data$CountryName=="France"],data$StringencyIndex[data$CountryName=="France"], col="darkred", lwd=3)
text(150, 2000, labels="France", font=2, col="darkred")
axis(1, at=c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315), labels=c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1"))
axis(2, at=seq(0,18000,2000), labels=c("0", "2K", "4K", "6K", "8K", "10K", "12K", "14K", "16K", "18K"), las=2)
mtext(2, text="Fiscal Measures ($ Per Capita)", line=3)
#dev.off()

#pdf("healthinvestment_allcountries.pdf", height=4, width=8)
#plot.new()
par(fig=c(0,1,0.5,1), new=TRUE)
plot.window(xlim=c(0,345), ylim=c(0,1300))
for(i in 1:length(countries)){
  country <- countries[i]
  lines(data$day[data$CountryName==country],data$healthinvestment[data$CountryName==country], col="gray50")
}
lines(data$day[data$CountryName=="United States"],data$healthinvestment[data$CountryName=="United States"], col="dodgerblue4", lwd=3)
text(150, 600, labels="United States", font=2, col="dodgerblue4")
lines(data$day[data$CountryName=="France"],data$StringencyIndex[data$CountryName=="France"], col="darkred", lwd=3)
text(150, 200, labels="France", font=2, col="darkred")
axis(1, at=c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315), labels=c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1"))
axis(2, at=seq(0,1300,200), labels=seq(0,1300,200), las=2)
title(ylab="Health Investment ($ Per Capita)")
dev.off()

########################### Figure 3 #############################

## Response-Risk Ratio
data$totalcaserate <- data$ConfirmedCases/data$totalpop*1000

pdf("figure3.pdf", height=8.5, width=11)
#tiff("figure3.tiff", height=8.5, width=11, units="in", res=600)

cntry <- c("India", "El Salvador", "Trinidad and Tobago", "New Zealand", "United States",
          "Sweden", "Switzerland", "Belarus")

plot.new()
par(fig=c(0,0.5,0,0.5), new=TRUE)
for(i in 1:length(countries)){
  country <- countries[i]
  working <- cbind(data$StringencyIndex[data$CountryName==country], data$totalcaserate[data$CountryName==country])
  max.p <- max(working[,1])
  max.c <- max(working[,2])
  if(i==1){
    max.si <- as.data.frame(cbind(country, max.p, max.c))
  }else{
    max.si <- rbind(max.si, cbind(country, max.p, max.c))
  }
}

max.si[,2] <- as.numeric(paste(max.si[,2]))
max.si[,3] <- as.numeric(paste(max.si[,3]))

max.si <- max.si[order(max.si$max.c),]

lab <- subset(max.si, max.si$country %in% cntry)

plot(max.si$max.p~max.si$max.c, ylab="gency", xlab="Cases Per 1,000", pch=20, col="grey60")
points(lab$max.p~lab$max.c, pch=18, col="black", cex=1.5)
text(lab$max.c, lab$max.p, lab$country, pos=4, cex=.65)
loess.ms <- loess(max.p~max.c, data=max.si)
lines(max.si$max.c, predict(loess.ms), col="blue")

par(fig=c(0,0.5,0.5,1), new=TRUE)

for(i in 1:length(countries)){
  country <- countries[i]
  working <- cbind(data$GovernmentResponseIndex[data$CountryName==country], data$totalcaserate[data$CountryName==country])
  max.p <- max(working[,1])
  max.c <- max(working[,2])
  if(i==1){
    max.gri <- as.data.frame(cbind(country, max.p, max.c))
  }else{
    max.gri <- rbind(max.gri, cbind(country, max.p, max.c))
  }
}

max.gri[,2] <- as.numeric(paste(max.gri[,2]))
max.gri[,3] <- as.numeric(paste(max.gri[,3]))

max.gri <- max.gri[order(max.gri$max.c),]

lab <- subset(max.gri, max.gri$country %in% cntry)

plot(max.gri$max.p~max.gri$max.c, ylab="Maximum Government Response", xlab="Cases Per 1,000", pch=20, ylim=c(0,100), col="grey60")
points(lab$max.p~lab$max.c, pch=18, col="black",cex=1.5)
text(lab$max.c, lab$max.p, lab$country, pos=4, cex=.7)
loess.gri <- loess(max.p~max.c, data=max.gri)
lines(max.gri$max.c, predict(loess.gri), col="blue")

par(fig=c(0.5,1,0,0.5), new=TRUE)

for(i in 1:length(countries)){
  country <- countries[i]
  working <- cbind(data$ContainmentHealthIndex[data$CountryName==country], data$totalcaserate[data$CountryName==country])
  max.p <- max(working[,1])
  max.c <- max(working[,2])
  if(i==1){
    max.chi <- as.data.frame(cbind(country, max.p, max.c))
  }else{
    max.chi <- rbind(max.chi, cbind(country, max.p, max.c))
  }
}

max.chi[,2] <- as.numeric(paste(max.chi[,2]))
max.chi[,3] <- as.numeric(paste(max.chi[,3]))

max.chi <- max.chi[order(max.chi$max.c),]

lab <- subset(max.chi, max.chi$country %in% cntry)

plot(max.chi$max.p~max.chi$max.c, ylab="Maximum Health Containment", xlab="Cases Per 1,000", pch=20, col="grey60")
points(lab$max.p~lab$max.c, pch=18, col="black", cex=1.5)
text(lab$max.c, lab$max.p, lab$country, pos=4, cex=.7)
loess.chi <- loess(max.p~max.c, data=max.chi)
lines(max.chi$max.c, predict(loess.chi), col="blue")

par(fig=c(0.5,1,0.5,1), new=TRUE)

for(i in 1:length(countries)){
  country <- countries[i]
  working <- cbind(data$EconomicSupportIndex[data$CountryName==country], data$totalcaserate[data$CountryName==country])
  max.p <- max(working[,1])
  max.c <- max(working[,2])
  if(i==1){
    max.esi <- as.data.frame(cbind(country, max.p, max.c))
  }else{
    max.esi <- rbind(max.esi, cbind(country, max.p, max.c))
  }
}

max.esi[,2] <- as.numeric(paste(max.esi[,2]))
max.esi[,3] <- as.numeric(paste(max.esi[,3]))

max.esi <- max.esi[order(max.esi$max.c),]

lab <- subset(max.esi, max.esi$country %in% cntry)

cntry1 <- c("New Zealand", "El Salvador", "Belarus")
cntry2 <- c("Trinidad and Tobago", "Sweden", "United States")
cntry3 <- c("Switzerland")
lab1 <- subset(max.esi, max.esi$country %in% cntry1)
lab2 <- subset(max.esi, max.esi$country %in% cntry2)
lab3 <- subset(max.esi, max.esi$country %in% cntry3)

plot(max.esi$max.p~max.esi$max.c, ylab="Maximum Economic Support", xlab="Cases Per 1,000", pch=20, col="grey60")
points(lab$max.p~lab$max.c, pch=18, col="black", cex=1.5)
text(lab1$max.c, lab1$max.p, lab1$country, pos=4, cex=.65)
text(lab2$max.c, lab2$max.p, lab2$country, cex=.65, adj=c(.2,2))
text(lab3$max.c, lab3$max.p, lab3$country, pos=3, cex=.65)
loess.esi <- loess(max.p~max.c, data=max.esi)
lines(max.esi$max.c, predict(loess.esi), col="blue")

dev.off()

############################ Table 1 ###############################

data.week$fiscalmeasures <- log(data.week$fiscalmeasures+1)
data.week$healthinvestment <- log(data.week$healthinvestment+1)
data.week$gdp_norm <- data.week$gdp_norm/1000
data.week$agedpop <- data.week$agedpop*100

# Identify countries with all 0 for economic support, fiscal measures, and health investment
for(i in 1:length(countries)){
  country <- as.character(countries[i])
  working <- data.week$EconomicSupportIndex[data.week$CountryName==country]
  if(sum(working)==0){
    print(country)
  }
}

pdata <- data.week[which(data.week$CountryName!="Belarus"),]
pdata <- pdata[which(data.week$CountryName!="Burkina Faso"),]
pdata <- pdata[which(pdata$CountryName!="Kiribati"),]
pdata <- pdata[which(pdata$CountryName!="Liberia"),]
pdata <- pdata[which(pdata$CountryName!="Libya"),]
pdata <- pdata[which(pdata$CountryName!="Mozambique"),]
pdata <- pdata[which(pdata$CountryName!="Nicaragua"),]
pdata <- pdata[which(pdata$CountryName!="Tanzania"),]

pdata$week <- c(1:46)

## Create pdata.frame for plm
pdata <- pdata.frame(pdata, index=c("CountryName", "week"), drop.index=TRUE, row.names=TRUE)

## Test for unit roots
# Undifferenced
purtest(pdata$casesgrowthfactor, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$casesnewweekly, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$deathsgrowthfactor, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$deathsnewweekly, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$StringencyIndex, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$GovernmentResponseIndex, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$ContainmentHealthIndex, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$EconomicSupportIndex, pmax=3, exo="trend", test="hadri", lags="AIC")


# Differenced
pdata$casesgrowthfactor <- diff(pdata$casesgrowthfactor)
pdata$casesnewweekly <- diff(pdata$casesnewweekly)
pdata$deathsgrowthfactor <- diff(pdata$deathsgrowthfactor)
pdata$deathsnewweekly <- diff(pdata$deathsnewweekly)
pdata$StringencyIndex <- diff(pdata$StringencyIndex)
pdata$GovernmentResponseIndex <- diff(pdata$GovernmentResponseIndex)
pdata$ContainmentHealthIndex <- diff(pdata$ContainmentHealthIndex)

pdata <- na.omit(pdata)

purtest(pdata$casesgrowthfactor, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$casesnewweekly, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$deathsgrowthfactor, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$deathsnewweekly, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$StringencyIndex, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$GovernmentResponseIndex, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$ContainmentHealthIndex, pmax=3, exo="trend", test="hadri", lags="AIC")

pdata$StringencyIndex <- diff(pdata$StringencyIndex)
pdata$GovernmentResponseIndex <- diff(pdata$GovernmentResponseIndex)
pdata$ContainmentHealthIndex <- diff(pdata$ContainmentHealthIndex)

pdata <- na.omit(pdata)

purtest(pdata$StringencyIndex, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$GovernmentResponseIndex, pmax=3, exo="trend", test="hadri", lags="AIC")
purtest(pdata$ContainmentHealthIndex, pmax=3, exo="trend", test="hadri", lags="AIC")

#### Analysis for Table 1

### Government Response Index

#Fixed Effects models
gri1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
gri2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
gri3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
gri4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")

#Random effects test
#H0: Zero variance in individual-specific errors
plmtest(gri1.fe, effect="twoways", type="ghm")
plmtest(gri2.fe, effect="twoways", type="ghm")
plmtest(gri3.fe, effect="twoways", type="ghm")
plmtest(gri4.fe, effect="twoways", type="ghm")

gri1.re <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=pdata, model="random", random.method="walhus")
summary (gri1.re)
gri2.re <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=pdata, model="random", random.method="walhus")
summary (gri2.re)
gri3.re <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=pdata, model="random", random.method="walhus")
summary (gri3.re)
gri4.re <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=pdata, model="random", random.method="walhus")
summary (gri4.re)

#Hausmann Test to see if RE improve the model
#HO: Random Effects model consistent and efficient (preferred)
#H1: Fixed effects model consistent and preferred
phtest(gri1.fe, gri1.re) #FE is preferred
phtest(gri2.fe, gri2.re) #FE is preferred
phtest(gri3.fe, gri3.re) #FE is preferred
phtest(gri4.fe, gri4.re) #RE is preferred

## Government Response Index
pdf("gri.pdf", height=8, width=8)
#tiff("gri.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
gri1.output <- list()
for(i in 1:5){
  gri1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  gri1.output[[i]] <- gri1.fe
  #print(i)
  #print(summary(gri1.fe))
  lag <- i
  coefficient <- coef(summary(gri1.fe))[3,1]
  se <- coef(summary(gri1.fe))[3,2]
  ll <- coef(summary(gri1.fe))[3,1] - 1.96*coef(summary(gri1.fe))[3,2]
  ul <- coef(summary(gri1.fe))[3,1] + 1.96*coef(summary(gri1.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    gri1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    gri1.plot<- rbind(gri1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-0.2,0.2))
points(gri1.plot$lag, gri1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(gri1.plot$ll[i], gri1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-0.2,0.2,0.1), labels=seq(-0.2,0.2,0.1), las=2)
title(main="(a) Cases Growth Factor", ylab="Government Response Index", xlab="Lag Length in Weeks")

#New Weekly Cases
gri2.output <- list()
for(i in 1:5){
  gri2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  gri2.output[[i]] <- gri2.fe
  #print(i)
  #print(summary(gri2.fe))
  lag <- i
  coefficient <- coef(summary(gri2.fe))[3,1]
  se <- coef(summary(gri2.fe))[3,2]
  ll <- coef(summary(gri2.fe))[3,1] - 1.96*coef(summary(gri2.fe))[3,2]
  ul <- coef(summary(gri2.fe))[3,1] + 1.96*coef(summary(gri2.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    gri2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    gri2.plot<- rbind(gri2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-60,90))
points(gri2.plot$lag, gri2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(gri2.plot$ll[i], gri2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-60,90,10), labels=seq(-60,90,10), las=2)
title(main="(b) New Weekly Cases", ylab="Government Response Index", xlab="Lag Length in Weeks")

#Deaths Growth Factor
gri3.output <- list()
for(i in 1:5){
  gri3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  gri3.output[[i]] <- gri3.fe
  #print(i)
  #print(summary(gri3.fe))
  lag <- i
  coefficient <- coef(summary(gri3.fe))[3,1]
  se <- coef(summary(gri3.fe))[3,2]
  ll <- coef(summary(gri3.fe))[3,1] - 1.96*coef(summary(gri3.fe))[3,2]
  ul <- coef(summary(gri3.fe))[3,1] + 1.96*coef(summary(gri3.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    gri3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    gri3.plot<- rbind(gri3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(gri3.plot$lag, gri3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(gri3.plot$ll[i], gri3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.1,0.1,0.1), labels=seq(-0.1,0.1,0.1), las=2)
title(main="(c) Deaths Growth Factor", ylab="Government Response Index", xlab="Lag Length in Weeks")

#New Weekly Deaths
gri4.output <- list()
for(i in 1:5){
  gri4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  gri4.output[[i]] <- gri4.fe
  #print(i)
  #print(summary(gri4.fe))
  lag <- i
  coefficient <- coef(summary(gri4.fe))[3,1]
  se <- coef(summary(gri4.fe))[3,2]
  ll <- coef(summary(gri4.fe))[3,1] - 1.96*coef(summary(gri4.fe))[3,2]
  ul <- coef(summary(gri4.fe))[3,1] + 1.96*coef(summary(gri4.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    gri4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    gri4.plot<- rbind(gri4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(0,5),ylim=c(-2,2))
points(gri4.plot$lag, gri4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(gri4.plot$ll[i], gri4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-2, 2, 1), labels=seq(-2, 2, 1), las=2)
title(main="(d) New Weekly Deaths", ylab="Government Response Index", xlab="Lag Length in Weeks")
dev.off()

## Economic Support Index
pdf("esi.pdf", height=8, width=8)
#tiff("esi.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
for(i in 1:5){
  esi1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  #print(i)
  #print(summary(esi1.fe))
  lag <- i
  coefficient <- coef(summary(esi1.fe))[4,1]
  se <- coef(summary(esi1.fe))[4,2]
  ll <- coef(summary(esi1.fe))[4,1] - 1.96*coef(summary(esi1.fe))[4,2]
  ul <- coef(summary(esi1.fe))[4,1] + 1.96*coef(summary(esi1.fe))[4,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    esi1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    esi1.plot<- rbind(esi1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(esi1.plot$lag, esi1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(esi1.plot$ll[i], esi1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.1,0.1,0.1), labels=seq(-0.1,0.1,0.1), las=2)
title(main="(a) Cases Growth Factor", ylab="Economic Support Index", xlab="Lag Length in Weeks")

#New Weekly Cases
for(i in 1:5){
  esi2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  #print(i)
  #print(summary(esi2.fe))
  lag <- i
  coefficient <- coef(summary(esi2.fe))[4,1]
  se <- coef(summary(esi2.fe))[4,2]
  ll <- coef(summary(esi2.fe))[4,1] - 1.96*coef(summary(esi2.fe))[4,2]
  ul <- coef(summary(esi2.fe))[4,1] + 1.96*coef(summary(esi2.fe))[4,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    esi2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    esi2.plot<- rbind(esi2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-30,20))
points(esi2.plot$lag, esi2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(esi2.plot$ll[i], esi2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-30,20,10), labels=seq(-30,20,10), las=2)
title(main="(b) New Weekly Cases", ylab="Economic Support Index", xlab="Lag Length in Weeks")

#Deaths Growth Factor
for(i in 1:5){
  esi3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  #print(i)
  #print(summary(esi3.fe))
  lag <- i
  coefficient <- coef(summary(esi3.fe))[4,1]
  se <- coef(summary(esi3.fe))[4,2]
  ll <- coef(summary(esi3.fe))[4,1] - 1.96*coef(summary(esi3.fe))[4,2]
  ul <- coef(summary(esi3.fe))[4,1] + 1.96*coef(summary(esi3.fe))[4,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    esi3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    esi3.plot<- rbind(esi3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.03,0.03))
points(esi3.plot$lag, esi3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(esi3.plot$ll[i], esi3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.03,0.03,0.01), labels=seq(-0.03,0.03,0.01), las=2)
title(main="(c) Deaths Growth Factor", ylab="Economic Support Index", xlab="Lag Length in Weeks")

#New Weekly Deaths
for(i in 1:5){
  esi4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  #print(i)
  #print(summary(esi4.fe))
  lag <- i
  coefficient <- coef(summary(esi4.fe))[4,1]
  se <- coef(summary(esi4.fe))[4,2]
  ll <- coef(summary(esi4.fe))[4,1] - 1.96*coef(summary(esi4.fe))[4,2]
  ul <- coef(summary(esi4.fe))[4,1] + 1.96*coef(summary(esi4.fe))[4,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    esi4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    esi4.plot<- rbind(esi4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.7,0.1))
points(esi4.plot$lag, esi4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(esi4.plot$ll[i], esi4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=c(-0.7, -0.5, -0.3, -0.1, 0.1), labels=c(-0.7, -0.5, -0.3, -0.1, 0.1), las=2)
title(main="(d) New Weekly Deaths", ylab="Economic Support Index", xlab="Lag Length in Weeks")
dev.off()

## Fiscal Measures
pdf("fm.pdf", height=8, width=8)
#tiff("fm.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
for(i in 1:5){
  fm1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  #print(i)
  #print(summary(fm1.fe))
  lag <- i
  coefficient <- coef(summary(fm1.fe))[5,1]
  se <- coef(summary(fm1.fe))[5,2]
  ll <- coef(summary(fm1.fe))[5,1] - 1.96*coef(summary(fm1.fe))[5,2]
  ul <- coef(summary(fm1.fe))[5,1] + 1.96*coef(summary(fm1.fe))[5,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    fm1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    fm1.plot<- rbind(fm1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}


plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-0.6,0.6))
points(fm1.plot$lag, fm1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(fm1.plot$ll[i], fm1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6), labels=c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6), las=2)
title(main="(a) Cases Growth Factor", ylab="Fiscal Measures (log)", xlab="Lag Length in Weeks")

#New Weekly Cases
for(i in 1:5){
  fm2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  #print(i)
  #print(summary(fm2.fe))
  lag <- i
  coefficient <- coef(summary(fm2.fe))[5,1]
  se <- coef(summary(fm2.fe))[5,2]
  ll <- coef(summary(fm2.fe))[5,1] - 1.96*coef(summary(fm2.fe))[5,2]
  ul <- coef(summary(fm2.fe))[5,1] + 1.96*coef(summary(fm2.fe))[5,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    fm2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    fm2.plot<- rbind(fm2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-250,250))
points(fm2.plot$lag, fm2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(fm2.plot$ll[i], fm2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-250,250,50), labels=seq(-250,250,50), las=2)
title(main="(b) New Weekly Cases", ylab="Fiscal Measures (log)", xlab="Lag Length in Weeks")

#Deaths Growth Factor
for(i in 1:5){
  fm3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  #print(i)
  #print(summary(fm3.fe))
  lag <- i
  coefficient <- coef(summary(fm3.fe))[5,1]
  se <- coef(summary(fm3.fe))[5,2]
  ll <- coef(summary(fm3.fe))[5,1] - 1.96*coef(summary(fm3.fe))[5,2]
  ul <- coef(summary(fm3.fe))[5,1] + 1.96*coef(summary(fm3.fe))[5,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    fm3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    fm3.plot<- rbind(fm3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-.3,.3))
points(fm3.plot$lag, fm3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(fm3.plot$ll[i], fm3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=c(-.3,-.2,-.1,0,.1,.2,.3), labels=c(-.3,-.2,-.1,0,.1,.2,.3), las=2)
title(main="(c) Deaths Growth Factor", ylab="Fiscal Measures (log)", xlab="Lag Length in Weeks")

#New Daily Deaths
for(i in 1:5){
  fm4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  #print(i)
  #print(summary(fm4.fe))
  lag <- i
  coefficient <- coef(summary(fm4.fe))[5,1]
  se <- coef(summary(fm4.fe))[5,2]
  ll <- coef(summary(fm4.fe))[5,1] - 1.96*coef(summary(fm4.fe))[5,2]
  ul <- coef(summary(fm4.fe))[5,1] + 1.96*coef(summary(fm4.fe))[5,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    fm4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    fm4.plot<- rbind(fm4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-6,4))
points(fm4.plot$lag, fm4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(fm4.plot$ll[i], fm4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-6,4,2), labels=seq(-6,4,2), las=2)
title(main="(d) New Weekly Deaths", ylab="Fiscal Measures (log)", xlab="Lag Length in Weeks")
dev.off()

## Health Investment
pdf("hi.pdf", height=8, width=8)
#tiff("hi.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
for(i in 1:5){
  hi1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  #print(i)
  #print(summary(hi1.fe))
  lag <- i
  coefficient <- coef(summary(hi1.fe))[6,1]
  se <- coef(summary(hi1.fe))[6,2]
  ll <- coef(summary(hi1.fe))[6,1] - 1.96*coef(summary(hi1.fe))[6,2]
  ul <- coef(summary(hi1.fe))[6,1] + 1.96*coef(summary(hi1.fe))[6,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    hi1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    hi1.plot<- rbind(hi1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-1,1))
points(hi1.plot$lag, hi1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(hi1.plot$ll[i], hi1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-1,1,.5), labels=seq(-1,1,.5), las=2)
title(main="(a) Cases Growth Factor", ylab="Health Investments (log)", xlab="Lag Length in Weeks")

#New Weekly Cases
for(i in 1:5){
  hi2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  #print(i)
  #print(summary(hi2.fe))
  lag <- i
  coefficient <- coef(summary(hi2.fe))[6,1]
  se <- coef(summary(hi2.fe))[6,2]
  ll <- coef(summary(hi2.fe))[6,1] - 1.96*coef(summary(hi2.fe))[6,2]
  ul <- coef(summary(hi2.fe))[6,1] + 1.96*coef(summary(hi2.fe))[6,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    hi2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    hi2.plot<- rbind(hi2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(30,550))
points(hi2.plot$lag, hi2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(hi2.plot$ll[i], hi2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(30,550,50), labels=seq(30,550,50), las=2)
title(main="(b) New Weekly Cases", ylab="Health Investments (log)", xlab="Lag Length in Weeks")

#Deaths Growth Factor
for(i in 1:5){
  hi3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  #print(i)
  #print(summary(hi3.fe))
  lag <- i
  coefficient <- coef(summary(hi3.fe))[6,1]
  se <- coef(summary(hi3.fe))[6,2]
  ll <- coef(summary(hi3.fe))[6,1] - 1.96*coef(summary(hi3.fe))[6,2]
  ul <- coef(summary(hi3.fe))[6,1] + 1.96*coef(summary(hi3.fe))[6,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    hi3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    hi3.plot<- rbind(hi3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-.4,.4))
points(hi3.plot$lag, hi3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(hi3.plot$ll[i], hi3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-.4,.4,.2), labels=seq(-.4,.4,.2), las=2)
title(main="(c) Deaths Growth Factor", ylab="Health Investments (log)", xlab="Lag Length in Weeks")

#New Weekly Deaths
for(i in 1:5){
  hi4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  #print(i)
  #print(summary(hi4.fe))
  lag <- i
  coefficient <- coef(summary(hi4.fe))[6,1]
  se <- coef(summary(hi4.fe))[6,2]
  ll <- coef(summary(hi4.fe))[6,1] - 1.96*coef(summary(hi4.fe))[6,2]
  ul <- coef(summary(hi4.fe))[6,1] + 1.96*coef(summary(hi4.fe))[6,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    hi4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    hi4.plot<- rbind(hi4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-5,5))
points(hi4.plot$lag, hi4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(hi4.plot$ll[i], hi4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-5,5,2), labels=seq(-5,5,2), las=2)
title(main="(d) New Weekly Deaths", ylab="Health Investments (log)", xlab="Lag Length in Weeks")
dev.off()

## Containment Health Index
pdf("chi.pdf", height=8, width=8)
#tiff("chi.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
chi1.output <- list()
for(i in 1:5){
  chi1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(ContainmentHealthIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  chi1.output[[i]] <- chi1.fe
  #print(i)
  #print(summary(chi1.fe))
  lag <- i
  coefficient <- coef(summary(chi1.fe))[3,1]
  se <- coef(summary(chi1.fe))[3,2]
  ll <- coef(summary(chi1.fe))[3,1] - 1.96*coef(summary(chi1.fe))[3,2]
  ul <- coef(summary(chi1.fe))[3,1] + 1.96*coef(summary(chi1.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    chi1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    chi1.plot<- rbind(chi1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-.2,.2))
points(chi1.plot$lag, chi1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(chi1.plot$ll[i], chi1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-.2,.2,.1), labels=seq(-.2,.2,.1), las=2)
title(main="(a) Cases Growth Factor", ylab="Containment Index", xlab="Lag Length in Weeks")

#New Daily Cases
chi2.output <- list()
for(i in 1:5){
  chi2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(ContainmentHealthIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  chi2.output[[i]] <- chi2.fe
  #print(i)
  #print(summary(chi2.fe))
  lag <- i
  coefficient <- coef(summary(chi2.fe))[3,1]
  se <- coef(summary(chi2.fe))[3,2]
  ll <- coef(summary(chi2.fe))[3,1] - 1.96*coef(summary(chi2.fe))[3,2]
  ul <- coef(summary(chi2.fe))[3,1] + 1.96*coef(summary(chi2.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    chi2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    chi2.plot<- rbind(chi2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-50,100))
points(chi2.plot$lag, chi2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(chi2.plot$ll[i], chi2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-50,100,50), labels=seq(-50,100,50), las=2)
title(main="(b) New Weekly Cases", ylab="Containment Health Index", xlab="Lag Length in Weeks")

#Deaths Growth Factor
chi3.output <- list()
for(i in 1:5){
  chi3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(ContainmentHealthIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  chi3.output[[i]] <- chi3.fe
  #print(i)
  #print(summary(chi3.fe))
  lag <- i
  coefficient <- coef(summary(chi3.fe))[3,1]
  se <- coef(summary(chi3.fe))[3,2]
  ll <- coef(summary(chi3.fe))[3,1] - 1.96*coef(summary(chi3.fe))[3,2]
  ul <- coef(summary(chi3.fe))[3,1] + 1.96*coef(summary(chi3.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    chi3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    chi3.plot<- rbind(chi3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(chi3.plot$lag, chi3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(chi3.plot$ll[i], chi3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.1,0.1,0.1), labels=seq(-0.1,0.1,0.1), las=2)
title(main="(c) Deaths Growth Factor", ylab="Containment Health Index", xlab="Lag Length in Weeks")

#New Daily Deaths
chi4.output <- list()
for(i in 1:5){
  chi4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(ContainmentHealthIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  chi4.output[[i]] <- chi4.fe
  #print(i)
  #print(summary(chi4.fe))
  lag <- i
  coefficient <- coef(summary(chi4.fe))[3,1]
  se <- coef(summary(chi4.fe))[3,2]
  ll <- coef(summary(chi4.fe))[3,1] - 1.96*coef(summary(chi4.fe))[3,2]
  ul <- coef(summary(chi4.fe))[3,1] + 1.96*coef(summary(chi4.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    chi4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    chi4.plot<- rbind(chi4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-2,2))
points(chi4.plot$lag, chi4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(chi4.plot$ll[i], chi4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-2,2,1), labels=seq(-2,2,1), las=2)
title(main="(d) New Weekly Deaths", ylab="Containment Health Index", xlab="Lag Length in Weeks")
dev.off()

## Stringency Index
pdf("si.pdf", height=8, width=8)
#tiff("si.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
si1.output <- list()
for(i in 1:5){
  si1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(StringencyIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  si1.output[[i]] <- si1.fe
  #print(i)
  #print(summary(si1.fe))
  lag <- i
  coefficient <- coef(summary(si1.fe))[3,1]
  se <- coef(summary(si1.fe))[3,2]
  ll <- coef(summary(si1.fe))[3,1] - 1.96*coef(summary(si1.fe))[3,2]
  ul <- coef(summary(si1.fe))[3,1] + 1.96*coef(summary(si1.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    si1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    si1.plot<- rbind(si1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(si1.plot$lag, si1.plot$coefficient, pch=20)
for(i in 1:20){
  lines(c(i,i), c(si1.plot$ll[i], si1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-.1,.1,.1), labels=seq(-.1,.1,.1), las=2)
title(main="(a) Cases Growth Factor", ylab="Stringency Index", xlab="Lag Length in Weeks")

#New Weekly Cases
si2.output <- list()
for(i in 1:5){
  si2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(StringencyIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  si2.output[[i]] <- si2.fe
  #print(i)
  #print(summary(si2.fe))
  lag <- i
  coefficient <- coef(summary(si2.fe))[3,1]
  se <- coef(summary(si2.fe))[3,2]
  ll <- coef(summary(si2.fe))[3,1] - 1.96*coef(summary(si2.fe))[3,2]
  ul <- coef(summary(si2.fe))[3,1] + 1.96*coef(summary(si2.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    si2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    si2.plot<- rbind(si2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-40,60))
points(si2.plot$lag, si2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(si2.plot$ll[i], si2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-40,60,20), labels=seq(-40,60,20), las=2)
title(main="(b) New Weekly Cases", ylab="Stringency Index", xlab="Lag Length in Weeks")

#Deaths Growth Factor
si3.output <- list()
for(i in 1:5){
  si3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(StringencyIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  si3.output[[i]] <- si3.fe
  #print(i)
  #print(summary(si3.fe))
  lag <- i
  coefficient <- coef(summary(si3.fe))[3,1]
  se <- coef(summary(si3.fe))[3,2]
  ll <- coef(summary(si3.fe))[3,1] - 1.96*coef(summary(si3.fe))[3,2]
  ul <- coef(summary(si3.fe))[3,1] + 1.96*coef(summary(si3.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    si3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    si3.plot<- rbind(si3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(si3.plot$lag, si3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(si3.plot$ll[i], si3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.1,0.1,0.1), labels=seq(-0.1,0.1,0.1), las=2)
title(main="(c) Deaths Growth Factor", ylab="Stringency Index", xlab="Lag Length in Weeks")

#New Daily Deaths
si4.output <- list()
for(i in 1:5){
  si4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(StringencyIndex,i) + lag(EconomicSupportIndex, i) + lag(fiscalmeasures, i) + lag(healthinvestment, i) + agedpop + gdp_norm + popdensity, data=pdata, model="pooling", effect="twoways")
  si4.output[[i]] <- si4.fe
  #print(i)
  #print(summary(si4.fe))
  lag <- i
  coefficient <- coef(summary(si4.fe))[3,1]
  se <- coef(summary(si4.fe))[3,2]
  ll <- coef(summary(si4.fe))[3,1] - 1.96*coef(summary(si4.fe))[3,2]
  ul <- coef(summary(si4.fe))[3,1] + 1.96*coef(summary(si4.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    si4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    si4.plot<- rbind(si4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-2,1))
points(si4.plot$lag, si4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(si4.plot$ll[i], si4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-2,1,1), labels=seq(-2,1,1), las=2)
title(main="(d) New Weekly Deaths", ylab="Stringency Index", xlab="Lag Length in Weeks")
dev.off()

#### Supplemental Information

stargazer(gri1.output)
stargazer(gri2.output)
stargazer(gri3.output)
stargazer(gri4.output)
stargazer(chi1.output)
stargazer(chi2.output)
stargazer(chi3.output)
stargazer(chi4.output)
stargazer(si1.output)
stargazer(si2.output)
stargazer(si3.output)
stargazer(si4.output)


##### Additional Code for Replication Project ###############################

# Install and load additional packages
install.packages(c("ggplo2", "gridExtra"))

library(ggplot2)
library(gridExtra)

##### Corrected Code for Figure 1 ####
## Corrected SI, GRI, CHI and ESI Over Time
pdf("figure1_corrected.pdf", height=8.5, width=11)
#tiff("figure1.tiff", height=8.5, width=11, units="in", res=600)
plot.new()
par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.window(xlim=c(0,345), ylim=c(0,100))
for(i in 1:length(countries)){
  country <- countries[i]
  lines(data$day[data$CountryName==country],data$StringencyIndex[data$CountryName==country], col="gray50")
}
lines(data$day[data$CountryName=="United States"],data$StringencyIndex[data$CountryName=="United States"], col="dodgerblue4", lwd=3)
text(150, 20, labels="United States", font=2, col="dodgerblue4")
lines(data$day[data$CountryName=="France"],data$StringencyIndex[data$CountryName=="France"], col="darkred", lwd=3)
text(150, 95, labels="France", font=2, col="darkred")
axis(1, at=c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315), labels=c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1"))
axis(2, at=seq(0,100,10), labels=seq(0,100,10), las=2)
title(ylab="Stringency Index")
#dev.off()

#pdf("govtresponse_allcountries.pdf", height=4, width=8)
#plot.new()
par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.window(xlim=c(0,345), ylim=c(0,100))
for(i in 1:length(countries)){
  country <- countries[i]
  lines(data$day[data$CountryName==country],data$GovernmentResponseIndex[data$CountryName==country], col="gray50")
}
lines(data$day[data$CountryName=="United States"],data$GovernmentResponseIndex[data$CountryName=="United States"], col="dodgerblue4", lwd=3)
text(150, 20, labels="United States", font=2, col="dodgerblue4")
lines(data$day[data$CountryName=="France"],data$GovernmentResponseIndex[data$CountryName=="France"], col="darkred", lwd=3)
text(150, 95, labels="France", font=2, col="darkred")
axis(1, at=c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315), labels=c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1"))
axis(2, at=seq(0,100,10), labels=seq(0,100,10), las=2)
title(ylab="Government Response Index")
#dev.off()

#pdf("containment_allcountries.pdf", height=4, width=8)
#plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(0,345), ylim=c(0,100))
for(i in 1:length(countries)){
  country <- countries[i]
  lines(data$day[data$CountryName==country],data$ContainmentHealthIndex[data$CountryName==country], col="gray50")
}
lines(data$day[data$CountryName=="United States"],data$ContainmentHealthIndex[data$CountryName=="United States"], col="dodgerblue4", lwd=3)
text(150, 20, labels="United States", font=2, col="dodgerblue4")
lines(data$day[data$CountryName=="France"],data$ContainmentHealthIndex[data$CountryName=="France"], col="darkred", lwd=3)
text(150, 95, labels="France", font=2, col="darkred")
axis(1, at=c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315), labels=c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1"))
axis(2, at=seq(0,100,10), labels=seq(0,100,10), las=2)
title(ylab="Containment Health Index")
#dev.off()

#pdf("econsupport_allcountries.pdf", height=4, width=8)
#plot.new()
par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.window(xlim=c(0,345), ylim=c(0,100))
for(i in 1:length(countries)){
  country <- countries[i]
  lines(data$day[data$CountryName==country],data$EconomicSupportIndex[data$CountryName==country], col="gray50")
}
lines(data$day[data$CountryName=="United States"],data$EconomicSupportIndex[data$CountryName=="United States"], col="dodgerblue4", lwd=3)
text(150, 20, labels="United States", font=2, col="dodgerblue4")
lines(data$day[data$CountryName=="France"],data$EconomicSupportIndex[data$CountryName=="France"], col="darkred", lwd=3)
text(150, 95, labels="France", font=2, col="darkred")
axis(1, at=c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315), labels=c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1"))
axis(2, at=seq(0,100,10), labels=seq(0,100,10), las=2)
title(ylab="Economic Support Index")
dev.off()



##### Checking test results ####
#Hausman Test to see if RE improve the model
#HO: Random Effects model consistent and efficient (preferred)
#H1: Fixed effects model consistent and preferred
phtest(gri1.fe, gri1.re) #"FE is preferred", and statistically significant 
phtest(gri2.fe, gri2.re) #"FE is preferred", yet not statistically significant
phtest(gri3.fe, gri3.re) #"FE is preferred", yet not statistically significant
phtest(gri4.fe, gri4.re) #"RE is preferred", yet statistically significant



##### Inspect Analyses, Generate Membership ####
# Note most coefficients lack strong statistical significance.
# Adjusted R-Squared values also do not pass much beyond 0.25.
# Coupled together, this indicates the models are not necessarily powerful estimators,
# though the F-statistics are all statistically significant, (giving  evidence
# indicating the effect of at least one predictor is statistically significant).
lapply(gri1.output, summary)
lapply(gri2.output, summary)
lapply(gri3.output, summary)
lapply(gri4.output, summary)
lapply(chi1.output, summary)
lapply(chi2.output, summary)
lapply(chi3.output, summary)
lapply(chi4.output, summary)
lapply(si1.output, summary)
lapply(si2.output, summary)
lapply(si3.output, summary)
lapply(si4.output, summary)

## Assigning membership groups of Over- and Under-Response # "Above" and "Below"####
# Calculate residuals for each point in each plot
new_si <- data.frame("country" = max.si$country)
new_gri <- data.frame("country" = max.gri$country)
new_chi <- data.frame("country" = max.chi$country)
new_esi <- data.frame("country" = max.esi$country)
new_si$residuals <- resid(loess.ms)
new_gri$residuals <- resid(loess.gri)
new_chi$residuals <- resid(loess.chi)
new_esi$residuals <- resid(loess.esi)

# Create dummy variables for membership based on residuals
new_si$si_membership <- factor(ifelse(new_si$residuals > 0, 1, 0),
                               labels = c("Below", "Above"))
new_gri$gri_membership <- factor(ifelse(new_gri$residuals > 0, 1, 0),
                                 labels = c("Below", "Above"))
new_chi$chi_membership <- factor(ifelse(new_chi$residuals > 0, 1, 0),
                                 labels = c("Below", "Above"))
new_esi$esi_membership <- factor(ifelse(new_esi$residuals > 0, 1, 0),
                                 labels = c("Below", "Above"))

##### NEW DATA FOR PLOTTING #### 
# Prepare new data frame for plotting
new_data <- data
new_data$country <- new_data$CountryName # Rename for merging
# Merge membership dummy variables
new_data <- merge(new_data, new_si[c("country", "si_membership")], by = "country")
new_data <- merge(new_data, new_gri[c("country", "gri_membership")], by = "country")
new_data <- merge(new_data, new_chi[c("country", "chi_membership")], by = "country")
new_data <- merge(new_data, new_esi[c("country", "esi_membership")], by = "country")

## Plot Stringency Index ####
theme_set(theme_classic())
# Create Average Stringency Index for countries labelled Above and Below
average_stringency <- aggregate(StringencyIndex ~ si_membership + day, data = new_data, FUN = mean)
# Create ggplot object
si_plot1 <- ggplot(new_data, aes(x = day, y = StringencyIndex, color = si_membership, group = si_membership)) +
  # Add lines for countries, grouped by Under/Over Response
  geom_line(aes(group = CountryName), linewidth = 0.5, alpha = 0.5) +
  # Add Average Lines for each group
  geom_line(data = average_stringency[average_stringency$si_membership == "Above", ], 
            aes(group = si_membership, color = "Average Above"), 
            linetype = "dashed", size = 1.5, alpha = 1, color = "darkorange") +
  geom_text(data = average_stringency[average_stringency$si_membership == "Above", ],
            aes(x = 300, y = 95, label = "Average Above"), color = "darkorange", fontface = 2,
            alpha = 0.6) +
  
  geom_line(data = average_stringency[average_stringency$si_membership == "Below", ], 
            aes(group = si_membership, color = "Average Below"), 
            linetype = "dashed", size = 1.5, alpha = 1, color = "darkgreen") +
  geom_text(data = average_stringency[average_stringency$si_membership == "Below", ],
            aes(x = 300, y = 20, label = "Average Below"), color = "darkgreen", fontface = 2,
            alpha = 0.6) +
  # Change base line colors for groups
  scale_color_manual(values = c("Above" = "pink", "Below" = "yellow")) +
  labs(color = "Response \nMembership") +
  # Add specific lines and labels for United States and France
  geom_line(data = subset(new_data, CountryName == "United States"), size = 1.5, color = "dodgerblue4", alpha = 0.6) +
  geom_text(data = subset(new_data, CountryName == "United States"), aes(x = 150, y = 20, label = "United States"), color = "dodgerblue4", fontface = 2, alpha = 0.6) +
  
  geom_line(data = subset(new_data, CountryName == "France"), size = 1.5, color = "darkred", alpha = 0.6) +
  geom_text(data = subset(new_data, CountryName == "France"), aes(x = 150, y = 95, label = "France"), color = "darkred", fontface = 2, alpha = 0.6) +
  # Set axis labels
  labs(x = "Day", y = "Stringency Index") +
  # Set axis points and labels in line with authors' specifications
  scale_x_continuous(breaks = c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315),
                     labels = c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1")) +
  scale_y_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10), limits = c(0, 100)) #+
# Set plot title
#ggtitle("Stringency Index")

## Plot Government Response Index ####
# Create Average Government Response Index for countries labelled Above and Below
average_gri <- aggregate(GovernmentResponseIndex ~ gri_membership + day, data = new_data, FUN = mean)
# Create ggplot object
gri_plot1 <- ggplot(new_data, aes(x = day, y = GovernmentResponseIndex, color = gri_membership, group = gri_membership)) +
  # Add lines for countries, grouped by Under/Over Response
  geom_line(aes(group = CountryName), linewidth = 0.5, alpha = 0.5) +
  # Add Average Lines for each group
  geom_line(data = average_gri[average_gri$gri_membership == "Above", ], 
            aes(group = gri_membership, color = "Average Above"), 
            linetype = "dashed", size = 1.5, alpha = 1, color = "darkorange") +
  geom_text(data = average_gri[average_gri$gri_membership == "Above", ],
            aes(x = 300, y = 95, label = "Average Above"), color = "darkorange", fontface = 2,
            alpha = 0.6) +
  
  geom_line(data = average_gri[average_gri$gri_membership == "Below", ], 
            aes(group = gri_membership, color = "Average Below"), 
            linetype = "dashed", size = 1.5, alpha = 1, color = "darkgreen") +
  geom_text(data = average_gri[average_gri$gri_membership == "Below", ],
            aes(x = 300, y = 20, label = "Average Below"), color = "darkgreen", fontface = 2,
            alpha = 0.6) +
  # Change base line colors for groups
  scale_color_manual(values = c("Above" = "pink", "Below" = "yellow")) +
  labs(color = "Response \nMembership") +
  # Add specific lines and labels for United States and France
  geom_line(data = subset(new_data, CountryName == "United States"), size = 1.5, color = "dodgerblue4", alpha = 0.6) +
  geom_text(data = subset(new_data, CountryName == "United States"), aes(x = 150, y = 20, label = "United States"), color = "dodgerblue4", fontface = 2, alpha = 0.6) +
  
  geom_line(data = subset(new_data, CountryName == "France"), size = 1.5, color = "darkred", alpha = 0.6) +
  geom_text(data = subset(new_data, CountryName == "France"), aes(x = 150, y = 95, label = "France"), color = "darkred", fontface = 2, alpha = 0.6) +
  # Set axis labels
  labs(x = "Day", y = "Government Response Index") +
  # Set axis points and labels in line with authors' specifications
  scale_x_continuous(breaks = c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315),
                     labels = c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1")) +
  scale_y_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10), limits = c(0, 100)) #+
# Set plot title
#ggtitle("Government Response Index")

## Plot Containment Health Index ####
# Create Average Government Response Index for countries labelled Above and Below
average_chi <- aggregate(ContainmentHealthIndex ~ chi_membership + day, data = new_data, FUN = mean)
# Create ggplot object
chi_plot1 <- ggplot(new_data, aes(x = day, y = ContainmentHealthIndex, color = chi_membership, group = chi_membership)) +
  # Add lines for countries, grouped by Under/Over Response
  geom_line(aes(group = CountryName), linewidth = 0.5, alpha = 0.5) +
  # Add Average Lines for each group
  geom_line(data = average_chi[average_chi$chi_membership == "Above", ], 
            aes(group = chi_membership, color = "Average Above"), 
            linetype = "dashed", size = 1.5, alpha = 1, color = "darkorange") +
  geom_text(data = average_chi[average_chi$chi_membership == "Above", ],
            aes(x = 300, y = 95, label = "Average Above"), color = "darkorange", fontface = 2,
            alpha = 0.6) +
  
  geom_line(data = average_chi[average_chi$chi_membership == "Below", ], 
            aes(group = chi_membership, color = "Average Below"), 
            linetype = "dashed", size = 1.5, alpha = 1, color = "darkgreen") +
  geom_text(data = average_chi[average_chi$chi_membership == "Below", ],
            aes(x = 300, y = 20, label = "Average Below"), color = "darkgreen", fontface = 2,
            alpha = 0.6) +
  # Change base line colors for groups
  scale_color_manual(values = c("Above" = "pink", "Below" = "yellow")) +
  labs(color = "Response \nMembership") +
  # Add specific lines and labels for United States and France
  geom_line(data = subset(new_data, CountryName == "United States"), size = 1.5, color = "dodgerblue4", alpha = 0.6) +
  geom_text(data = subset(new_data, CountryName == "United States"), aes(x = 150, y = 20, label = "United States"), color = "dodgerblue4", fontface = 2, alpha = 0.6) +
  
  geom_line(data = subset(new_data, CountryName == "France"), size = 1.5, color = "darkred", alpha = 0.6) +
  geom_text(data = subset(new_data, CountryName == "France"), aes(x = 150, y = 95, label = "France"), color = "darkred", fontface = 2, alpha = 0.6) +
  # Set axis labels
  labs(x = "Day", y = "Containment Health Index") +
  # Set axis points and labels in line with authors' specifications
  scale_x_continuous(breaks = c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315),
                     labels = c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1")) +
  scale_y_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10), limits = c(0, 100)) #+
# Set plot title
#ggtitle("Containment Health Index")

## Plot Economic Support Index ####
# Create Average Government Response Index for countries labelled Above and Below
average_esi <- aggregate(EconomicSupportIndex ~ esi_membership + day, data = new_data, FUN = mean)
# Create ggplot object
esi_plot1 <- ggplot(new_data, aes(x = day, y = EconomicSupportIndex, color = esi_membership, group = esi_membership)) +
  # Add lines for countries, grouped by Under/Over Response
  geom_line(aes(group = CountryName), linewidth = 0.5, alpha = 0.5) +
  # Add Average Lines for each group
  geom_line(data = average_esi[average_esi$esi_membership == "Above", ], 
            aes(group = esi_membership, color = "Average Above"), 
            linetype = "dashed", size = 1.5, alpha = 1, color = "darkorange") +
  geom_text(data = average_esi[average_esi$esi_membership == "Above", ],
            aes(x = 300, y = 95, label = "Average Above"), color = "darkorange", fontface = 2,
            alpha = 0.6) +
  
  geom_line(data = average_esi[average_esi$esi_membership == "Below", ], 
            aes(group = esi_membership, color = "Average Below"), 
            linetype = "dashed", size = 1.5, alpha = 1, color = "darkgreen") +
  geom_text(data = average_esi[average_esi$esi_membership == "Below", ],
            aes(x = 300, y = 20, label = "Average Below"), color = "darkgreen", fontface = 2,
            alpha = 0.6) +
  # Change base line colors for groups
  scale_color_manual(values = c("Above" = "pink", "Below" = "yellow")) +
  labs(color = "Response \nMembership") +
  # Add specific lines and labels for United States and France
  geom_line(data = subset(new_data, CountryName == "United States"), size = 1.5, color = "dodgerblue4", alpha = 0.6) +
  geom_text(data = subset(new_data, CountryName == "United States"), aes(x = 150, y = 20, label = "United States"), color = "dodgerblue4", fontface = 2, alpha = 0.6) +
  
  geom_line(data = subset(new_data, CountryName == "France"), size = 1.5, color = "darkred", alpha = 0.6) +
  geom_text(data = subset(new_data, CountryName == "France"), aes(x = 150, y = 95, label = "France"), color = "darkred", fontface = 2, alpha = 0.6) +
  # Set axis labels
  labs(x = "Day", y = "Economic Support Index") +
  # Set axis points and labels in line with authors' specifications
  scale_x_continuous(breaks = c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315),
                     labels = c("2/1", "3/1", "4/1", "5/1", "6/1", "7/1", "8/1", "9/1", "10/1", "11/1", "12/1")) +
  scale_y_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10), limits = c(0, 100)) #+
# Set plot title
#ggtitle("Economic Support Index")

# Plot to one graphic like authors
pdf("all_plots1.pdf", height = 8.5, width = 11)
grid.arrange(si_plot1, gri_plot1, chi_plot1, esi_plot1, ncol = 2)
dev.off()


##### SUBSETTING PANEL DATA ####
# Create "country" variable to enable merging - modify for consistency
new_pdata <- pdata
new_pdata$country <- sub("-\\d+$", "", rownames(pdata))

# Vectors for country members
esi_Above <- as.vector(new_esi[new_esi$esi_membership == "Above", "country"])
esi_Below <- as.vector(new_esi[new_esi$esi_membership == "Below", "country"])

gri_Above <- as.vector(new_gri[new_gri$gri_membership == "Above", "country"])
gri_Below <- as.vector(new_gri[new_gri$gri_membership == "Below", "country"])

si_Above <- as.vector(new_si[new_si$si_membership == "Above", "country"])
si_Below <- as.vector(new_si[new_si$si_membership == "Below", "country"])

chi_Above <- as.vector(new_chi[new_chi$chi_membership == "Above", "country"])
chi_Below <- as.vector(new_chi[new_chi$chi_membership == "Below", "country"])

# Subsetting by membership
a_esi_new_pdata <- new_pdata[new_pdata$country %in% esi_Above, ]
b_esi_new_pdata <- new_pdata[new_pdata$country %in% esi_Below, ]

a_gri_new_pdata <- new_pdata[new_pdata$country %in% gri_Above, ]
b_gri_new_pdata <- new_pdata[new_pdata$country %in% gri_Below, ]

a_si_new_pdata <- new_pdata[new_pdata$country %in% si_Above, ]
b_si_new_pdata <- new_pdata[new_pdata$country %in% si_Below, ]

a_chi_new_pdata <- new_pdata[new_pdata$country %in% chi_Above, ]
b_chi_new_pdata <- new_pdata[new_pdata$country %in% chi_Below, ]

##### UPDATED MODELS ####
## Government Response Index####
# Updated Fixed Effects models for Government Response Index (Above)
agri1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_gri_new_pdata, model="pooling", effect="twoways")
agri2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_gri_new_pdata, model="pooling", effect="twoways")
agri3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_gri_new_pdata, model="pooling", effect="twoways")
agri4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_gri_new_pdata, model="pooling", effect="twoways")

# Updated Random effects test for Government Response Index (Above)
# H0: Zero variance in individual-specific errors
plmtest(agri1.fe, effect="twoways", type="ghm") # Statistically significant
plmtest(agri2.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(agri3.fe, effect="twoways", type="ghm") # Statistically significant
plmtest(agri4.fe, effect="twoways", type="ghm") # Not statistically significant

# Updated Random Effects models for Government Reponse Index (Above)
agri1.re <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_gri_new_pdata, model="random", random.method="walhus")
summary (agri1.re)
agri2.re <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_gri_new_pdata, model="random", random.method="walhus")
summary (agri2.re)
agri3.re <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_gri_new_pdata, model="random", random.method="walhus")
summary (agri3.re)
agri4.re <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_gri_new_pdata, model="random", random.method="walhus")
summary (agri4.re)

#Hausmann Test to see if FE improve the model (Above)
#HO: Random Effects model consistent and efficient (preferred)
#H1: Fixed effects model consistent and preferred
phtest(agri1.fe, agri1.re) # FE is preferred, statistically significant
phtest(agri2.fe, agri2.re) # RE is preferred, not statistically significant
phtest(agri3.fe, agri3.re) # RE is preferred, not statistically significant
phtest(agri4.fe, agri4.re) # FE is preferred, statistically significant

# Updated Fixed Effects models for Government Response Index (Below)
bgri1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_gri_new_pdata, model="pooling", effect="twoways")
bgri2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_gri_new_pdata, model="pooling", effect="twoways")
bgri3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_gri_new_pdata, model="pooling", effect="twoways")
bgri4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_gri_new_pdata, model="pooling", effect="twoways")

# Updated Random effects test for Government Response Index (Below)
# H0: Zero variance in individual-specific errors
plmtest(bgri1.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(bgri2.fe, effect="twoways", type="ghm") # Statistically significant
plmtest(bgri3.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(bgri4.fe, effect="twoways", type="ghm") # Not statistically significant

# Unpdated Random Effects models for Government Response Index (Below)
bgri1.re <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_esi_new_pdata, model="random", random.method="walhus")
summary (bgri1.re)
bgri2.re <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_esi_new_pdata, model="random", random.method="walhus")
summary (bgri2.re)
bgri3.re <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_esi_new_pdata, model="random", random.method="walhus")
summary (bgri3.re)
bgri4.re <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_esi_new_pdata, model="random", random.method="walhus")
summary (bgri4.re)

#Hausmann Test to see if FE improve the model (Below)
#HO: Random Effects model consistent and efficient (preferred)
#H1: Fixed effects model consistent and preferred
phtest(bgri1.fe, bgri1.re) # RE is preferred, not statistically significant
phtest(bgri2.fe, bgri2.re) # RE is preferred, not statistically significant
phtest(bgri3.fe, bgri3.re) # FE is preferred, statistically significant
phtest(bgri4.fe, bgri4.re) # FE is preferred, statistically significant


## Economic Support Index ####
# Updated Fixed Effects models for Economic Support Index (Above)
aesi1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_esi_new_pdata, model="pooling", effect="twoways")
aesi2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_esi_new_pdata, model="pooling", effect="twoways")
aesi3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_esi_new_pdata, model="pooling", effect="twoways")
aesi4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_esi_new_pdata, model="pooling", effect="twoways")

# Updated Random effects test for Economic Support Index (Above)
# H0: Zero variance in individual-specific errors
plmtest(aesi1.fe, effect="twoways", type="ghm") # Statistically significant
plmtest(aesi2.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(aesi3.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(aesi4.fe, effect="twoways", type="ghm") # Statistically significant

# Updated Random Effects models for Economic Support Index (Above)
aesi1.re <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_esi_new_pdata, model="random", random.method="walhus")
summary (aesi1.re)
aesi2.re <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_esi_new_pdata, model="random", random.method="walhus")
summary (aesi2.re)
aesi3.re <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_esi_new_pdata, model="random", random.method="walhus")
summary (aesi3.re)
aesi4.re <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_esi_new_pdata, model="random", random.method="walhus")
summary (aesi4.re)

#Hausmann Test to see if RE improve the model (Above)
#HO: Random Effects model consistent and efficient (preferred)
#H1: Fixed effects model consistent and preferred
phtest(aesi1.fe, aesi1.re) # FE is preferred, statistically significant
phtest(aesi2.fe, aesi2.re) # RE is preferred, not statistically significant
phtest(aesi3.fe, aesi3.re) # RE is preferred, not statistically significant
phtest(aesi4.fe, aesi4.re) # FE is preferred, statistically significant

# Updated Fixed Effects models for Economic Support Index (Below)
besi1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_esi_new_pdata, model="pooling", effect="twoways")
besi2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_esi_new_pdata, model="pooling", effect="twoways")
besi3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_esi_new_pdata, model="pooling", effect="twoways")
besi4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_esi_new_pdata, model="pooling", effect="twoways")

# Updated Random effects test for Economic Support Index (Below)
# H0: Zero variance in individual-specific errors
plmtest(besi1.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(besi2.fe, effect="twoways", type="ghm") # Statistically significant
plmtest(besi3.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(besi4.fe, effect="twoways", type="ghm") # Not statistically significant

# Updated Random Effects models for Economic Support Index (Below)
besi1.re <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_esi_new_pdata, model="random", random.method="walhus")
summary (besi1.re)
besi2.re <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_esi_new_pdata, model="random", random.method="walhus")
summary (besi2.re)
besi3.re <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_esi_new_pdata, model="random", random.method="walhus")
summary (besi3.re)
besi4.re <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_esi_new_pdata, model="random", random.method="walhus")
summary (besi4.re)

#Hausmann Test to see if RE improve the model (Below)
#HO: Random Effects model consistent and efficient (preferred)
#H1: Fixed effects model consistent and preferred
phtest(besi1.fe, besi1.re) # RE is preferred, not statistically significant
phtest(besi2.fe, besi2.re) # RE is preferred, not statistically significant
phtest(besi3.fe, besi3.re) # RE is preferred, not statistically significant
phtest(besi4.fe, besi4.re) # FE is preferred, statistically significant


## Stringency Index ####
# Updated Fixed Effects models for Stringency Index (Above)
asi1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_si_new_pdata, model="pooling", effect="twoways")
asi2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_si_new_pdata, model="pooling", effect="twoways")
asi3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_si_new_pdata, model="pooling", effect="twoways")
asi4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_si_new_pdata, model="pooling", effect="twoways")

# Updated Random effects test for Stringency Index (Above)
# H0: Zero variance in individual-specific errors
plmtest(asi1.fe, effect="twoways", type="ghm") # Statistically significant
plmtest(asi2.fe, effect="twoways", type="ghm") # Statistically significant
plmtest(asi3.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(asi4.fe, effect="twoways", type="ghm") # Not statistically significant

# Updated Random Effects models for Stringency Index (Above)
asi1.re <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_si_new_pdata, model="random", random.method="walhus")
summary(asi1.re)
asi2.re <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_si_new_pdata, model="random", random.method="walhus")
summary(asi2.re)
asi3.re <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_si_new_pdata, model="random", random.method="walhus")
summary(asi3.re)
asi4.re <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_si_new_pdata, model="random", random.method="walhus")
summary(asi4.re)

#Hausmann Test to see if RE improve the model (Above)
#HO: Random Effects model consistent and efficient (preferred)
#H1: Fixed effects model consistent and preferred
phtest(asi1.fe, asi1.re) # FE is preferred, statistically significant
phtest(asi2.fe, asi2.re) # RE is preferred, not statistically significant
phtest(asi3.fe, asi3.re) # RE is preferred, not statistically significant
phtest(asi4.fe, asi4.re) # FE is preferred, statistically significant

# Updated Fixed Effects models for Stringency Index (Below)
bsi1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_si_new_pdata, model="pooling", effect="twoways")
bsi2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_si_new_pdata, model="pooling", effect="twoways")
bsi3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_si_new_pdata, model="pooling", effect="twoways")
bsi4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_si_new_pdata, model="pooling", effect="twoways")

# Updated Random effects test for Stringency Index (Below)
# H0: Zero variance in individual-specific errors
plmtest(bsi1.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(bsi2.fe, effect="twoways", type="ghm") # Statistically significant
plmtest(bsi3.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(bsi4.fe, effect="twoways", type="ghm") # Statistically significant

# Updated Random Effects models for Stringency Index (Below)
bsi1.re <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_si_new_pdata, model="random", random.method="walhus")
summary(bsi1.re)
bsi2.re <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_si_new_pdata, model="random", random.method="walhus")
summary(bsi2.re)
bsi3.re <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_si_new_pdata, model="random", random.method="walhus")
summary(bsi3.re)
bsi4.re <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_si_new_pdata, model="random", random.method="walhus")
summary(bsi4.re)

#Hausmann Test to see if RE improve the model (Below)
#HO: Random Effects model consistent and efficient (preferred)
#H1: Fixed effects model consistent and preferred
phtest(bsi1.fe, bsi1.re) # RE is preferred, not statistically significant
phtest(bsi2.fe, bsi2.re) # RE is preferred, not statistically significant
phtest(bsi3.fe, bsi3.re) # RE is preferred, not statistically significant
phtest(bsi4.fe, bsi4.re) # FE is preferred, statistically significant


## Containment Health Index ####
# Updated Fixed Effects models for Containment Health Index (Above)
achi1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_chi_new_pdata, model="pooling", effect="twoways")
achi2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_chi_new_pdata, model="pooling", effect="twoways")
achi3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_chi_new_pdata, model="pooling", effect="twoways")
achi4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_chi_new_pdata, model="pooling", effect="twoways")

# Updated Random effects test for Containment Health Index (Above)
# H0: Zero variance in individual-specific errors
plmtest(achi1.fe, effect="twoways", type="ghm") # Statistically significant
plmtest(achi2.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(achi3.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(achi4.fe, effect="twoways", type="ghm") # Not statistically significant

# Updated Random Effects models for Containment Health Index (Above)
achi1.re <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_chi_new_pdata, model="random", random.method="walhus")
summary(achi1.re)
achi2.re <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_chi_new_pdata, model="random", random.method="walhus")
summary(achi2.re)
achi3.re <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_chi_new_pdata, model="random", random.method="walhus")
summary(achi3.re)
achi4.re <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=a_chi_new_pdata, model="random", random.method="walhus")
summary(achi4.re)

#Hausmann Test to see if RE improve the model (Above)
#HO: Random Effects model consistent and efficient (preferred)
#H1: Fixed effects model consistent and preferred
phtest(achi1.fe, achi1.re) # FE is preferred, statistically significant
phtest(achi2.fe, achi2.re) # RE is preferred, not statistically significant
phtest(achi3.fe, achi3.re) # RE is preferred, not statistically significant
phtest(achi4.fe, achi4.re) # FE is preferred, statistically significant

# Updated Fixed Effects models for Containment Health Index (Below)
bchi1.fe <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_chi_new_pdata, model="pooling", effect="twoways")
bchi2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_chi_new_pdata, model="pooling", effect="twoways")
bchi3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_chi_new_pdata, model="pooling", effect="twoways")
bchi4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 1) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_chi_new_pdata, model="pooling", effect="twoways")

# Updated Random effects test for Containment Health Index (Below)
# H0: Zero variance in individual-specific errors
plmtest(bchi1.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(bchi2.fe, effect="twoways", type="ghm") # Statistically significant
plmtest(bchi3.fe, effect="twoways", type="ghm") # Not statistically significant
plmtest(bchi4.fe, effect="twoways", type="ghm") # Statistically significant

# Updated Random Effects models for Containment Health Index (Below)
bchi1.re <- plm(casesgrowthfactor ~  lag(casesgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_chi_new_pdata, model="random", random.method="walhus")
summary(bchi1.re)
bchi2.re <- plm(casesnewweekly ~  lag(casesnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_chi_new_pdata, model="random", random.method="walhus")
summary(bchi2.re)
bchi3.re <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_chi_new_pdata, model="random", random.method="walhus")
summary(bchi3.re)
bchi4.re <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) + lag(GovernmentResponseIndex, 1) + lag(EconomicSupportIndex, 1) + lag(fiscalmeasures, 3) + lag(healthinvestment, 1) + agedpop + gdp_norm + popdensity, data=b_chi_new_pdata, model="random", random.method="walhus")
summary(bchi4.re)

#Hausmann Test to see if RE improve the model (Below)
#HO: Random Effects model consistent and efficient (preferred)
#H1: Fixed effects model consistent and preferred
phtest(bchi1.fe, bchi1.re) # RE is preferred, not statistically significant
phtest(bchi2.fe, bchi2.re) # RE is preferred, not statistically significant
phtest(bchi3.fe, bchi3.re) # RE is preferred, not statistically significant
phtest(bchi4.fe, bchi4.re) # FE is preferred, statistically significant

##### Plotting Coefficients ####
## Government Response Index (Above) ####
pdf("agri.pdf", height=8, width=8)
#tiff("gri.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
agri1.output <- list()
for(i in 1:5){
  agri1.fe <- plm(casesgrowthfactor ~ lag(casesgrowthfactor, 1) + 
                    lag(GovernmentResponseIndex, i) + 
                    lag(EconomicSupportIndex, i) + 
                    lag(fiscalmeasures, i) + 
                    lag(healthinvestment, i) + 
                    agedpop + gdp_norm + popdensity, 
                  data = a_gri_new_pdata, 
                  model = "pooling", 
                  effect = "twoways")
  agri1.output[[i]] <- agri1.fe
  #print(i)
  #print(summary(agri1.fe))
  lag <- i
  coefficient <- coef(summary(agri1.fe))[3,1]
  se <- coef(summary(agri1.fe))[3,2]
  ll <- coef(summary(agri1.fe))[3,1] - 1.96*coef(summary(agri1.fe))[3,2]
  ul <- coef(summary(agri1.fe))[3,1] + 1.96*coef(summary(agri1.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    agri1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    agri1.plot<- rbind(agri1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-0.2,0.2))
points(agri1.plot$lag, agri1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(agri1.plot$ll[i], agri1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-0.2,0.2,0.1), labels=seq(-0.2,0.2,0.1), las=2)
title(main="(a) Cases Growth Factor", ylab="Government Response Index", xlab="Lag Length in Weeks")

#New Weekly Cases
agri2.output <- list()
for(i in 1:5){
  agri2.fe <- plm(casesnewweekly ~  lag(casesnewweekly, 1) +
                    lag(GovernmentResponseIndex, i) +
                    lag(EconomicSupportIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data=a_gri_new_pdata, model="pooling", effect="twoways")
  agri2.output[[i]] <- agri2.fe
  #print(i)
  #print(summary(agri2.fe))
  lag <- i
  coefficient <- coef(summary(agri2.fe))[3,1]
  se <- coef(summary(agri2.fe))[3,2]
  ll <- coef(summary(agri2.fe))[3,1] - 1.96*coef(summary(agri2.fe))[3,2]
  ul <- coef(summary(agri2.fe))[3,1] + 1.96*coef(summary(agri2.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    agri2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    agri2.plot<- rbind(agri2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-60,90))
points(agri2.plot$lag, agri2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(agri2.plot$ll[i], agri2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-60,90,10), labels=seq(-60,90,10), las=2)
title(main="(b) New Weekly Cases", ylab="Government Response Index", xlab="Lag Length in Weeks")

#Deaths Growth Factor
agri3.output <- list()
for(i in 1:5){
  agri3.fe <- plm(deathsgrowthfactor ~  lag(deathsgrowthfactor, 1) +
                    lag(GovernmentResponseIndex, i) +
                    lag(EconomicSupportIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data=a_gri_new_pdata, model="pooling", effect="twoways")
  agri3.output[[i]] <- agri3.fe
  #print(i)
  #print(summary(agri3.fe))
  lag <- i
  coefficient <- coef(summary(agri3.fe))[3,1]
  se <- coef(summary(agri3.fe))[3,2]
  ll <- coef(summary(agri3.fe))[3,1] - 1.96*coef(summary(agri3.fe))[3,2]
  ul <- coef(summary(agri3.fe))[3,1] + 1.96*coef(summary(agri3.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    agri3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    agri3.plot<- rbind(agri3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(agri3.plot$lag, agri3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(agri3.plot$ll[i], agri3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.1,0.1,0.1), labels=seq(-0.1,0.1,0.1), las=2)
title(main="(c) Deaths Growth Factor", ylab="Government Response Index", xlab="Lag Length in Weeks")

#New Weekly Deaths
agri4.output <- list()
for(i in 1:5){
  agri4.fe <- plm(deathsnewweekly ~  lag(deathsnewweekly, 1) +
                    lag(GovernmentResponseIndex, i) +
                    lag(EconomicSupportIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data=a_gri_new_pdata, model="pooling", effect="twoways")
  agri4.output[[i]] <- agri4.fe
  #print(i)
  #print(summary(agri4.fe))
  lag <- i
  coefficient <- coef(summary(agri4.fe))[3,1]
  se <- coef(summary(agri4.fe))[3,2]
  ll <- coef(summary(agri4.fe))[3,1] - 1.96*coef(summary(agri4.fe))[3,2]
  ul <- coef(summary(agri4.fe))[3,1] + 1.96*coef(summary(agri4.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    agri4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    agri4.plot<- rbind(agri4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-2,2))
points(agri4.plot$lag, agri4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(agri4.plot$ll[i], agri4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-2, 2, 1), labels=seq(-2, 2, 1), las=2)
title(main="(d) New Weekly Deaths", ylab="Government Response Index", xlab="Lag Length in Weeks")
dev.off()


## Government Response Index (Below) ####
pdf("bgri.pdf", height=8, width=8)
#tiff("gri.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
bgri1.output <- list()
for(i in 1:5){
  bgri1.fe <- plm(casesgrowthfactor ~ lag(casesgrowthfactor, 1) + 
                    lag(GovernmentResponseIndex, i) + 
                    lag(EconomicSupportIndex, i) + 
                    lag(fiscalmeasures, i) + 
                    lag(healthinvestment, i) + 
                    agedpop + gdp_norm + popdensity, 
                  data = b_gri_new_pdata, 
                  model = "pooling", 
                  effect = "twoways")
  bgri1.output[[i]] <- bgri1.fe
  #print(i)
  #print(summary(bgri1.fe))
  lag <- i
  coefficient <- coef(summary(bgri1.fe))[3,1]
  se <- coef(summary(bgri1.fe))[3,2]
  ll <- coef(summary(bgri1.fe))[3,1] - 1.96*coef(summary(bgri1.fe))[3,2]
  ul <- coef(summary(bgri1.fe))[3,1] + 1.96*coef(summary(bgri1.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    bgri1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    bgri1.plot<- rbind(bgri1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-0.2,0.2))
points(bgri1.plot$lag, bgri1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(bgri1.plot$ll[i], bgri1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-0.2,0.2,0.1), labels=seq(-0.2,0.2,0.1), las=2)
title(main="(a) Cases Growth Factor", ylab="Government Response Index", xlab="Lag Length in Weeks")

#New Weekly Cases
bgri2.output <- list()
for(i in 1:5){
  bgri2.fe <- plm(casesnewweekly ~ lag(casesnewweekly, 1) +
                    lag(GovernmentResponseIndex, i) +
                    lag(EconomicSupportIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = b_gri_new_pdata, model = "pooling", effect = "twoways")
  bgri2.output[[i]] <- bgri2.fe
  #print(i)
  #print(summary(bgri2.fe))
  lag <- i
  coefficient <- coef(summary(bgri2.fe))[3,1]
  se <- coef(summary(bgri2.fe))[3,2]
  ll <- coef(summary(bgri2.fe))[3,1] - 1.96*coef(summary(bgri2.fe))[3,2]
  ul <- coef(summary(bgri2.fe))[3,1] + 1.96*coef(summary(bgri2.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    bgri2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    bgri2.plot<- rbind(bgri2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-60,90))
points(bgri2.plot$lag, bgri2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(bgri2.plot$ll[i], bgri2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-60,90,10), labels=seq(-60,90,10), las=2)
title(main="(b) New Weekly Cases", ylab="Government Response Index", xlab="Lag Length in Weeks")

#Deaths Growth Factor
bgri3.output <- list()
for(i in 1:5){
  bgri3.fe <- plm(deathsgrowthfactor ~ lag(deathsgrowthfactor, 1) +
                    lag(GovernmentResponseIndex, i) +
                    lag(EconomicSupportIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = b_gri_new_pdata, model = "pooling", effect = "twoways")
  bgri3.output[[i]] <- bgri3.fe
  #print(i)
  #print(summary(bgri3.fe))
  lag <- i
  coefficient <- coef(summary(bgri3.fe))[3,1]
  se <- coef(summary(bgri3.fe))[3,2]
  ll <- coef(summary(bgri3.fe))[3,1] - 1.96*coef(summary(bgri3.fe))[3,2]
  ul <- coef(summary(bgri3.fe))[3,1] + 1.96*coef(summary(bgri3.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    bgri3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    bgri3.plot<- rbind(bgri3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(bgri3.plot$lag, bgri3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(bgri3.plot$ll[i], bgri3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.1,0.1,0.1), labels=seq(-0.1,0.1,0.1), las=2)
title(main="(c) Deaths Growth Factor", ylab="Government Response Index", xlab="Lag Length in Weeks")

#New Weekly Deaths
bgri4.output <- list()
for(i in 1:5){
  bgri4.fe <- plm(deathsnewweekly ~ lag(deathsnewweekly, 1) +
                    lag(GovernmentResponseIndex, i) +
                    lag(EconomicSupportIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = b_gri_new_pdata, model = "pooling", effect = "twoways")
  bgri4.output[[i]] <- bgri4.fe
  #print(i)
  #print(summary(bgri4.fe))
  lag <- i
  coefficient <- coef(summary(bgri4.fe))[3,1]
  se <- coef(summary(bgri4.fe))[3,2]
  ll <- coef(summary(bgri4.fe))[3,1] - 1.96*coef(summary(bgri4.fe))[3,2]
  ul <- coef(summary(bgri4.fe))[3,1] + 1.96*coef(summary(bgri4.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    bgri4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    bgri4.plot<- rbind(bgri4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-2,2))
points(bgri4.plot$lag, bgri4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(bgri4.plot$ll[i], bgri4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-2, 2, 1), labels=seq(-2, 2, 1), las=2)
title(main="(d) New Weekly Deaths", ylab="Government Response Index", xlab="Lag Length in Weeks")
dev.off()


## Economic Support Index (Above) ####
pdf("aesi.pdf", height=8, width=8)
#tiff("esi.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
aesi1.output <- list()
for(i in 1:5){
  aesi1.fe <- plm(casesgrowthfactor ~ lag(casesgrowthfactor, 1) + 
                    lag(EconomicSupportIndex, i) + 
                    lag(EconomicSupportIndex, i) + 
                    lag(fiscalmeasures, i) + 
                    lag(healthinvestment, i) + 
                    agedpop + gdp_norm + popdensity, 
                  data = a_esi_new_pdata, 
                  model = "pooling", 
                  effect = "twoways")
  aesi1.output[[i]] <- aesi1.fe
  #print(i)
  #print(summary(aesi1.fe))
  lag <- i
  coefficient <- coef(summary(aesi1.fe))[3,1]
  se <- coef(summary(aesi1.fe))[3,2]
  ll <- coef(summary(aesi1.fe))[3,1] - 1.96*coef(summary(aesi1.fe))[3,2]
  ul <- coef(summary(aesi1.fe))[3,1] + 1.96*coef(summary(aesi1.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    aesi1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    aesi1.plot<- rbind(aesi1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-0.2,0.2))
points(aesi1.plot$lag, aesi1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(aesi1.plot$ll[i], aesi1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-0.2,0.2,0.1), labels=seq(-0.2,0.2,0.1), las=2)
title(main="(a) Cases Growth Factor", ylab="Economic Support Index", xlab="Lag Length in Weeks")

#New Weekly Cases
aesi2.output <- list()
for(i in 1:5){
  aesi2.fe <- plm(casesnewweekly ~ lag(casesnewweekly, 1) +
                    lag(EconomicSupportIndex, i) +
                    lag(EconomicSupportIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = a_esi_new_pdata, model = "pooling", effect = "twoways")
  aesi2.output[[i]] <- aesi2.fe
  #print(i)
  #print(summary(aesi2.fe))
  lag <- i
  coefficient <- coef(summary(aesi2.fe))[3,1]
  se <- coef(summary(aesi2.fe))[3,2]
  ll <- coef(summary(aesi2.fe))[3,1] - 1.96*coef(summary(aesi2.fe))[3,2]
  ul <- coef(summary(aesi2.fe))[3,1] + 1.96*coef(summary(aesi2.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    aesi2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    aesi2.plot<- rbind(aesi2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-60,90))
points(aesi2.plot$lag, aesi2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(aesi2.plot$ll[i], aesi2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-60,90,10), labels=seq(-60,90,10), las=2)
title(main="(b) New Weekly Cases", ylab="Economic Support Index", xlab="Lag Length in Weeks")

#Deaths Growth Factor
aesi3.output <- list()
for(i in 1:5){
  aesi3.fe <- plm(deathsgrowthfactor ~ lag(deathsgrowthfactor, 1) +
                    lag(EconomicSupportIndex, i) +
                    lag(EconomicSupportIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = a_esi_new_pdata, model = "pooling", effect = "twoways")
  aesi3.output[[i]] <- aesi3.fe
  #print(i)
  #print(summary(aesi3.fe))
  lag <- i
  coefficient <- coef(summary(aesi3.fe))[3,1]
  se <- coef(summary(aesi3.fe))[3,2]
  ll <- coef(summary(aesi3.fe))[3,1] - 1.96*coef(summary(aesi3.fe))[3,2]
  ul <- coef(summary(aesi3.fe))[3,1] + 1.96*coef(summary(aesi3.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    aesi3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    aesi3.plot<- rbind(aesi3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(aesi3.plot$lag, aesi3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(aesi3.plot$ll[i], aesi3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.1,0.1,0.1), labels=seq(-0.1,0.1,0.1), las=2)
title(main="(c) Deaths Growth Factor", ylab="Economic Support Index", xlab="Lag Length in Weeks")

#New Weekly Deaths
aesi4.output <- list()
for(i in 1:5){
  aesi4.fe <- plm(deathsnewweekly ~ lag(deathsnewweekly, 1) +
                    lag(EconomicSupportIndex, i) +
                    lag(EconomicSupportIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = a_esi_new_pdata, model = "pooling", effect = "twoways")
  aesi4.output[[i]] <- aesi4.fe
  #print(i)
  #print(summary(aesi4.fe))
  lag <- i
  coefficient <- coef(summary(aesi4.fe))[3,1]
  se <- coef(summary(aesi4.fe))[3,2]
  ll <- coef(summary(aesi4.fe))[3,1] - 1.96*coef(summary(aesi4.fe))[3,2]
  ul <- coef(summary(aesi4.fe))[3,1] + 1.96*coef(summary(aesi4.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    aesi4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    aesi4.plot<- rbind(aesi4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-2,2))
points(aesi4.plot$lag, aesi4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(aesi4.plot$ll[i], aesi4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-2, 2, 1), labels=seq(-2, 2, 1), las=2)
title(main="(d) New Weekly Deaths", ylab="Economic Support Index", xlab="Lag Length in Weeks")
dev.off()

## Economic Support Index (Below) ####
pdf("besi.pdf", height=8, width=8)
#tiff("esi.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
besi1.output <- list()
for(i in 1:5){
  besi1.fe <- plm(casesgrowthfactor ~ lag(casesgrowthfactor, 1) + 
                    lag(EconomicSupportIndex, i) + 
                    lag(EconomicSupportIndex, i) + 
                    lag(fiscalmeasures, i) + 
                    lag(healthinvestment, i) + 
                    agedpop + gdp_norm + popdensity, 
                  data = b_esi_new_pdata, 
                  model = "pooling", 
                  effect = "twoways")
  besi1.output[[i]] <- besi1.fe
  #print(i)
  #print(summary(besi1.fe))
  lag <- i
  coefficient <- coef(summary(besi1.fe))[3,1]
  se <- coef(summary(besi1.fe))[3,2]
  ll <- coef(summary(besi1.fe))[3,1] - 1.96*coef(summary(besi1.fe))[3,2]
  ul <- coef(summary(besi1.fe))[3,1] + 1.96*coef(summary(besi1.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    besi1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    besi1.plot<- rbind(besi1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-0.2,0.2))
points(besi1.plot$lag, besi1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(besi1.plot$ll[i], besi1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-0.2,0.2,0.1), labels=seq(-0.2,0.2,0.1), las=2)
title(main="(a) Cases Growth Factor", ylab="Economic Support Index", xlab="Lag Length in Weeks")

#New Weekly Cases
besi2.output <- list()
for(i in 1:5){
  besi2.fe <- plm(casesnewweekly ~ lag(casesnewweekly, 1) +
                    lag(EconomicSupportIndex, i) +
                    lag(EconomicSupportIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = b_esi_new_pdata, model = "pooling", effect = "twoways")
  besi2.output[[i]] <- besi2.fe
  #print(i)
  #print(summary(besi2.fe))
  lag <- i
  coefficient <- coef(summary(besi2.fe))[3,1]
  se <- coef(summary(besi2.fe))[3,2]
  ll <- coef(summary(besi2.fe))[3,1] - 1.96*coef(summary(besi2.fe))[3,2]
  ul <- coef(summary(besi2.fe))[3,1] + 1.96*coef(summary(besi2.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    besi2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    besi2.plot<- rbind(besi2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-60,90))
points(besi2.plot$lag, besi2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(besi2.plot$ll[i], besi2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-60,90,10), labels=seq(-60,90,10), las=2)
title(main="(b) New Weekly Cases", ylab="Economic Support Index", xlab="Lag Length in Weeks")

#Deaths Growth Factor
besi3.output <- list()
for(i in 1:5){
  besi3.fe <- plm(deathsgrowthfactor ~ lag(deathsgrowthfactor, 1) +
                    lag(EconomicSupportIndex, i) +
                    lag(EconomicSupportIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = b_esi_new_pdata, model = "pooling", effect = "twoways")
  besi3.output[[i]] <- besi3.fe
  #print(i)
  #print(summary(besi3.fe))
  lag <- i
  coefficient <- coef(summary(besi3.fe))[3,1]
  se <- coef(summary(besi3.fe))[3,2]
  ll <- coef(summary(besi3.fe))[3,1] - 1.96*coef(summary(besi3.fe))[3,2]
  ul <- coef(summary(besi3.fe))[3,1] + 1.96*coef(summary(besi3.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    besi3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    besi3.plot<- rbind(besi3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(besi3.plot$lag, besi3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(besi3.plot$ll[i], besi3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.1,0.1,0.1), labels=seq(-0.1,0.1,0.1), las=2)
title(main="(c) Deaths Growth Factor", ylab="Economic Support Index", xlab="Lag Length in Weeks")

#New Weekly Deaths
besi4.output <- list()
for(i in 1:5){
  besi4.fe <- plm(deathsnewweekly ~ lag(deathsnewweekly, 1) +
                    lag(EconomicSupportIndex, i) +
                    lag(EconomicSupportIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = b_esi_new_pdata, model = "pooling", effect = "twoways")
  besi4.output[[i]] <- besi4.fe
  #print(i)
  #print(summary(besi4.fe))
  lag <- i
  coefficient <- coef(summary(besi4.fe))[3,1]
  se <- coef(summary(besi4.fe))[3,2]
  ll <- coef(summary(besi4.fe))[3,1] - 1.96*coef(summary(besi4.fe))[3,2]
  ul <- coef(summary(besi4.fe))[3,1] + 1.96*coef(summary(besi4.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    besi4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    besi4.plot<- rbind(besi4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-2,2))
points(besi4.plot$lag, besi4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(besi4.plot$ll[i], besi4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-2, 2, 1), labels=seq(-2, 2, 1), las=2)
title(main="(d) New Weekly Deaths", ylab="Economic Support Index", xlab="Lag Length in Weeks")
dev.off()


## Stringency Index (Above) ####
pdf("asi.pdf", height=8, width=8)
#tiff("esi.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
asi1.output <- list()
for(i in 1:5){
  asi1.fe <- plm(casesgrowthfactor ~ lag(casesgrowthfactor, 1) + 
                   lag(ContainmentHealthIndex, i) + 
                   lag(ContainmentHealthIndex, i) + 
                   lag(fiscalmeasures, i) + 
                   lag(healthinvestment, i) + 
                   agedpop + gdp_norm + popdensity, 
                 data = a_si_new_pdata, 
                 model = "pooling", 
                 effect = "twoways")
  asi1.output[[i]] <- asi1.fe
  #print(i)
  #print(summary(asi1.fe))
  lag <- i
  coefficient <- coef(summary(asi1.fe))[3,1]
  se <- coef(summary(asi1.fe))[3,2]
  ll <- coef(summary(asi1.fe))[3,1] - 1.96*coef(summary(asi1.fe))[3,2]
  ul <- coef(summary(asi1.fe))[3,1] + 1.96*coef(summary(asi1.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    asi1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    asi1.plot<- rbind(asi1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-0.2,0.2))
points(asi1.plot$lag, asi1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(asi1.plot$ll[i], asi1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-0.2,0.2,0.1), labels=seq(-0.2,0.2,0.1), las=2)
title(main="(a) Cases Growth Factor", ylab="Containment Health Index", xlab="Lag Length in Weeks")

#New Weekly Cases
asi2.output <- list()
for(i in 1:5){
  asi2.fe <- plm(casesnewweekly ~ lag(casesnewweekly, 1) +
                   lag(ContainmentHealthIndex, i) +
                   lag(ContainmentHealthIndex, i) +
                   lag(fiscalmeasures, i) +
                   lag(healthinvestment, i) +
                   agedpop + gdp_norm + popdensity,
                 data = a_si_new_pdata, model = "pooling", effect = "twoways")
  asi2.output[[i]] <- asi2.fe
  #print(i)
  #print(summary(asi2.fe))
  lag <- i
  coefficient <- coef(summary(asi2.fe))[3,1]
  se <- coef(summary(asi2.fe))[3,2]
  ll <- coef(summary(asi2.fe))[3,1] - 1.96*coef(summary(asi2.fe))[3,2]
  ul <- coef(summary(asi2.fe))[3,1] + 1.96*coef(summary(asi2.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    asi2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    asi2.plot<- rbind(asi2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-60,90))
points(asi2.plot$lag, asi2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(asi2.plot$ll[i], asi2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-60,90,10), labels=seq(-60,90,10), las=2)
title(main="(b) New Weekly Cases", ylab="Containment Health Index", xlab="Lag Length in Weeks")

#Deaths Growth Factor
asi3.output <- list()
for(i in 1:5){
  asi3.fe <- plm(deathsgrowthfactor ~ lag(deathsgrowthfactor, 1) +
                   lag(ContainmentHealthIndex, i) +
                   lag(ContainmentHealthIndex, i) +
                   lag(fiscalmeasures, i) +
                   lag(healthinvestment, i) +
                   agedpop + gdp_norm + popdensity,
                 data = a_si_new_pdata, model = "pooling", effect = "twoways")
  asi3.output[[i]] <- asi3.fe
  #print(i)
  #print(summary(asi3.fe))
  lag <- i
  coefficient <- coef(summary(asi3.fe))[3,1]
  se <- coef(summary(asi3.fe))[3,2]
  ll <- coef(summary(asi3.fe))[3,1] - 1.96*coef(summary(asi3.fe))[3,2]
  ul <- coef(summary(asi3.fe))[3,1] + 1.96*coef(summary(asi3.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    asi3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    asi3.plot<- rbind(asi3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(asi3.plot$lag, asi3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(asi3.plot$ll[i], asi3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.1,0.1,0.1), labels=seq(-0.1,0.1,0.1), las=2)
title(main="(c) Deaths Growth Factor", ylab="Containment Health Index", xlab="Lag Length in Weeks")

#New Weekly Deaths
asi4.output <- list()
for(i in 1:5){
  asi4.fe <- plm(deathsnewweekly ~ lag(deathsnewweekly, 1) +
                   lag(ContainmentHealthIndex, i) +
                   lag(ContainmentHealthIndex, i) +
                   lag(fiscalmeasures, i) +
                   lag(healthinvestment, i) +
                   agedpop + gdp_norm + popdensity,
                 data = a_si_new_pdata, model = "pooling", effect = "twoways")
  asi4.output[[i]] <- asi4.fe
  #print(i)
  #print(summary(asi4.fe))
  lag <- i
  coefficient <- coef(summary(asi4.fe))[3,1]
  se <- coef(summary(asi4.fe))[3,2]
  ll <- coef(summary(asi4.fe))[3,1] - 1.96*coef(summary(asi4.fe))[3,2]
  ul <- coef(summary(asi4.fe))[3,1] + 1.96*coef(summary(asi4.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    asi4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    asi4.plot<- rbind(asi4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-2,2))
points(asi4.plot$lag, asi4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(asi4.plot$ll[i], asi4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-2, 2, 1), labels=seq(-2, 2, 1), las=2)
title(main="(d) New Weekly Deaths", ylab="Containment Health Index", xlab="Lag Length in Weeks")
dev.off()


## Stringency Index (Below) ####
pdf("bsi.pdf", height=8, width=8)
#tiff("esi.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
bsi1.output <- list()
for(i in 1:5){
  bsi1.fe <- plm(casesgrowthfactor ~ lag(casesgrowthfactor, 1) + 
                   lag(StringencyIndex, i) + 
                   lag(StringencyIndex, i) + 
                   lag(fiscalmeasures, i) + 
                   lag(healthinvestment, i) + 
                   agedpop + gdp_norm + popdensity, 
                 data = b_si_new_pdata, 
                 model = "pooling", 
                 effect = "twoways")
  bsi1.output[[i]] <- bsi1.fe
  #print(i)
  #print(summary(bsi1.fe))
  lag <- i
  coefficient <- coef(summary(bsi1.fe))[3,1]
  se <- coef(summary(bsi1.fe))[3,2]
  ll <- coef(summary(bsi1.fe))[3,1] - 1.96*coef(summary(bsi1.fe))[3,2]
  ul <- coef(summary(bsi1.fe))[3,1] + 1.96*coef(summary(bsi1.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    bsi1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    bsi1.plot<- rbind(bsi1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-0.2,0.2))
points(bsi1.plot$lag, bsi1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(bsi1.plot$ll[i], bsi1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-0.2,0.2,0.1), labels=seq(-0.2,0.2,0.1), las=2)
title(main="(a) Cases Growth Factor", ylab="Stringency Index", xlab="Lag Length in Weeks")

#New Weekly Cases
bsi2.output <- list()
for(i in 1:5){
  bsi2.fe <- plm(casesnewweekly ~ lag(casesnewweekly, 1) +
                   lag(StringencyIndex, i) +
                   lag(StringencyIndex, i) +
                   lag(fiscalmeasures, i) +
                   lag(healthinvestment, i) +
                   agedpop + gdp_norm + popdensity,
                 data = b_si_new_pdata, model = "pooling", effect = "twoways")
  bsi2.output[[i]] <- bsi2.fe
  #print(i)
  #print(summary(bsi2.fe))
  lag <- i
  coefficient <- coef(summary(bsi2.fe))[3,1]
  se <- coef(summary(bsi2.fe))[3,2]
  ll <- coef(summary(bsi2.fe))[3,1] - 1.96*coef(summary(bsi2.fe))[3,2]
  ul <- coef(summary(bsi2.fe))[3,1] + 1.96*coef(summary(bsi2.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    bsi2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    bsi2.plot<- rbind(bsi2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-60,90))
points(bsi2.plot$lag, bsi2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(bsi2.plot$ll[i], bsi2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-60,90,10), labels=seq(-60,90,10), las=2)
title(main="(b) New Weekly Cases", ylab="Stringency Index", xlab="Lag Length in Weeks")

#Deaths Growth Factor
bsi3.output <- list()
for(i in 1:5){
  bsi3.fe <- plm(deathsgrowthfactor ~ lag(deathsgrowthfactor, 1) +
                   lag(StringencyIndex, i) +
                   lag(StringencyIndex, i) +
                   lag(fiscalmeasures, i) +
                   lag(healthinvestment, i) +
                   agedpop + gdp_norm + popdensity,
                 data = b_si_new_pdata, model = "pooling", effect = "twoways")
  bsi3.output[[i]] <- bsi3.fe
  #print(i)
  #print(summary(bsi3.fe))
  lag <- i
  coefficient <- coef(summary(bsi3.fe))[3,1]
  se <- coef(summary(bsi3.fe))[3,2]
  ll <- coef(summary(bsi3.fe))[3,1] - 1.96*coef(summary(bsi3.fe))[3,2]
  ul <- coef(summary(bsi3.fe))[3,1] + 1.96*coef(summary(bsi3.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    bsi3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    bsi3.plot<- rbind(bsi3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(bsi3.plot$lag, bsi3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(bsi3.plot$ll[i], bsi3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.1,0.1,0.1), labels=seq(-0.1,0.1,0.1), las=2)
title(main="(c) Deaths Growth Factor", ylab="Stringency Index", xlab="Lag Length in Weeks")

#New Weekly Deaths
bsi4.output <- list()
for(i in 1:5){
  bsi4.fe <- plm(deathsnewweekly ~ lag(deathsnewweekly, 1) +
                   lag(StringencyIndex, i) +
                   lag(StringencyIndex, i) +
                   lag(fiscalmeasures, i) +
                   lag(healthinvestment, i) +
                   agedpop + gdp_norm + popdensity,
                 data = b_si_new_pdata, model = "pooling", effect = "twoways")
  bsi4.output[[i]] <- bsi4.fe
  #print(i)
  #print(summary(bsi4.fe))
  lag <- i
  coefficient <- coef(summary(bsi4.fe))[3,1]
  se <- coef(summary(bsi4.fe))[3,2]
  ll <- coef(summary(bsi4.fe))[3,1] - 1.96*coef(summary(bsi4.fe))[3,2]
  ul <- coef(summary(bsi4.fe))[3,1] + 1.96*coef(summary(bsi4.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    bsi4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    bsi4.plot<- rbind(bsi4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-2,2))
points(bsi4.plot$lag, bsi4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(bsi4.plot$ll[i], bsi4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-2, 2, 1), labels=seq(-2, 2, 1), las=2)
title(main="(d) New Weekly Deaths", ylab="Stringency Index", xlab="Lag Length in Weeks")
dev.off()


## Containment Health Index (Above) ####
pdf("achi.pdf", height=8, width=8)
#tiff("esi.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
achi1.output <- list()
for(i in 1:5){
  achi1.fe <- plm(casesgrowthfactor ~ lag(casesgrowthfactor, 1) + 
                    lag(ContainmentHealthIndex, i) + 
                    lag(ContainmentHealthIndex, i) + 
                    lag(fiscalmeasures, i) + 
                    lag(healthinvestment, i) + 
                    agedpop + gdp_norm + popdensity, 
                  data = a_chi_new_pdata, 
                  model = "pooling", 
                  effect = "twoways")
  achi1.output[[i]] <- achi1.fe
  #print(i)
  #print(summary(achi1.fe))
  lag <- i
  coefficient <- coef(summary(achi1.fe))[3,1]
  se <- coef(summary(achi1.fe))[3,2]
  ll <- coef(summary(achi1.fe))[3,1] - 1.96*coef(summary(achi1.fe))[3,2]
  ul <- coef(summary(achi1.fe))[3,1] + 1.96*coef(summary(achi1.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    achi1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    achi1.plot<- rbind(achi1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-0.2,0.2))
points(achi1.plot$lag, achi1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(achi1.plot$ll[i], achi1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-0.2,0.2,0.1), labels=seq(-0.2,0.2,0.1), las=2)
title(main="(a) Cases Growth Factor", ylab="Containment Health Index", xlab="Lag Length in Weeks")

#New Weekly Cases
achi2.output <- list()
for(i in 1:5){
  achi2.fe <- plm(casesnewweekly ~ lag(casesnewweekly, 1) +
                    lag(ContainmentHealthIndex, i) +
                    lag(ContainmentHealthIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = a_chi_new_pdata, model = "pooling", effect = "twoways")
  achi2.output[[i]] <- achi2.fe
  #print(i)
  #print(summary(achi2.fe))
  lag <- i
  coefficient <- coef(summary(achi2.fe))[3,1]
  se <- coef(summary(achi2.fe))[3,2]
  ll <- coef(summary(achi2.fe))[3,1] - 1.96*coef(summary(achi2.fe))[3,2]
  ul <- coef(summary(achi2.fe))[3,1] + 1.96*coef(summary(achi2.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    achi2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    achi2.plot<- rbind(achi2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-60,90))
points(achi2.plot$lag, achi2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(achi2.plot$ll[i], achi2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-60,90,10), labels=seq(-60,90,10), las=2)
title(main="(b) New Weekly Cases", ylab="Containment Health Index", xlab="Lag Length in Weeks")

#Deaths Growth Factor
achi3.output <- list()
for(i in 1:5){
  achi3.fe <- plm(deathsgrowthfactor ~ lag(deathsgrowthfactor, 1) +
                    lag(ContainmentHealthIndex, i) +
                    lag(ContainmentHealthIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = a_chi_new_pdata, model = "pooling", effect = "twoways")
  achi3.output[[i]] <- achi3.fe
  #print(i)
  #print(summary(achi3.fe))
  lag <- i
  coefficient <- coef(summary(achi3.fe))[3,1]
  se <- coef(summary(achi3.fe))[3,2]
  ll <- coef(summary(achi3.fe))[3,1] - 1.96*coef(summary(achi3.fe))[3,2]
  ul <- coef(summary(achi3.fe))[3,1] + 1.96*coef(summary(achi3.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    achi3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    achi3.plot<- rbind(achi3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(achi3.plot$lag, achi3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(achi3.plot$ll[i], achi3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.1,0.1,0.1), labels=seq(-0.1,0.1,0.1), las=2)
title(main="(c) Deaths Growth Factor", ylab="Containment Health Index", xlab="Lag Length in Weeks")

#New Weekly Deaths
achi4.output <- list()
for(i in 1:5){
  achi4.fe <- plm(deathsnewweekly ~ lag(deathsnewweekly, 1) +
                    lag(ContainmentHealthIndex, i) +
                    lag(ContainmentHealthIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = a_chi_new_pdata, model = "pooling", effect = "twoways")
  achi4.output[[i]] <- achi4.fe
  #print(i)
  #print(summary(achi4.fe))
  lag <- i
  coefficient <- coef(summary(achi4.fe))[3,1]
  se <- coef(summary(achi4.fe))[3,2]
  ll <- coef(summary(achi4.fe))[3,1] - 1.96*coef(summary(achi4.fe))[3,2]
  ul <- coef(summary(achi4.fe))[3,1] + 1.96*coef(summary(achi4.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    achi4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    achi4.plot<- rbind(achi4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-2,2))
points(achi4.plot$lag, achi4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(achi4.plot$ll[i], achi4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-2, 2, 1), labels=seq(-2, 2, 1), las=2)
title(main="(d) New Weekly Deaths", ylab="Containment Health Index", xlab="Lag Length in Weeks")
dev.off()


## Containment Health Index (Below) ####
pdf("bchi.pdf", height=8, width=8)
#tiff("esi.tiff", height=8, width=8, units="in", res=600)
#Cases Growth Factor
bchi1.output <- list()
for(i in 1:5){
  bchi1.fe <- plm(casesgrowthfactor ~ lag(casesgrowthfactor, 1) + 
                    lag(ContainmentHealthIndex, i) + 
                    lag(ContainmentHealthIndex, i) + 
                    lag(fiscalmeasures, i) + 
                    lag(healthinvestment, i) + 
                    agedpop + gdp_norm + popdensity, 
                  data = b_chi_new_pdata, 
                  model = "pooling", 
                  effect = "twoways")
  bchi1.output[[i]] <- bchi1.fe
  #print(i)
  #print(summary(bchi1.fe))
  lag <- i
  coefficient <- coef(summary(bchi1.fe))[3,1]
  se <- coef(summary(bchi1.fe))[3,2]
  ll <- coef(summary(bchi1.fe))[3,1] - 1.96*coef(summary(bchi1.fe))[3,2]
  ul <- coef(summary(bchi1.fe))[3,1] + 1.96*coef(summary(bchi1.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    bchi1.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    bchi1.plot<- rbind(bchi1.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

plot.new()
par(fig=c(0,0.5,0.5,1), new=TRUE)
plot.window(xlim=c(1,5),ylim=c(-0.2,0.2))
points(bchi1.plot$lag, bchi1.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(bchi1.plot$ll[i], bchi1.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-0.2,0.2,0.1), labels=seq(-0.2,0.2,0.1), las=2)
title(main="(a) Cases Growth Factor", ylab="Containment Health Index", xlab="Lag Length in Weeks")

#New Weekly Cases
bchi2.output <- list()
for(i in 1:5){
  bchi2.fe <- plm(casesnewweekly ~ lag(casesnewweekly, 1) +
                    lag(ContainmentHealthIndex, i) +
                    lag(ContainmentHealthIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = b_chi_new_pdata, model = "pooling", effect = "twoways")
  bchi2.output[[i]] <- bchi2.fe
  #print(i)
  #print(summary(bchi2.fe))
  lag <- i
  coefficient <- coef(summary(bchi2.fe))[3,1]
  se <- coef(summary(bchi2.fe))[3,2]
  ll <- coef(summary(bchi2.fe))[3,1] - 1.96*coef(summary(bchi2.fe))[3,2]
  ul <- coef(summary(bchi2.fe))[3,1] + 1.96*coef(summary(bchi2.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    bchi2.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    bchi2.plot<- rbind(bchi2.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0.5,1), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-60,90))
points(bchi2.plot$lag, bchi2.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(bchi2.plot$ll[i], bchi2.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-60,90,10), labels=seq(-60,90,10), las=2)
title(main="(b) New Weekly Cases", ylab="Containment Health Index", xlab="Lag Length in Weeks")

#Deaths Growth Factor
bchi3.output <- list()
for(i in 1:5){
  bchi3.fe <- plm(deathsgrowthfactor ~ lag(deathsgrowthfactor, 1) +
                    lag(ContainmentHealthIndex, i) +
                    lag(ContainmentHealthIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = b_chi_new_pdata, model = "pooling", effect = "twoways")
  bchi3.output[[i]] <- bchi3.fe
  #print(i)
  #print(summary(bchi3.fe))
  lag <- i
  coefficient <- coef(summary(bchi3.fe))[3,1]
  se <- coef(summary(bchi3.fe))[3,2]
  ll <- coef(summary(bchi3.fe))[3,1] - 1.96*coef(summary(bchi3.fe))[3,2]
  ul <- coef(summary(bchi3.fe))[3,1] + 1.96*coef(summary(bchi3.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    bchi3.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    bchi3.plot<- rbind(bchi3.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0,0.5,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-0.1,0.1))
points(bchi3.plot$lag, bchi3.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(bchi3.plot$ll[i], bchi3.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(1,5,1), labels=seq(1,5,1))
axis(2, at=seq(-0.1,0.1,0.1), labels=seq(-0.1,0.1,0.1), las=2)
title(main="(c) Deaths Growth Factor", ylab="Containment Health Index", xlab="Lag Length in Weeks")

#New Weekly Deaths
bchi4.output <- list()
for(i in 1:5){
  bchi4.fe <- plm(deathsnewweekly ~ lag(deathsnewweekly, 1) +
                    lag(ContainmentHealthIndex, i) +
                    lag(ContainmentHealthIndex, i) +
                    lag(fiscalmeasures, i) +
                    lag(healthinvestment, i) +
                    agedpop + gdp_norm + popdensity,
                  data = b_chi_new_pdata, model = "pooling", effect = "twoways")
  bchi4.output[[i]] <- bchi4.fe
  #print(i)
  #print(summary(bchi4.fe))
  lag <- i
  coefficient <- coef(summary(bchi4.fe))[3,1]
  se <- coef(summary(bchi4.fe))[3,2]
  ll <- coef(summary(bchi4.fe))[3,1] - 1.96*coef(summary(bchi4.fe))[3,2]
  ul <- coef(summary(bchi4.fe))[3,1] + 1.96*coef(summary(bchi4.fe))[3,2]
  working.data <- data.frame(lag, coefficient, se, ll, ul)
  if(i ==1){
    bchi4.plot <- data.frame(lag, coefficient, se, ll, ul)
  }else{
    bchi4.plot<- rbind(bchi4.plot, data.frame(lag, coefficient, se, ll, ul))
  }
}

par(fig=c(0.5,1,0,0.5), new=TRUE)
plot.new()
plot.window(xlim=c(1,5),ylim=c(-2,2))
points(bchi4.plot$lag, bchi4.plot$coefficient, pch=20)
for(i in 1:5){
  lines(c(i,i), c(bchi4.plot$ll[i], bchi4.plot$ul[i]))
}
abline(h=0)
axis(1, at=seq(0,5,1), labels=seq(0,5,1))
axis(2, at=seq(-2, 2, 1), labels=seq(-2, 2, 1), las=2)
title(main="(d) New Weekly Deaths", ylab="Containment Health Index", xlab="Lag Length in Weeks")
dev.off()