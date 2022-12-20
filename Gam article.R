library(mgcv)
library(MASS)
library(stringr)
library(gamm4)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(cowplot)
library(kableExtra)
library(docxtools)
library(knitr)
library(tibble)
library(dplyr)
library(gratia)
library(latex2exp)
library(lmtest)
library(car)


# Annecy only

p<-read.table(file.choose(),
              sep=';',
              dec=',',
              header=T)

mag1 <- gam(Density~s(CIED, bs = "tp", k = 4),
           data=p,family=gaussian(link="identity"),
           method="REML", select=TRUE)
summary(mag1)
(AIC(mag1))+(2*1*2)/(8-1-1)  #  AICc=AIC+2k(k+1)/n-k-1
draw(mag1, residuals=TRUE)
gam.check(mag1)
appraise(mag1)
shapiro.test(mag1[["residuals"]])
bptest(mag1)
par(mfrow=c(1,2))
acf(residuals(mag1))
pacf(residuals(mag1))
durbinWatsonTest(mag1)

mag2 <- gam(Density~s(CILD, bs = "tp", k = 4),
           data=p,family=gaussian(link="identity"),
           method="REML", select=TRUE)
summary(mag2)
(AIC(mag2))+(2*1*2)/(10-1-1)  #  AICc=AIC+2k(k+1)/n-k-1
draw(mag2, residuals=TRUE)
gam.check(mag2)
appraise(mag2)
shapiro.test(mag2[["residuals"]])
bptest(mag2)
par(mfrow=c(1,2))
acf(residuals(mag2))
pacf(residuals(mag2))
durbinWatsonTest(mag2)

# Bourget only

d<-read.table(file.choose(),
              sep=';',
              dec=',',
              header=T)

mag3 <- gam(Density~s(CILD, bs = "tp"),
            data=d,family=gaussian(link="identity"),
            method="REML", select=TRUE)
summary(mag3)
(AIC(mag3))+(2*1*2)/(11-1-1)  #  AICc=AIC+2k(k+1)/n-k-1
draw(mag3, residuals=TRUE)
gam.check(mag3)
appraise(mag3)
shapiro.test(mag3[["residuals"]])
bptest(mag3)
par(mfrow=c(1,2))
acf(residuals(mag3))
pacf(residuals(mag3))
durbinWatsonTest(mag3)

mag4 <- gam(Density~s(CILD, bs = "tp", k = 5),
            data=d,family=gaussian(link="identity"),
            method="REML", select=TRUE)
summary(mag4)
(AIC(mag4))+(2*1*2)/(10-1-1)  #  AICc=AIC+2k(k+1)/n-k-1
draw(mag4, residuals=TRUE)
gam.check(mag4)
appraise(mag4)
shapiro.test(mag4[["residuals"]])
bptest(mag4)
par(mfrow=c(1,2))
acf(residuals(mag4))
pacf(residuals(mag4))
durbinWatsonTest(mag4)

# Two Lakes

b<-read.table(file.choose(),
              sep=';',
              dec=',',
              header=T)

b$Lake<-as.factor(b$Lake)

#GS
mag5 <- gam(Density ~ s(CIED) + te(CIED, by = Lake, bs = c ("fs")),
            data=b, method="REML", select=TRUE)
summary(mag5)
(AIC(mag5))+(2*2*3)/(18-2-1)  #  AICc=AIC+2k(k+1)/n-k-1
draw(mag5,residuals=TRUE)
gam.check(mag5)
appraise(mag5)
shapiro.test(mag5[["residuals"]])
bptest(mag5)
par(mfrow=c(1,2))
acf(residuals(mag5))
pacf(residuals(mag5))
durbinWatsonTest(mag5)

#G
mag6 <- gam(Density ~ s(CIED) + s(Lake, bs = c ("fs")),
            data=b, method="REML", select=TRUE)
summary(mag6)
(AIC(mag6))+(2*2*3)/(18-2-1)  #  AICc=AIC+2k(k+1)/n-k-1
draw(mag6,residuals=TRUE)
gam.check(mag6)
appraise(mag6)

#GI
mag7 <- gam(Density ~ s(CIED, k=6) + s(CIED, by = Lake, bs = c ("tp","fs"), k=6) + s(Lake, bs = c ("re"), k=6),
            data=b, method="REML", select=TRUE)
summary(mag7)
(AIC(mag7))+(2*3*4)/(18-3-1)  #  AICc=AIC+2k(k+1)/n-k-1
draw(mag7,residuals=TRUE)
gam.check(mag7)
appraise(mag7)

#I
mag8 <- gam(Density ~ s(CIED, by = Lake, bs = c ("tp","fs"), k =6) + te(Lake, bs = c ("re"), k=6),
            data=b, method="REML", select=TRUE)
summary(mag8)
(AIC(mag8))+(2*2*3)/(18-2-1)  #  AICc=AIC+2k(k+1)/n-k-1
draw(mag8,residuals=TRUE)
gam.check(mag8)
appraise(mag8)

#E
mag9 <- gam(Density ~ s(CIED),
            data=b, method="REML", select=TRUE)
summary(mag9)
(AIC(mag9))+(2*1*2)/(18-1-1)  #  AICc=AIC+2k(k+1)/n-k-1
draw(mag9,residuals=TRUE)
gam.check(mag9)
appraise(mag9)

