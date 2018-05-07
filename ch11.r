library(foreign);library(dynlm)

########## c1
hseinv <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/hseinv.dta")
tsdata <- ts(hseinv, start = 1947)

cor(hseinv$linvpc[-1], hseinv$linvpc_1[-1])
cor(hseinv$lprice[-1], hseinv$lprice_1[-1])

detrend.invpc <- dynlm(linvpc ~ trend(tsdata), data = tsdata)
linvpc.dt <- resid(detrend.invpc)

detrend.price <- dynlm(lprice ~ trend(tsdata), data = tsdata)
lprice.dt <- resid(detrend.price)


acf(linvpc.dt, plot = FALSE)
acf(lprice.dt, plot = FALSE)

acf(hseinv$linvpc, 1, plot = FALSE)
acf(hseinv$lprice, 1, plot = FALSE)

modelc1 <- dynlm(linvpc ~ d(lprice)+trend(tsdata), data = tsdata)
summary(modelc1)

modelc1.2 <- dynlm(linvpc.dt ~ d(lprice), data = tsdata)
summary(modelc1.2)

modelc1.3 <- dynlm(d(linvpc) ~ d(lprice)+trend(tsdata), data = tsdata)
summary(modelc1.3)


############# c2
earns <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/earns.dta")

modelc2 <- dynlm(ghrwage ~ goutphr+goutph_1, data=earns)
summary(modelc2)

modelc2.2 <- dynlm(ghrwage ~ goutphr+I(goutph_1-goutphr), data=earns)
summary(modelc2.2)

modelc2.3 <- dynlm(ghrwage ~ goutphr+goutph_1+goutph_2, data=earns)
summary(modelc2.3)

############ c3
nyse <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/nyse.dta")
tsdata <- ts(nyse, start = 1990)

modelc3 <- dynlm(return ~ L(return)+I(L(return)^2), data = tsdata)
summary(modelc3)
modelc3.1 <- dynlm(return ~ return_1, data = nyse)
summary(modelc3.1)


modelc3.3 <- dynlm(return ~ L(return)+L(return):L(return, 2), data = tsdata)
summary(modelc3.3)

############# c4
phillips <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/phillips.dta")
tsdata <- ts(phillips, start = 1948)

modelc4.1 <- dynlm(d(inf) ~ d(unem), data=tsdata)
summary(modelc4.1)


################ c5 
library(car);library(lmtest);library(sandwich)
nyse <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/fertil3.dta")
tsdata <- ts(nyse, start = 1990)

modelc5.1 <- dynlm(d(gfr) ~ d(pe)+L(d(pe), 1)+L(d(pe), 2), data = tsdata)
summary(modelc5.1)
modelc5.2 <- dynlm(d(gfr) ~ d(pe)+L(d(pe), 1)+L(d(pe), 2)+trend(tsdata), data = tsdata)
summary(modelc5.2)

modelc5.3 <- dynlm(d(gfr) ~ d(pe)+L(d(pe), 1)+L(d(pe), 2) + ww2+pill, data = tsdata)
summary(modelc5.3)

linearHypothesis(modelc5.3, c("ww2", "pill"))

modelc5.4 <- dynlm(d(gfr) ~ d(pe)+I(L(d(pe), 1)-d(pe)) + I(L(d(pe), 2)-d(pe))+ 
                     ww2+pill, data = tsdata)
summary(modelc5.4)

modelc5.5 <- dynlm(d(gfr) ~ d(pe)+I(L(d(pe), 1)-d(pe)) + I(L(d(pe), 2)-d(pe))+ 
                     trend(tsdata)+ww2+pill, data = tsdata)
summary(modelc5.5)

########### c6
inven <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/inven.dta")

tsdata <- ts(inven, start = 1959)

modelc6.1 <- dynlm(d(inven) ~ d(gdp), data = tsdata)
summary(modelc6.1)

modelc6.2 <- dynlm(d(inven) ~ d(gdp)+r3, data = tsdata)
summary(modelc6.2)

modelc6.3 <- dynlm(d(inven) ~ d(gdp)+d(r3), data = tsdata)
summary(modelc6.3)

############# c7
consump <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/consump.dta")
tsdata <- ts(consump, start = 1959)

modelc7.1 <- dynlm(d(log(c)) ~ L(d(log(c))), data = tsdata)
summary(modelc7.1)

modelc7.2 <- dynlm(d(log(c)) ~ L(d(log(c))) + L(gy)+L(i3), data = tsdata)
##linearHypothesis(modelc7.2, c("(L(gy))", "(L(i3))"))

pf(((0.288-0.1985)/2)/((1-0.288)/31), 2, 31, lower.tail = FALSE)

############# c8
setwd('e:/wooldridge/URfIE')
?arima
phillips2003 <- read.dta('phillips.dta')
tsdata <- ts(phillips2003, start = 1948)

ar(phillips2003$unem, FALSE, 1)
unem.ar <- arima(phillips2003$unem, c(1,0,0))
predict(unem.ar, 1)

modelc8.1 <- dynlm(unem ~ L(unem), data = tsdata)
summary(modelc8.1)
1.48968+0.74238*6.0

modelc8.2 <- dynlm(unem ~ L(unem) +L(inf), data = tsdata)
summary(modelc8.2)
unem2004.hat <-1.29642+0.64948*6.0+0.18301*2.3
unem2004.hat-1.96*0.8434
unem2004.hat+1.96*0.8434

modelc8 <- dynlm(unem ~ I(L(unem)-6) +I(L(inf)-2.3), data = tsdata)
summary(modelc8)

unem.sigmahat <- (0.13638^2+0.8434^2)^.5

unem2004.hat-1.96*unem.sigmahat
unem2004.hat+1.96*unem.sigmahat

############ c9
traffic2 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/traffic2.dta")
tsdata <- ts(traffic2, start = c(1981,1), frequency = 12)

acf(traffic2$prcfat, plot = FALSE)
acf(traffic2$unem, plot = FALSE)

modelc11 <- dynlm(d(prcfat) ~ wkends+d(unem)+spdlaw+beltlaw+trend(tsdata)+season(tsdata), data=tsdata)
summary(modelc11)

############# c10 
minwage <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/minwage.dta")
tsdata <- ts(minwage, start = c(1958,1), frequency = 12)
acf(minwage$gwage232, plot = FALSE, na.action = na.pass)

modelc10.1 <- dynlm(gwage232 ~ L(gwage232)+gmwage+gcpi, data = tsdata)
summary(modelc10.1)

modelc10.2 <- dynlm(gwage232 ~ L(gwage232)+gmwage+gcpi +L(gemp232), data = tsdata)
summary(modelc10.2)

modelc10.3 <- dynlm(gwage232 ~ gmwage+gcpi, data = tsdata)
summary(modelc10.3)

modelc10.4 <- dynlm(gwage232 ~ L(gwage232)+L(gemp232), data = tsdata)
summary(modelc10.4)

modelc10.5 <- dynlm(gmwage ~ L(gwage232)+L(gemp232), data = tsdata)
summary(modelc10.5)
