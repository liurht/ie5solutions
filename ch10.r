setwd('e:/wooldridge/datasets')
library(foreign)

######## eg 10.4
load('fertil3.RData')
fertil3 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/fertil3.dta")
modeleg104 <- lm(gfr ~ pe+ww2+pill, data = data)
summary(modeleg104)

#### with pe lag variable
modeleg104lag <- lm(gfr ~ pe+pe_1+pe_2+ww2+pill, data = data)
summary(modeleg104lag)

#### t test for lrp
modeleg104lrp <- lm(gfr ~ pe+I(pe_1-pe)+I(pe_2-pe)+ww2+pill, data = fertil3)
summary(modeleg104lrp)


###################### computer exercise
###### c1
library(foreign)
intdef <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/intdef.dta")

post79 <- ifelse(intdef$year>1979, 1, 0)
modelc1 <- lm(i3 ~ inf+def+post79, data = intdef)
summary(modelc1)

###### c2
library(dynlm)
library(car)

barium <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/barium.dta")
# Define monthly time series beginning in Feb. 
tsdata <- ts(barium, start = c(1978, 2), frequency = 12)

### regression with trend
modelc2t <- dynlm(lchnimp ~ lchempi+lgas+lrtwex+befile6+affile6+afdec6 + trend(tsdata), data = tsdata)
summary(modelc2t)

### joint significance of other independent variables
linearHypothesis(modelc2t, c("(Intercept)", "lchempi","lgas","lrtwex","befile6",
          "affile6", "afdec6"))

### add monthly dummy variables
modelc2ts <- dynlm(lchnimp ~ lchempi+lgas+lrtwex+befile6+affile6+afdec6 + trend(tsdata) + season(tsdata), data = tsdata)
summary(modelc2ts)

###### c3
prminwge <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/prminwge.dta")

tsdata <- ts(prminwge, start = 1950)
modelc3 <- dynlm(lprepop ~ lmincov+lusgnp+lprgnp +trend(tsdata), tsdata)
summary(modelc3)

###### c4
## see eg 10.4

###### c5
ezanders <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/ezanders.dta')

tsdata <- ts(ezanders, start = c(1980,1), frequency = 12)

###### examine the time trend and seasonality of luclms
modelc5ts <- dynlm(luclms ~ trend(tsdata)+season(tsdata), data=tsdata)
summary(modelc5ts)

###### event study of ez establishment
modelc5event <- dynlm(luclms ~ ez+trend(tsdata)+season(tsdata), data=tsdata)
summary(modelc5event)

#### exact effect
100*(exp(-0.508027)-1)

####### c6
fertil3<-read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/fertil3.dta")

# Define Yearly time series beginning in 1913
tsdata <- ts(fertil3, start=1913)

#### detrending
modelc6gfrres <- dynlm(gfr ~ trend(tsdata)+I(trend(tsdata)^2), data=tsdata)
gfddot <- resid(modelc6gfrres) 

modelc6 <- dynlm(gfddot ~ pe+ww2+pill+trend(tsdata)+I(trend(tsdata)^2), data=tsdata)
summary(modelc6)

modelc6t3 <- dynlm(gfr ~ pe+ww2+pill+trend(tsdata)+I(trend(tsdata)^2)+I(trend(tsdata)^3), data=tsdata)
summary(modelc6t3)


########## c7
consump <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/consump.dta")

tsdata <- ts(consump, start = 1959)

modelc7 <- dynlm(gc ~ gy, tsdata)
summary(modelc7)

modelc7lag <- dynlm(gc ~ gy+gy_1, tsdata)
summary(modelc7lag)

modelc7int <- dynlm(gc ~ gy+r3, tsdata)
summary(modelc7int)


############ c8
fertil3<-read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/fertil3.dta")

# Define Yearly time series beginning in 1913
tsdata <- ts(fertil3, start=1913)

##(10.19) with more lags
modelc10lag <- dynlm(gfr ~ pe+L(pe)+L(pe,2)+L(pe,3)+L(pe,4)+ww2+pill, data=tsdata)
summary(modelc10lag)

### joint significance of lags
linearHypothesis(modelc10lag, matchCoefs(modelc10lag, 'pe')) ## pe and its all lags

pf(((0.5368-.499)/2)/((1-0.5368)/60), 2,60, lower.tail = FALSE) ## lag3 lag4

#### lrp 
modelc10lrp <- dynlm(gfr ~ pe+I(L(pe)-pe)+I(L(pe,2)-pe)+I(L(pe,3)-pe)+I(L(pe,4)-pe)+ww2+pill, data=tsdata)
summary(modelc10lrp)

####pdl model
modelc10pdl <- dynlm(gfr ~ I(pe+L(pe)+L(pe,2)+L(pe,3)+L(pe,4))+
                       I(L(pe)+2*L(pe,2)+3*L(pe,3)+4*L(pe,4))+
                       I(4*L(pe,2)+9*L(pe,3)+16*L(pe,4))+
                       ww2+pill, data=tsdata)
summary(modelc10pdl)

delta0=0.07546
delta1=0.07546-0.05940+0.01175
delta2=0.07546-0.05940*2+0.01175*4
delta3=0.07546-0.05940*3+0.01175*9
delta4=0.07546-0.05940*4+0.01175*16

lrp.pdl = delta0+delta1+delta2+delta3+delta4

######### c9
volat <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/volat.dta")

modelc11 <- lm(rsp500 ~ pcip+i3, data=volat)
summary(modelc11)


########## c10
intdef <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/intdef.dta")

cor(intdef$inf, intdef$def)  # correlation of inf and def

# (10.15) with inf and def lags
tsdata <- ts(intdef, start = 1948)
modelc10 <- dynlm(i3 ~ inf+L(inf)+def+L(def), tsdata)
summary(modelc10)

pf(((0.685-.602)/2)/((1-0.685)/50), 2, 50, lower.tail = FALSE)


############ c11
traffic2 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/traffic2.dta")

tsdata <- ts(traffic2, start = c(1981,1), frequency = 12)

modelc11ts <- dynlm(ltotacc ~ trend(tsdata)+season(tsdata), data=tsdata)
summary(modelc11ts)

modelc11 <- dynlm(ltotacc ~ wkends+unem+spdlaw+beltlaw+trend(tsdata)+season(tsdata), data=tsdata)
summary(modelc11)

mean(traffic2$prcfat)

modelc112 <- dynlm(prcfat ~ wkends+unem+spdlaw+beltlaw+trend(tsdata)+season(tsdata), data=tsdata)
summary(modelc112)

########## c12
minwage <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/minwage.dta")
tsdata <-ts(minwage, start = c(1998,1), frequency = 12)

modelc12 <- lm(gwage232 ~ gmwage+gcpi, data=minwage)
summary(modelc12)

modelc12lag <- dynlm(gwage232 ~ gmwage+L(gmwage, 1:12)+gcpi, data=tsdata)
summary(modelc12lag)
linearHypothesis(modelc12lag, matchCoefs(modelc12lag, 'gmwage'))

modelc12em <- lm(gemp232 ~ gmwage+gcpi, data = minwage)
summary(modelc12em)

modelc12emlag <- dynlm(gemp232 ~ gmwage+L(gmwage, 1:12)+gcpi, data=tsdata)
summary(modelc12emlag)

linearHypothesis(modelc12emlag, matchCoefs(modelc12emlag, 'gmwage'))