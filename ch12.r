library(foreign);library(lmtest);library(sandwich);library(orcutt);library(prais)
library(dynlm);library(car)

############## c1
fertil3 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/fertil3.dta")
tsdata <- ts(fertil3, start = 1913)

modelc1 <- dynlm(d(gfr) ~ d(pe)+L(d(pe))+L(d(pe), 2), data=tsdata)

bgtest(modelc1, order=1, type="F")

uhat <- resid(modelc1)
modelc1.2 <- dynlm(uhat ~ d(pe)+L(d(pe))+L(d(pe), 2) + L(uhat), data=tsdata)
linearHypothesis(modelc1.2, c("L(uhat)"))

modelc1.2 <- dynlm(uhat ~ L(uhat))  ###以回归元严格外生为假定
summary(modelc1.2)

############# c2
wageprc <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wageprc.dta")
tsdata <- ts(wageprc, start = c(1990, 1), frequency = 12)

modelc2.1 <- dynlm(gprice ~ gwage+L(gwage, 1:12), data = tsdata)
summary(modelc2.1)

uhat <- resid(modelc2.1)
modelc2.2 <- dynlm(uhat ~ L(uhat))
summary(modelc2.2)

modelc2.co <- cochrane.orcutt(modelc2.1)
summary(modelc2.co)

modelc2.lrp <- dynlm(gprice ~ gwage+I(L(gwage, 1:12)-gwage), data = tsdata)
summary(modelc2.lrp)
modelc2.lrp.co <- cochrane.orcutt(modelc2.lrp)
summary(modelc2.lrp.co)


############### c3 
inven <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/inven.dta")
tsdata <- ts(inven, start = 1959)

modelc3.1 <- dynlm(d(inven) ~ d(gdp), data = tsdata)
uhat <- resid(modelc3.1)

modelc3.2 <- dynlm(uhat ~ L(uhat))
summary(modelc3.2)

bgtest(modelc3.2, order=1)

cochrane.orcutt(modelc3.1)  ### no need

############## c4
nyse <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/nyse.dta")
tsdata <- ts(nyse)

modelc4.1 <- dynlm(return ~ L(return), data = tsdata)
uhat <- resid(modelc4.1)
summary(modelc4.1)

uhatsq <- uhat^2
modelc4.2 <- dynlm(uhatsq ~ L(return), data = tsdata)
summary(modelc4.2)
u2.fitted <- fitted(modelc4.2)
table(u2.fitted<0)

modelc4.3 <- dynlm(uhatsq ~ L(return)+I(L(return)^2), data = tsdata)
u2.fitted2 <- fitted(modelc4.3)
table(u2.fitted2<0)  

modelc4.4 <- lm(return ~ return_1, data = nyse, weights = c(rep(0,2), 1/u2.fitted2))
summary(modelc4.4)

modelc4.5 <- dynlm(uhatsq ~ L(uhatsq))
u2.fitted3 <- fitted(modelc4.5)
summary(modelc4.5)

modelc4.6 <- lm(return ~ return_1, data = nyse, weights = c(rep(0,3), 1/u2.fitted3))
summary(modelc4.6)


################## c5
fair <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/fair.dta")
tsdata <- ts(fair)
tsdata1992 <- ts(subset(fair, year<=1992))

modelc5.1 <- dynlm(demwins ~ partyWH+incum+partyWH:gnews+partyWH:inf, 
                   data = tsdata1992)
summary(modelc5.1)

denwins.hat <- fitted(modelc5.1)
summary(denwins.hat)
table(denwins.hat>1)
table(denwins.hat<0)
demwins.pred <- ifelse(denwins.hat>0.5, 1, 0)
table(demwins.pred==fair$demwins[-21])

predict(modelc5.1, fair[21,]) # prediction for 1996 year

uhat <- resid(modelc5.1)
modelc5.2 <- dynlm(uhat ~ L(uhat))
summary(modelc5.2)

coeftest(modelc5.2, vcov = vcovHC(modelc5.2, 'HC0'))

coeftest(modelc5.1, vcov = vcovHC(modelc5.1, 'HC0'))

############ c6
consump <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/consump.dta")
tsdata <- ts(consump, start = 1959)

modelc6.1 <- dynlm(d(log(c)) ~ d(log(y)), data = tsdata)
summary(modelc6.1)

uhat <- resid(modelc6.1)
modelc6.2 <- dynlm(uhat ~ L(uhat))
summary(modelc6.2)

modelc6.3 <- dynlm(d(log(c)) ~ L(d(log(c))), data = tsdata)
uhat2 <- resid(modelc6.3)
uhat2sq <- uhat2^2

modelc6.4 <- dynlm(uhat2sq ~ L(d(log(c))) +I(L(d(log(c)))^2), data=tsdata)
summary(modelc6.4)                               

############### c7
barium <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/barium.dta") 
tsdata <- ts(barium, start = c(1959, 2), frequency = 12)

modelc7 <- dynlm(lchnimp ~ lchempi+lgas+lrtwex+befile6+affile6+afdec6, data = tsdata)
cochrane.orcutt(modelc7)

############### c8
traffic2 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/traffic2.dta") 
tsdata <- ts(traffic2, start = c(1981,1), frequency = 12)

modelc8 <- dynlm(prcfat ~ trend(tsdata)+season(tsdata)+wkends+unem+spdlaw+beltlaw, 
                 data = tsdata)
uhat <- resid(modelc8)

modelc8.2 <- dynlm(uhat ~ L(uhat))
summary(modelc8.2)
bgtest(modelc8, 1)

coeftest(modelc8, vcovHAC)

prais.winsten(prcfat ~ t+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec+
                wkends+unem+spdlaw+beltlaw, data = traffic2)


############## c9
fish <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/fish.dta") 
tsdata <- ts(fish)

modelc9.1 <- dynlm(lavgprc ~ trend(tsdata)+mon+tues+wed+thurs, data=tsdata)
summary(modelc9.1)

linearHypothesis(modelc9.1, c('mon', 'tues', 'wed', 'thurs'))

modelc9.2 <- dynlm(lavgprc ~ trend(tsdata)+mon+tues+wed+thurs +
                     wave2+wave3, data=tsdata)
summary(modelc9.2)

uhat <- resid(modelc9.2)
modelc9.3 <- dynlm(uhat ~ L(uhat))
summary(modelc9.3)

coeftest(modelc9.2, vcovHAC)

modelc9.4 <- cochrane.orcutt(modelc9.2)
summary(modelc9.4)

prais.winsten(lavgprc ~ t+mon+tues+wed+thurs + wave2+wave3, data=fish)

############# c10
phillips <- read.dta('e:/wooldridge/URfIE/phillips.dta')
tsdata <- ts(phillips, start = 1948)

modelc10 <- dynlm(inf ~ unem, data=tsdata)
summary(modelc10)

uhat <- resid(modelc10)
modelc10.2 <- dynlm(uhat ~ L(uhat))
summary(modelc10.2)

prais.winsten(inf ~ unem, data=tsdata)
cochrane.orcutt(modelc10)

############## c11
nyse <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/nyse.dta')
tsdata <- ts(nyse)

modelc11 <- dynlm(return ~ L(return), data = tsdata)
summary(modelc11)

uhatsq <- resid(modelc11)^2
summary(uhatsq)

modelc11.2 <- dynlm(uhatsq ~ L(return) +I(L(return)^2), data=tsdata)
summary(modelc11.2)
ret.vm = 0.78946/(2*0.29666)
3.25734-0.78946*ret.vm+0.29666*ret.vm^2

varhat <- fitted(modelc11.2)
summary(varhat)

modelc11.3 <- dynlm(uhatsq ~ L(uhatsq))
summary(modelc11.3)
varhat.arch <- fitted(modelc11.3)
summary(varhat.arch)

modelc11.4 <- dynlm(uhatsq ~ L(uhatsq,1:2))
summary(modelc11.4)

############### c12 
minwage <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/minwage.dta')
tsdata <- ts(minwage)

modelc12 <- dynlm(gwage232 ~ gmwage+gcpi, data = tsdata)
uhat <- resid(modelc12)

modelc12.2 <- dynlm(uhat ~ L(uhat))
summary(modelc12.2)

prais.winsten(gwage232 ~ gmwage+gcpi, minwage)
prais.winsten(gwage232 ~ gmwage+gcpi+gmwage_1+gmwage_2+gmwage_3+gmwage_4+gmwage_5+
        gmwage_6+gmwage_7+gmwage_8+gmwage_9+gmwage_10+gmwage_11+gmwage_12, minwage)


fvalue <- ((0.4404-0.4074)/12)/((1-0.4404)/584)
pf(fvalue, 12, 584, lower.tail = FALSE)

coeftest(modelc12, NeweyWest(modelc12, 6))

modelc12.3 <- lm(gwage232 ~ gmwage+gcpi+gmwage_1+gmwage_2+gmwage_3+gmwage_4+gmwage_5+
                   gmwage_6+gmwage_7+gmwage_8+gmwage_9+gmwage_10+gmwage_11+gmwage_12, minwage)
coeftest(modelc12.3, vcovHAC)
