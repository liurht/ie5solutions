library(foreign);library(plm);library(lmtest)

# c1
rental <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/rental.dta')
rental.p <- pdata.frame(rental, c('city', 'year'))

modelc1 <- plm(lrent ~ y90+lpop+lavginc+pctstu, data = rental.p, model = 'pooling')
summary(modelc1)

bptest(modelc1)
bgtest(modelc1)

modelc1.fd <- plm(lrent ~ y90+lpop+lavginc+pctstu, data = rental.p, model = 'fd')
summary(modelc1.fd)

modelc1.fe <- plm(lrent ~ y90+lpop+lavginc+pctstu, data = rental.p, model = 'within')
summary(modelc1.fe)

# c2
crime4 <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/crime4.dta')
crime4.p <- pdata.frame(crime4, index=c("county","year"))

modelc2.fe <- plm(lcrmrte ~ factor(year)+lprbarr+lprbconv+lprbpris+lavgsen+lpolpc, 
                  data=crime4.p, model='within')
summary(modelc2.fe)


modelc2.fe.2 <- plm(lcrmrte ~ factor(year)+lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+
                    lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc, 
                  data=crime4.p, model='within')
summary(modelc2.fe.2)

library(car)
linearHypothesis(modelc2.fe.2, matchCoefs(modelc2.fe.2, 'lw'))

# c3
jtrain <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/jtrain.dta')
jtrain.p <- pdata.frame(jtrain, c('fcode', 'year'))

modelc3.fe <- plm(hrsemp ~ factor(year)+grant+grant_1+lemploy, 
                  data = jtrain.p, model = 'within')
summary(modelc3.fe)

# c4 
ezunem <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/ezunem.dta')
ezunem.p <- pdata.frame(ezunem, c('city', 'year'))

modelc4.dif <- plm(guclms ~ cez, data = ezunem.p, model = 'within')
summary(modelc4.dif)

modelc4.dif.2 <- plm(guclms ~ factor(year)+cez, data = ezunem.p, model = 'within')
summary(modelc4.dif.2)

# c5
wagepan <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/wagepan.dta')
wagepan.p <- pdata.frame(wagepan, c('nr', 'year'))

modelc5.fe <- plm(lwage ~ educ+black+hisp+exper+expersq+married+union+
                    occ2+occ3+occ4+occ5+occ6+occ7+occ8+occ9, data = wagepan.p, model = 'within')
summary(modelc5.fe)

# c6
modelc6.pool <- plm(lwage ~ educ+black+hisp+exper+expersq+married+union+
                      factor(year):union, data = wagepan.p, model = 'pooling')
modelc6.re <- plm(lwage ~ educ+black+hisp+exper+expersq+married+union+
                      factor(year):union, data = wagepan.p, model = 'random')
modelc6.fe <- plm(lwage ~ educ+black+hisp+exper+expersq+married+union+
                    factor(year):union, data = wagepan.p, model = 'within')
summary(modelc6.pool)
summary(modelc6.re)
summary(modelc6.fe)

# c7
murder <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/murder.dta')

murder.p <- pdata.frame(murder, c('state', 'year'))

modelc7.pool <- plm(mrdrte ~ exec+unem+d93, data = murder.p, 
                    subset=(year==90 | year==93 ), model = 'pooling')
summary(modelc7.pool)

modelc7.fe <- plm(mrdrte ~ exec+unem, data = murder.p, 
                    subset=(year==90 | year==93 ), model = 'within')
summary(modelc7.fe)

modelc7.fd <- plm(mrdrte ~ exec+unem, data = murder.p, 
                  subset=(year==90 | year==93 ), model = 'fd')
summary(modelc7.fd)
coeftest(modelc7.fe, vcovHC)
coeftest(modelc7.fd, vcovHC)

exec.srt <- sort(murder$exec, decreasing = TRUE)
head(exec.srt)

modelc7.ntx <- plm(mrdrte ~ exec+unem, data = murder.p, 
                   subset=((year==90 | year==93) & state != 'TX'), model = 'fd') 
summary(modelc7.ntx)

coeftest(modelc7.ntx, vcovHC)

modelc7 <- plm(mrdrte ~ factor(year)+exec+unem, data = murder.p, model = 'within') 
summary(modelc7)
coeftest(modelc7, vcovHC)

# c8
mathpnl <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/mathpnl.dta')
mathpnl.p <- pdata.frame(mathpnl, c('distid', 'year'))

modelc8.pool <- plm(math4 ~ factor(year)+lexpp+lexpp_1+lenrol+lunch,
                  data = mathpnl.p, model = 'pooling')
summary(modelc8.pool)

vhat <- resid(modelc8.pool)
arima(vhat, c(1,0,0))
bgtest(modelc8.pool)

modelc8.fe <- plm(math4 ~ factor(year)+lexpp+lexpp_1+lenrol+lunch,
                  data = mathpnl.p, model = 'within')
summary(modelc8.fe)

modelc8.lrp <- plm(math4 ~ factor(year)+lexpp+I(lexpp_1-lexpp)+lenrol+lunch,
                  data = mathpnl.p, model = 'within')
summary(modelc8.lrp)

bgtest(modelc8.fe)

# c9
load('e:/wooldridge/datasets/wagepan.RData')
wagepan.p <- pdata.frame(data, c('nr', 'year'))

modelc9.pool <- plm(lwage ~ educ+black+hisp, data = wagepan.p, model = 'pooling')
summary(modelc9.pool)

modelc9.re <- plm(lwage ~ educ+black+hisp, data = wagepan.p, model = 'random')
summary(modelc9.re)

modelc9.pool.2 <- plm(lwage ~ educ+black+hisp + factor(year), data = wagepan.p, model = 'pooling')
summary(modelc9.pool.2)

modelc9.re.2 <- plm(lwage ~ educ+black+hisp + factor(year), data = wagepan.p, model = 'random')
summary(modelc9.re.2)

modelc9.fe <- plm(lwage ~ educ+black+hisp + factor(year), data = wagepan.p, model = 'within')
summary(modelc9.fe)

# c10
airfare <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/airfare.dta')
airfare.p <- pdata.frame(airfare, c('id', 'year'))

modelc10.pool <- plm(lfare ~ concen+ldist+ldistsq+factor(year), data = airfare.p,
                     model = 'pooling')
summary(modelc10.pool)

library(sandwich)
coeftest(modelc10.pool, vcov=vcovHC(modelc10.pool, type='HC0'))
0.3601203-1.96*0.0584923
0.3601203+1.96*0.0584923

modelc10.re <- plm(lfare ~ concen+ldist+ldistsq+factor(year), data = airfare.p,
                     model = 'random')
summary(modelc10.re)

modelc10.fe <- plm(lfare ~ concen+ldist+ldistsq+factor(year), data = airfare.p,
                   model = 'within')
summary(modelc10.fe)

# c11
modelc11.ols <- plm(lwage ~ educ+black+hisp+exper+expersq+married+union+factor(year),
                    data = wagepan.p, model = 'pooling')
coeftest(modelc11.ols, vcovHC)

modelc11.fe <- plm(lwage ~ expersq+married+union+factor(year),
                    data = wagepan.p, model = 'within')
coeftest(modelc11.fe, vcovHC)

# c12 
elem <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/elem94_95.dta')
elem.p <- pdata.frame(elem, c('distid','schid'))
pdim(elem.p)

ds <- with(elem, table(distid))
ds.v <- as.vector(ds)
max(ds)
min(ds)
mean(ds)

modelc12.pool <- plm(lavgsal ~ bs+lenrol+lstaff+lunch, data = elem.p, model = 'pooling')
summary(modelc12.pool)

coeftest(modelc12.pool, vcovHC)

modelc12.pool.2 <- plm(lavgsal ~ bs+lenrol+lstaff+lunch, data = elem.p, 
                     subset = (bs<=.5), model = 'pooling')
summary(modelc12.pool.2)

coeftest(modelc12.pool.2, vcovHC)

modelc12.fe <- plm(lavgsal ~ bs+lenrol+lstaff+lunch, data = elem.p, 
                   subset = (bs<=.5), model = 'within')
summary(modelc12.fe)

coeftest(modelc12.pool, vcovHC)