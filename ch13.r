library(foreign);library(plm);library(car);library(lmtest)

########### c1
fertil1 <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/fertil1.dta')

modelc1 <- lm(kids ~ educ+age+agesq+black+east+northcen+west+farm+othrural+town+
                 smcity+y74+y76+y78+y80+y82+y84, data = fertil1)
summary(modelc1)

linearHypothesis(modelc1, c('farm', 'othrural', 'town', 'smcity'))

linearHypothesis(modelc1, c('east', 'northcen', 'west'))

uhat <- resid(modelc1)

modelc1.1 <- lm(I(uhat^2) ~ y74+y76+y78+y80+y82+y84, data = fertil1)
summary(modelc1.1)

modelc1.2 <- lm(kids ~ educ*(y74+y76+y78+y80+y82+y84)+age+agesq+black+east+
                  northcen+west+farm+othrural+town+smcity, data = fertil1)
summary(modelc1.2)

fvalue <- ((0.1365-0.1295)/6)/((1-0.1365)/1105)
pf(fvalue, 6, 1105, lower.tail = FALSE)


############### c2 
cps7885<- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/cps78_85.dta')

modelc2 <- lm(lwage ~ y85*(educ+female)+exper+expersq+union, data=cps7885)
summary(modelc2)

modelc2.1 <- lm(lwage ~ y85*female+educ+I(y85educ-12*y85)+exper+expersq+union, 
                data=cps7885)
summary(modelc2.1)
cps7885$rwage <- with(cps7885, ifelse(year == 78, lwage, lwage-log(1.65)))

modelc2.2 <- lm(rwage ~ y85*(educ+female)+exper+expersq+union, data=cps7885)
summary(modelc2.2)

uhat1 <- resid(modelc2)
uhat2 <- resid(modelc2.2)

modelc2.3 <- lm(lwage ~ y85*(educ+female+union)+exper+expersq, data=cps7885)
summary(modelc2.3)


############### c3
kielmc <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/kielmc.dta')

modelc3 <- lm(lprice ~ y81*ldist, data = kielmc)
summary(modelc3)

modelc3.2 <- lm(lprice ~ y81*ldist +age+agesq+rooms+baths+lintst+lland+larea, data = kielmc)
summary(modelc3.2)


################ c4
injury <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/injury.dta')

modelc4.ky <- lm(ldurat ~ afchnge*highearn+male+married+head+neck+upextr+trunk+
                lowback+lowextr+occdis+manuf+construc, data=injury, subset = (ky==1))
summary(modelc4.ky)

modelc4.mi <- lm(ldurat ~ afchnge*highearn, data=injury, subset = (mi == 1))
summary(modelc4.mi)


################# c5
rental <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/rental.dta')

rental.p <- pdata.frame(rental, index = c('city', 'year'))
pdim(rental.p)

modelc5.ols <- lm(lrent ~ y90+lpop+lavginc+pctstu, data = rental)
summary(modelc5.ols)

bptest(modelc5.ols)  ##异方差检验

bgtest(modelc5.ols, 1)  ##残差项序列相关

rental.odd <- rental[c(TRUE,FALSE),]
rental.even <- rental[!c(TRUE,FALSE),]
rental.d <- rental.even-rental.odd

# modelc5.fd <- plm(lrent ~ trend(rental.p) +lpop+lavginc+pctstu, data = rental.p, model = 'fd')
modelc5.fd <- lm(lrent ~ lpop+lavginc+pctstu, data=rental.d)
summary(modelc5.fd)

coeftest(modelc5.fd, vcovHC(modelc5.fd, 'HC0'))

modelc5.pool <- plm(lrent ~ y90+lpop+lavginc+pctstu, data = rental.p, model = 'pooling')
summary(modelc5.pool)

############# c6
crime3 <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/crime3.dta')

crime3.odd <- crime3[c(TRUE, FALSE),]
crime3.even <- crime3[c(FALSE, TRUE),]
crime3.d <- crime3.even - crime3.odd

modelc6.fd <- lm(lcrime ~ clrprc1+I(clrprc2+clrprc1), data=crime3.d) 
summary(modelc6.fd)

modelc6.2 <- lm(clcrime ~ I((cclrprc2+cclrprc1)/2), data=crime3)
summary(modelc6.2)

############### c7
gpa3 <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/gpa3.dta')

gpa3.p <- pdata.frame(gpa3, c('id', 'term'))

modelc7.pool <- plm(trmgpa ~ spring+sat+hsperc+female+black+white+
                      frstsem+tothrs+crsgpa+season, data = gpa3.p, model = 'pooling')
summary(modelc7.pool)

modelc7.fd <- plm(trmgpa ~ spring+sat+hsperc+female+black+white+
                      frstsem+tothrs+crsgpa+season, data = gpa3.p, model = 'fd')
summary(modelc7.fd)

############## c8
vote2 <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/vote2.dta')

modelc8.fd <- lm(cvote ~ clinexp+clchexp+cincshr, data = vote2)
summary(modelc8.fd)

linearHypothesis(modelc8.fd, c('clinexp', 'clchexp'))

modelc8.fd.2 <- lm(cvote ~ cincshr, data = vote2)
summary(modelc8.fd.2)

modelc8.fd.3 <- lm(cvote ~ cincshr, data = vote2, subset=(rptchall==1))
summary(modelc8.fd.3)

################ c9
crime4 <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/crime4.dta')
crime4$clwcon = c(NA,diff(crime4$lwcon))
crime4$clwtuc = c(NA,diff(crime4$lwtuc))
crime4$clwtrd = c(NA,diff(crime4$lwtrd))
crime4$clwfir = c(NA,diff(crime4$lwfir))
crime4$clwser = c(NA,diff(crime4$lwser))
crime4$clwmfg = c(NA,diff(crime4$lwmfg))
crime4$clwfed = c(NA,diff(crime4$lwfed))
crime4$clwsta = c(NA,diff(crime4$lwsta))
crime4$clwloc = c(NA,diff(crime4$lwloc))

modelc9 <- lm(clcrmrte ~ d83+d84+d85+d86+d87+
                clprbarr+clprbcon+clprbpri+clavgsen+clpolpc+
                clwcon+clwtuc+clwtrd+clwfir+clwser+clwmfg+clwfed+clwsta+clwloc,
              data = crime4)
summary(modelc9)

linearHypothesis(modelc9, matchCoefs(modelc9, 'clw'))

################# c10
wagepan <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/wagepan.dta')
wagepan.p <- pdata.frame(wagepan, c('nr', 'year'))

modelc10.pool <- plm(lwage ~ educ+black+hisp+exper+married+union+
                       d81+d82+d83+d84+d85+d86+d87, data=wagepan.p, model='pooling')
modelc10.ols <- lm(lwage ~ educ+black+hisp+exper+married+union+
                       d81+d82+d83+d84+d85+d86+d87, data=wagepan)
summary(modelc10.pool)

library(sandwich)
coeftest(modelc10.ols, vcovHAC)
coeftest(modelc10.pool, vcovHC)

wagepan.p$cmarried <- diff(wagepan.p$married)
wagepan.p$cunion <- diff(wagepan.p$union)
wagepan.p$clwage <- diff(wagepan.p$lwage)
modelc10.fd <- lm(clwage ~ cmarried+cunion+
                     d82+d83+d84+d85+d86+d87, data=wagepan.p)
summary(modelc10.fd)


############# c11 
mathpnl <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/mathpnl.dta')

mathpnl.p <- pdata.frame(mathpnl, c('distid', 'year'))

modelc11.pool <- plm(math4 ~ y93+y94+y95+y96+y97+y98+lrexpp+lenrol+lunch, 
                     data = mathpnl.p, model = 'pooling')
summary(modelc11.pool)

mathpnl.p$clrexpp <- diff(mathpnl.p$lrexpp)
mathpnl.p$clrexpp_1 <- lag(diff(mathpnl.p$lrexpp))
mathpnl.p$clenrol <- diff(mathpnl.p$lenrol)
mathpnl.p$clunch <- diff(mathpnl.p$lunch)

modelc11.fd <- lm(cmath4 ~ y94+y95+y96+y97+y98+clrexpp+clenrol+clunch, data = mathpnl.p)
summary(modelc11.fd)

modelc11.fd.lag <- lm(cmath4 ~ y95+y96+y97+y98+clrexpp_1+clenrol+clunch, data = mathpnl.p)
summary(modelc11.fd.lag)

coeftest(modelc11.fd.lag, vcovHC)
coeftest(modelc11.fd.lag, vcovHAC)

uhat <- resid(modelc11.fd.lag)

arima(uhat, c(1,0,0))
ar(uhat, order.max = 1)

modelc11.fd.lag.2 <- lm(cmath4 ~ y95+y96+y97+y98+clrexpp_1, data = mathpnl.p)
waldtest(modelc11.fd.lag, modelc11.fd.lag.2, vcov=vcovHAC)


################ c12
murder <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/murder.dta')
murder.p <- pdata.frame(murder, c('state', 'year'))
pdim(murder.p)

modelc12.pool <- plm(mrdrte ~ d93+exec+unem, data = subset(murder.p, year != 87), model = 'pool')
summary(modelc12.pool)

modelc12.fd <- lm(cmrdrte ~ cexec+cunem, data = subset(murder.p, year ==93))
summary(modelc12.fd)

yhatsq <- fitted(modelc12.fd)^2
uhatsq <- resid(modelc12.fd)^2
bptest(modelc12.fd)
summary(lm(uhatsq ~ yhatsq))

coeftest(modelc12.fd, vcovHC(modelc12.fd, 'HC0'))


############## c13
modelc13.fd <- plm(lwage ~ (d81+d82+d83+d84+d85+d86+d87)*educ+union, 
                   data = wagepan.p, model = 'fd')
summary(modelc13.fd)

modelc13.fd.hac <- plm(lwage ~ d81+d82+d83+d84+d85+d86+d87+educ+union, data = wagepan.p, model = 'fd')
summary(modelc13.fd.hac)

linearHypothesis(modelc13.fd, matchCoefs(modelc13.fd, 'educ'))
#linearHypothesis(modelc13.fd, matchCoefs(modelc13.fd, 'educ'), vcovHC)
waldtest(modelc13.fd, modelc13.fd.hac, vcovHAC)

modelc13.fd.2 <- plm(lwage ~ (d81+d82+d83+d84+d85+d86+d87)*(educ+union), 
                   data = wagepan.p, model = 'fd')
summary(modelc13.fd.2)

linearHypothesis(modelc13.fd.2, matchCoefs(modelc13.fd.2, 'union'))

############ c14
jtrain3 <- read.dta('http://fmwww.bc.edu/ec-p/data/wooldridge/jtrain3.dta')

modelc14.1 <- lm(re78 ~ train, data = jtrain3)
summary(modelc14.1)

modelc14.2 <- lm(I(re78-re75) ~ train, data = jtrain3)
summary(modelc14.2)

coeftest(modelc14.2, vcovHC(modelc14.2, 'HC0'))
