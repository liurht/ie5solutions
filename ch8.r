setwd('e:/wooldridge/datasets')

# import the lmtest library and sandwich library
library(lmtest)
library(sandwich)

# the original model
load('wage1.RData')
modeleg1 <- lm(lwage ~ exper, data)
summary(modeleg1)

load('401ksubs.RData')
data = subset(data, fsize==1)
modeleg86 <- lm(nettfa ~ inc, data)
u2 <- modeleg86$residuals^2
yp <- modeleg86$fitted.values
ypsq <- yp^2

newdata <- cbind(u2, yp, ypsq, data)
models83 <- lm(u2 ~ inc, newdata)
summary(models83)

models832 <- lm(u2 ~ yp+ypsq, newdata)
summary(models832)

whitetest <- nrow(newdata)*0.04282
pchisq(whitetest, 2, lower.tail = FALSE)

#example6
load('401ksubs.RData')
modele6ols <- lm(nettfa ~ inc+I((age-25)^2)+male+e401k, data = data, subset = (fsize==1))
summary(modele6ols)

#for heteroskedasticity-robust standard error
library(lmtest)
library(sandwich)

coeftest(modele6ols, vcov. = vcovHC)
coeftest(modele6ols, vcov. = vcovHC(modele6ols, type = 'HC0'))

##################
# vcov argument setting
library(foreign)
library(sandwich)
library(lmtest)

dfAPI = read.dta("http://www.ats.ucla.edu/stat/stata/webbooks/reg/elemapi2.dta")
lmAPI = lm(api00 ~ acs_k3 + acs_46 + full + enroll, data= dfAPI)
summary(lmAPI)                                  # non-robust

# check that "sandwich" returns HC0
coeftest(lmAPI, vcov = sandwich)                # robust; sandwich
coeftest(lmAPI, vcov = vcovHC(lmAPI, "HC0"))    # robust; HC0 

# check that the default robust var-cov matrix is HC3
coeftest(lmAPI, vcov = vcovHC(lmAPI))           # robust; HC3 
coeftest(lmAPI, vcov = vcovHC(lmAPI, "HC3"))    # robust; HC3 (default)

# reproduce the Stata default
coeftest(lmAPI, vcov = vcovHC(lmAPI, "HC1"))    # robust; HC1 (Stata default)


## The last line of code above reproduces results from Stata:

# use http://www.ats.ucla.edu/stat/stata/webbooks/reg/elemapi2
# regress api00 acs_k3 acs_46 full enroll, robust

##################

# wls regression
modele6wls <- lm(nettfa ~ inc+I((age-25)^2)+male+e401k, weights= 1/inc, data = data, subset =(fsize==1))
summary(modele6wls)

## wls regression with heteroskatasticity-robust standard error
coeftest(modele6wls, vcov. = vcovHC)

## example 8.9
load('gpa1.RData')

### OLS
modele7 <- lm(PC ~ hsGPA+ACT+I(fathcoll | mothcoll), data)
summary(modele7)
yhat <- fitted(modele7)
### OLS with heteroskatasticity-robust sd
coeftest(modele7, vcov. = vcovHC(modele7, 'HC0'))

### WLS
#### compute the weights

hx <- yhat*(1-yhat)
modele7wls <- lm(PC ~ hsGPA+ACT+I(fathcoll | mothcoll), data, weights = 1/hx)
summary(modele7wls)

###############
#习题
###############
load('gpa3.RData')
## ols model
modelex4ols <- lm(trmgpa ~ crsgpa+cumgpa+tothrs+sat+hsperc+female+season, data, subset = (term==1 & frstsem==0))
## h-robust standard error
coeftest(modelex4ols, vcov. = vcovHC(modelex4ols, 'HC0'))

###############
#computer exercise
###############

# ce 1
load('sleep75.RData')
modelce11 <- lm(sleep ~ totwrk+educ+age+agesq+yngkid+male, data)
u2 <- resid(modelce11)^2

##计算男性女性的残差的估计方差
tapply(u2, data$male, sd)

## 检验残差的方差对男女的差异是否显著
summary(lm(u2~male, data))

bptest(modelce11)


# ce 2
load('hprice1.RData')

## ols model
modelce21 <- lm(price ~ lotsize+sqrft+bdrms, data)

##h-robust sd
coeftest(modelce21, vcov. = vcovHC(modelce21, 'HC0'))

# ols model with log variables
modelce22 <- lm(lprice ~ llotsize+lsqrft+bdrms, data)

## h-robust sd
coeftest(modelce22, vcov. = vcovHC(modelce22, 'HC0'))

## white test for heteroskedasticity 的特殊情况
bptest(modelce22, ~ fitted(modelce22)+I(fitted(modelce22)^2))

## white test for heteroskedasticity 的完全情况
bptest(modelce22, ~ llotsize*lsqrft+llotsize*bdrms+lsqrft*bdrms+I(llotsize^2)+I(lsqrft^2)+I(bdrms^2), data=data)


############## ce 4
load('vote1.RData')

# fit the origin ols model
modelce41 <- lm(voteA ~ prtystrA+democA+lexpendA+lexpendB, data = data)
# get its residual
u <- resid(modelce41)

# fit the model of residual to all independent variables
modelce42 <- lm(u ~ prtystrA+democA+lexpendA+lexpendB, data = data)
summary(modelce42)

# bp test for heteroskedasticity
bptest(modelce41)

# white test
bptest(modelce41, ~ fitted(modelce41)+I(fitted(modelce41)^2))

####################
#ce 5 
load('pntsprd.RData')

modelce5 <- lm(sprdcvr ~ 1, data)
summary(modelce5)

tvalue <- (0.51537-.5)/0.02127
pt(tvalue, 552, lower.tail = FALSE)*2

### percent of neutral site
table(data$neutral)

## ols model
modelce52 <- lm(sprdcvr ~ favhome+neutral+fav25+und25, data) 
summary(modelce52)

##h-robust sd
coeftest(modelce52, vcov. = vcovHC(modelce52, 'HC0'))


###############
#ce 6 

load('crime1.RData')
## ols model
arr86 <- ifelse(data$narr86>0, 1, 0)
modelce7ols <- lm(arr86 ~ pcnv+avgsen+tottime+ptime86+qemp86, data)
summary(modelce7ols)

yhat <- fitted(modelce7ols)
summary(yhat)



###wls
# weights
hx <- yhat*(1-yhat)

modelce7wls <- lm(arr86 ~ pcnv+avgsen+tottime+ptime86+qemp86, weights=1/hx, data)
summary(modelce7wls)

## test the joint significence of avgsen and tottime
modelce7wls2 <- lm(arr86 ~ pcnv+ptime86+qemp86, weights=1/hx, data)
summary(modelce7wls2)

fvalue= ((0.07439-0.07378)/2)/((1-0.07439)/2719)
pf(fvalue, 2, 2719, lower.tail = FALSE)


########### ce 7
load('loanapp.RData')

## ols model
modelce7 <- lm(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+
                 cosign+chist+pubrec+mortlat1+mortlat2+vr, data)
summary(modelce7)

## h-robust sd
coeftest(modelce7, vcov. = vcovHC(modelce7, 'HC0'))

## 95% confidence interval
c(0.1288196-qt(.975,1955)*0.019732,
0.1288196+qt(.975,1955)*0.019732) # for ols

c(0.1288196-qt(.975,1955)*0.0257641,
  0.1288196+qt(.975,1955)*0.0257641) # for h-robust sd

yhat <- fitted(modelce7)
summary(yhat)

table(yhat>1)

################## ce 8
load('gpa1.RData')

## ols model
modelce8ols <- lm(colGPA ~ hsGPA+ACT+skipped+PC, data)
uhat <- resid(modelce8ols)  # the residual of ols model

## white test 
bptest(modelce8ols, ~ fitted(modelce8ols)+I(fitted(modelce8ols)^2)) # from the bptest

modelce8white <- lm(I(uhat^2) ~ fitted(modelce8ols)+I(fitted(modelce8ols)^2))
hhat <- fitted(modelce8white)

summary(hhat) # check if it's all positive

## wls using hhat
modelce8wls <- lm(colGPA ~ hsGPA+ACT+skipped+PC, weights=1/hhat, data)
summary(modelce8wls)

## h-robust sd for wls model
coeftest(modelce8wls, vcov. = vcovHC(modelce8wls, 'HC0'))


################ ce 9
load('smoke.RData')

## ols model
modelce9ols <- lm(cigs ~ lincome+lcigpric+educ+age+agesq+restaurn, data)
yhat <- fitted(modelce9ols)  #fitted value of ols model

## fgls model
logu2 <- log(resid(modelce9ols)^2)
modelce9weights <- lm(logu2 ~ lincome+lcigpric+educ+age+agesq+restaurn, data) # estimate the weights
hhat <- exp(fitted(modelce9weights))

modelce9fgls <- lm(cigs ~ lincome+lcigpric+educ+age+agesq+restaurn, data, weights = 1/hhat)
summary(modelce9fgls)

### residual and fitted value from the fgls model
ufgls <- resid(modelce9fgls)
yfgls <- fitted(modelce9fgls)

## check the heteroskedasticity of the wls model
ubreve <- ufgls/hhat^.5
ybreve <- yfgls/hhat^.5

modelce9h <- lm(I(ubreve^2) ~ ybreve+I(ybreve^2))
summary(modelce9h)  # heteroskedasticity still exists

## h-robust sd for fgls model
coeftest(modelce9fgls, vcov. = vcovHC(modelce9fgls, 'HC0'))


############## ce 10
load('401ksubs.RData')

modelce10ols <- lm(e401k ~ inc+incsq+age+agesq+male, data)
summary(modelce10ols)

## h-robust sd
coeftest(modelce10ols, vcov. = vcovHC(modelce10ols, 'HC0'))

## white test 
bptest(modelce10ols, ~ fitted(modelce10ols)+I(fitted(modelce10ols)^2))
modelce10white <- lm(I(resid(modelce10ols)^2)~ fitted(modelce10ols)+I(fitted(modelce10ols)^2))
summary(modelce10white)

summary(fitted(modelce10ols)) ## check the fitted values are all between 0 and 1

## wls model
hx <- fitted(modelce10ols)*(1-fitted(modelce10ols))
modelce10wls <- lm(e401k ~ inc+incsq+age+agesq+male, data, weights = 1/hx)
summary(modelce10wls)

################# ce 11
modelce11ols <- lm(nettfa ~ inc+I((inc-10)^2)+age+I((age-25)^2)+e401k, data, subset = (marr==1 & fsize==2))
summary(modelce11ols)

## h-robust sd
coeftest(modelce11ols, vcov. = vcovHC(modelce11ols, 'HC0'))

# h-robust wald test
modelce112 <- lm(nettfa ~ e401k, data, subset = (marr==1 & fsize==2))
waldtest(modelce11ols, modelce112, vcov = vcovHC(modelce11ols, 'HC0'))

# fgls
logusq <- log(resid(modelce11ols)^2)
wdata <- cbind(subset(data, marr==1 & fsize==2), logusq)
modelce11weights <- lm(logusq ~ inc+I((inc-10)^2)+age+I((age-25)^2)+e401k, wdata)

hhat <- exp(fitted(modelce11weights))  # weights

modelce11wls <- lm(nettfa ~ inc+I((inc-10)^2)+age+I((age-25)^2)+e401k, wdata, weights = 1/hhat)
summary(modelce11wls)

coeftest(modelce11wls, vcov = vcovHC(modelce11wls, 'HC0'))

###通常的WLS模型的F检验
modelce11wlsr <- lm(nettfa ~ e401k, wdata, weights = 1/hhat)
summary(modelce11wlsr)

fvalue <- ((0.1622-0.01522)/4)/((1-0.1622)/1488)
pf(fvalue, 4, 1488, lower.tail = FALSE)

###稳健的WLS模型的F检验
waldtest(modelce11wls, modelce11wlsr, vcov = vcovHC(modelce11wls, 'HC0'))
