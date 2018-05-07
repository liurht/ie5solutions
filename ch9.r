setwd('e:/wooldridge/datasets')

################## example 9.2
load('wage2.RData')
str(data)

### without the ability variable
modeleg21 <- lm(lwage ~ educ+exper+tenure+married+south+urban+black, data)
summary(modeleg21)

### with ability's proxy variable IQ
modeleg22 <- lm(lwage ~ educ+exper+tenure+married+south+urban+black + IQ, data)
summary(modeleg22)


################# computer exercise

#### c1
load('ceosal1.RData')

modelce5 <- lm(lsalary ~ lsales+roe+I(ros<0), data)
summary(modelce5)

##RESET test
library(lmtest)
library(sandwich)

resettest(modelce5)

yhat <- fitted(modelce5)
modelce52 <- lm(lsalary ~ lsales+roe+I(ros<0) + I(yhat^2)+I(yhat^3), data)
waldtest(modelce52, modelce5, vcov = vcovHC(modelce52, 'HC0'))
waldtest(modelce52, modelce5, vcov = vcovHC)

##### c2
load('wage2.RData')

## use KWW as the proxy variable
modelc21 <- lm(lwage ~ educ+exper+tenure+married+south+urban+black + KWW, data)
summary(modelc21)

## use KWW and IQ as the proxy variables
modelc22 <- lm(lwage ~ educ+exper+tenure+married+south+urban+black + KWW+IQ, data)
summary(modelc22)

#### c3
load('jtrain.RData')

modelc31 <- lm(lscrap ~ grant, data = data, subset = (year==1988))
summary(modelc31)

modelc32 <- lm(lscrap ~ grant + l, data = data, subset = (year==1988))
summary(modelc32)

lscrap87 <- subset(data, year==1987)$lscrap
lscrap87 <- na.omit(lscrap87)

modelc33 <- lm(lscrap ~ grant + lscrap87, data = subset(data, year==1988))
summary(modelc33)

## t-test for grant and log(scrap87)
pt(-1.727, 51)
2*pt((0.83116-1)/0.04444, 51)

### heteroskedatic-robust t-test for grant and log(scrap87)
coeftest(modelc33, vcov. = vcovHC(modelc33, 'HC0'))

###### c4
load('infmrt.RData')
modelc41 <- lm(infmort ~ lpcinc+lphysic+lpopul+DC, data, subset = (d90==1))
summary(modelc41)
summary(resid(modelc41))

###### c5 
load('rdchem.RData')

modelc50 <- lm(rdintens ~ I(sales/1000) +I(salessq/1000^2)+profmarg, data)
summary(modelc50)

modelc50d <- lm(rdintens ~ I(sales/1000) +I(salessq/1000^2)+profmarg, data, subset = (sales<30000))
summary(modelc5d)

#### lad
library(quantreg)

modelc51lad <- rq(rdintens ~ I(sales/1000) +I(salessq/1000^2) +profmarg, data = data)
summary(modelc51lad)

modelc51d <- rq(rdintens ~ I(sales/1000) +I(salessq/1000^2) +profmarg, data=data, subset = (sales<30000) )
summary(modelc51d)


###### c6
load('meap93.RData')

modelc61 <- lm(lsalary ~ bensal+lenroll+lstaff, data, subset = (bensal>.01))
summary(modelc61)

modelc62 <- lm(lsalary ~ bensal+lenroll+lstaff+droprate+gradrate, data, subset = (bensal>.01))
summary(modelc62)

###### c7
load('loanapp.RData')

table(data$obrat>40)
modelc7 <- lm(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+
                cosign+chist+pubrec+mortlat1+mortlat2+vr, data, subset = (obrat<=40))
summary(modelc7)

modelc7all <- lm(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+
                cosign+chist+pubrec+mortlat1+mortlat2+vr, data)
summary(modelc7all)


#### c8
load('twoyear.RData')

mean(data$stotal)
sd(data$stotal)

### test the correlation between jc univ and stotal
modelc81 <- lm(univ ~ stotal, data)
summary(modelc81)

modelc82 <- lm(jc ~ stotal, data)
summary(modelc82)

### test beta_1 = beta_2
modelc8s <- lm(lwage ~ jc+I(jc+univ)+exper+stotal, data)
summary(modelc8s)

#### add the square of stotal
modelc8s2 <- lm(lwage ~ jc+I(jc+univ)+exper+stotal+I(stotal^2), data)
summary(modelc8s2)

#### add intercross variable
modelc8s3 <- lm(lwage ~ jc+I(jc+univ)+exper+stotal+jc:stotal+univ:stotal, data)
summary(modelc8s3)

waldtest(modelc8s3, modelc8s)

########## c9
load('401ksubs.RData')

#### ols regression
modelc9ols <- lm(nettfa ~ inc+incsq+age+agesq+male+e401k, data, subset=(fsize==1))
summary(modelc9ols)

## bp test for heteroskedaticity
bptest(modelc9ols)

### lad regression
modelc9lad <- rq(nettfa ~ inc+incsq+age+agesq+male+e401k, data=data, subset=(fsize==1))
summary(modelc9lad)

######## c10
library(foreign)
jtrain2 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/jtrain2.dta")
jtrain3 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/jtrain3.dta")

#### the percentage of training
table(jtrain2$train)[2]/nrow(jtrain2)
table(jtrain3$train)[2]/nrow(jtrain3)

#### regression of re78 to train
modelc10re2 <- lm(re78 ~ train, jtrain2)
summary(modelc10re2)

#### 
modelc101 <- lm(re78 ~ train + re74+re75+educ+age+black+hisp, data=jtrain2)
summary(modelc101)

#### model on jtrain3 dataset
#### regression of re78 to train
modelc10re3 <- lm(re78 ~ train, jtrain3)
summary(modelc10re3)

#### 
modelc102 <- lm(re78 ~ train + re74+re75+educ+age+black+hisp, data=jtrain3)
summary(modelc102)

#### statistics of avgre
summary((jtrain2$re74+jtrain2$re75)/2)
sd((jtrain2$re74+jtrain2$re75)/2)

summary(jtrain3$avgre)
sd(jtrain3$avgre)

summary(jtrain3$re78)
sd(jtrain3$re78)

#### without avgre above 10
modelc10j2 <- lm(re78 ~ train+re74+re75+educ+age+black+hisp, data=jtrain2, subset=((re74+re75)/2<=10))
summary(modelc10j2)

modelc10j3 <- lm(re78 ~ train+re74+re75+educ+age+black+hisp, data=jtrain3, subset=(avgre<=10))
summary(modelc10j3)

#### for jobless in 1974 and 1975
modelc10u2 <- lm(re78 ~ train, data=jtrain2, subset=(unem74==1 & unem75==1))  # on jtrain2
summary(modelc10u2)

modelc10u3 <- lm(re78 ~ train, data=jtrain3, subset=(unem74==1 & unem75==1))  # on jtrain3
summary(modelc10u3)
