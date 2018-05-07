setwd('e:/wooldridge/datasets')
load("ceosal2.RData")
logmodel1 = lm(lsalary~lsales+lmktval+ceoten, data=data)
data4p <- data.frame(lsales = log(5000), lmktval = log(10000), ceoten = 10)
yhat <- predict(logmodel1, data4p)
data$lsalesp <- data$lsales - data4p[1,1]
data$lmktvalp <- data$lmktval - data4p[1,2]
data$ceotenp <- data$ceoten - data4p[1,3]

logmodel2 <- lm(lsalary~lsalesp+lmktvalp+ceotenp, data)


rm(list = ls())
load('htv.RData')
data$pareduc = data$fatheduc + data$motheduc
model41 <- lm(lwage~ educ + educ:pareduc + exper, data)
summary(model41)

model42 <- lm(lwage~ educ + educ*pareduc + exper, data)
summary(model42)

model43 <- lm(lwage~ educ + exper, data)
summary(model43)

load('attend.RData')
data$priGPAsq = data$priGPA^2
data$ACTsq = data$ACT^2
data$atndrtesq = data$atndrte^2
model61 <- lm(stndfnl ~ atndrte + priGPA + ACT + priGPAsq + ACTsq +priGPA:atndrte, data)
summary(model61)

model62 <- lm(stndfnl ~ atndrte + priGPA + ACT + priGPAsq + ACTsq +priGPA:atndrte + atndrtesq + ACT:atndrte, data)
summary(model62)

fvalue = ((0.2315-0.2287)/2)/((1-0.2315)/671)
pf(fvalue, 2, 671, lower.tail = FALSE)

load('kielmc.RData')

data1 = subset(data, year == 1981)
modelc1 = lm(lprice ~ ldist, data1)
summary(modelc1)

modelc2 = lm(lprice ~ ldist + lintst + larea + lland + rooms +  baths + age, data1)
summary(modelc2)

modelc3 = lm(lprice ~ ldist + lintst + larea + lland + rooms +  baths + age + lintstsq, data1)
summary(modelc3)

data1$ldistsq = data1$ldist^2
modelc4 = lm(lprice ~ ldist + lintst + larea + lland + rooms +  baths + age + lintstsq + ldistsq, data1)
summary(modelc4)

rm(list = ls())
load('wage1.RData')
str(data)
modelc21 <- lm(lwage ~ educ + exper + expersq, data)
summary(modelc21)

r5 = (0.0410089-2*0.0007136*4)
r20 = (0.0410089-2*0.0007136*19)

rm(list = ls())
load('wage2.RData')
modelc31 <- lm(lwage ~ educ + exper + educ:exper, data)
modelc32 <- lm(lwage ~ educ, data)

fvaluec3 <- ((.2497-.1858)/2)/((1-.2497)/522)
pf(fvaluec3, 2, 522, lower.tail = FALSE)

data$new <- data$educ*(data$exper-10)
modelc33 <- lm(lwage ~ educ + exper +new, data)
summary(modelc33)

rm(list = ls())
load('gpa2.RData')
str(data)
modelc41 <- lm(sat ~ hsize + hsizesq, data)
summary(modelc41)
19.814/(2*2.131)
data$lsat = log(data$sat)
modelc42 <- lm(lsat ~ hsize + hsizesq, data)
summary(modelc42)
0.0196029/(2*0.0020872)

# ex c5
rm(list = ls())
load('hprice1.RData')

modelc51 <- lm(lprice ~ llotsize +lsqrft + bdrms, data)
newpoints <- data.frame(llotsize=log(20000), lsqrft = log(2500), bdrms=4)

residual <- modelc51$residuals
alpha1 <- sum(exp(residual))/length(residual)
yhat1 <- alpha1 * exp(predict(modelc51, newpoints))

fittedvalue <- modelc51$fitted.values
m <- exp(fittedvalue)
modelc52 <- lm(data$price ~ m - 1)
summary(modelc52) 

m2 <- exp(predict(modelc51))
cor(data$price, m2)^2

alpha2 <- modelc52$coefficients
yhat2 <- alpha2 * exp(predict(modelc51, newpoints))

modelc53 <- lm(price ~ lotsize +sqrft + bdrms, data)
summary(modelc53)

rm(list = ls())
load('vote1.RData')

modelc61 <- lm(voteA ~ prtystrA + expendA * expendB, data)
summary(modelc61)
(-3.172e-02 -300 * 6.629e-06)*100
(3.828e-02 -6.629e-06*100)*100

modelc62 <- lm(voteA ~ prtystrA + expendA + expendB + shareA, data)
summary(modelc62)

rm(list = ls())
load('attend.RData')

-1.63+2*.296*2.59+.0056*82

data$x4 <- (data$priGPA-2.59)^2
data$x5 <- data$ACT^2
data$x6 <- data$priGPA*(data$atndrte-82)
modelc71 <- lm(stndfnl ~ atndrte + priGPA + ACT + x4 + x5 + x6, data)
summary(modelc71)

# ex 8
rm(list = ls())
load('hprice1.RData')

modelc81 <- lm(price ~ lotsize + sqrft + bdrms, data)
summary(modelc81)
predict(modelc81, data.frame(lotsize=10000, sqrft=2300, bdrms=4))

data$x1 = data$lotsize - 10000
data$x2 = data$sqrft - 2300
data$x3 = data$bdrms - 4

modelc82 <- lm(price ~ x1 + x2 + x3, data)
summary(modelc82)

# ex 9
rm(list = ls())
load('nbasal.RData')

modelc91 <- lm(points ~ exper + age + coll + expersq, data)
summary(modelc91)

2.36363/(2*0.07703)

modelc92 <- lm(points ~ exper + age + coll + expersq + agesq, data)
summary(modelc92)

modelc93 <- lm(lwage ~ points + exper + age + coll + expersq, data)
summary(modelc93)

modelc94 <- lm(lwage ~ points + exper + expersq, data)
summary(modelc94)

fvalue = ((0.4878-0.4832)/2)/((1-0.4878)/263)
pf(fvalue, 2, 263, lower.tail = FALSE)

# ex 10
rm(list = ls())
load('bwght2.RData')

#回归第一问中的模型
modelc101 <- lm(lbwght ~ npvis + npvissq, data)
summary(modelc101)

#产前检查对体重的效果转负
0.0189167/(2*0.0004288)

#增加母亲年龄
modelc102 <- lm(lbwght ~ npvis + npvissq + mage + magesq, data)
summary(modelc102)

magemix <- 0.0253920/(2*0.0004119)
sum(data$mage > 30)/length(data$mage)

# choose between log(bwght) and bwght for dependent variable

# delete missing observations in npvis
newdata <- subset(data, !is.na(data$npvis))
modelc103 <- lm(bwght ~ npvis + npvissq + mage + magesq, newdata)
summary(modelc103)

m <- exp(modelc102$fitted.values)
m <- exp(predict(modelc102))
cor(newdata$bwght, m)^2

# ex 11
rm(list = ls())
load('apple.RData')

modelc111<- lm(ecolbs~ecoprc+regprc, data)
summary(modelc111)

modelc112<- lm(ecolbs~ecoprc+regprc+faminc+hhsize+educ+age, data)
summary(modelc112)

fvalue = ((0.04022-0.03641)/4)/((1-0.04022)/653)
pf(fvalue, 4, 653, lower.tail = FALSE)

#ex 12
rm(list = ls())
load('401ksubs.RData')

#choose a subset
newdata <- subset(data, (marr==1) & (fsize==2))

min(newdata$age)
sum(newdata$age==25)

modelc121<- lm(nettfa~inc+age+agesq,newdata)
summary(modelc121)

#age偏效应的统计量
newdata$x3 <- (newdata$age-25)^2
modelc122 <- lm(nettfa~inc+age+x3, newdata)
summary(modelc122)

modelc123 <- lm(nettfa~inc+x3, newdata)
summary(modelc123)

age = seq(25, 65, length.out = 65-25+1)
data4print <- data.frame(inc=rep(50,65-25+1), x3=age^2)
predict4print <- predict(modelc123, data4print)
plot(predict4print~age, type = 'l')

modelc124 <- lm(nettfa~inc+x3 + incsq, newdata)
summary(modelc124)