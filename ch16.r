library(foreign);library(AER);library(plm);library(dynlm)

## 例16.6
openness <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/openness.dta")

## iv估计法
summary(ivreg(inf ~ open+lpcinc | lpcinc+lland, data = openness))
## ols估计法
summary(lm(inf ~ open+lpcinc, data = openness))

## 思考题16.3
summary(open.lm <- lm(open ~ lpcinc+lland, data = openness))
summary(lm(inf ~ open+lpcinc+resid(open.lm), data = openness))

## 计算题1
smoke <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/smoke.dta")
summary(ols.mod <- lm(lincome ~ cigs+educ+age+agesq, data = smoke))

##  检验收入方程能否被识别
summary(cig.red <- lm(cigs ~ educ+age+agesq+lcigpric+restaurn, data = smoke))
summary(lm(cigs ~ educ+age+agesq, data = smoke))

fvalue <- ((0.051-0.04358)/2)/((1-0.051)/801)
pf(fvalue, 2, 801, lower.tail = F)

## iv估计法
summary(iv.mod <- ivreg(lincome ~ cigs+educ+age+agesq|lcigpric+restaurn+educ+
                          age+agesq, data = smoke))

## 计算题2
#mroz <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta")
load('e:/wooldridge/ie5solutions/datasets/mroz.RData')
mroz <- subset(data,!is.na(wage))


summary(ivreg(I(log(hours))~lwage+educ+age+kidslt6+nwifeinc|educ+age+kidslt6+nwifeinc+
                exper+expersq, data = mroz))

## 根据（16.24）计算弹性
1639.56/mean(mroz$hours)

## lwage educ均为内生变量时iv估计
summary(iv.mod <- ivreg(I(log(hours))~lwage+educ+age+kidslt6+nwifeinc|age+kidslt6+nwifeinc+
                exper+expersq+motheduc+fatheduc, data = mroz))

iv.aux <- summary(lm(resid(iv.mod) ~ age+kidslt6+nwifeinc+
             exper+expersq+motheduc+fatheduc, data = mroz))

pchisq(iv.aux$r.squared*nobs(iv.mod),2, lower.tail = F)

## 计算题3
openness <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/openness.dta")

## 去掉lpcinc
summary(ivreg(inf ~ open|lland, data = openness))

## 比较land lland作为工具变量的有效性
summary(lm(open ~ lland, data = openness))
summary(lm(open ~ land, data = openness))
summary(lm(open ~ lland + land, data = openness))

##增加外生变量oil
summary(ivreg(inf ~ open+lpcinc+oil|lland+lpcinc+oil, data = openness))

## 计算题4
consump <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/consump.dta")

## 过度约束识别
summary(iv.mod <- ivreg(gc ~ gy+r3 | gc_1+gy_1+r3_1, data = consump))
iv.aux <- summary(lm(resid(iv.mod) ~ gc_1+gy_1+r3_1, data = consump[-(1:2),]))
pchisq(iv.aux$r.squared*nobs(iv.mod), 1, lower.tail = F)

## 利用二阶滞后项作为工具变量
summary(ivreg(gc ~ gy+r3 | gc_2+gy_2+r3_2, data = consump))

## 检查工具变量的相关xing
summary(lm(gy ~ gc_2+gy_2+r3_2, data = consump))

## 计算题5
##数据来自网络，样本期为1964-2011
consump2011 <- read.csv('e:/wooldridge/ie5solutions/datasets/consump2011.csv',
                        header = T)  
consump2011.ts <- ts(consump2011, start = 1964)

summary(lm(gc ~ gy+r3, data=consump2011))
summary(iv.mod <- dynlm(gc ~ gy+r3|L(gc)+L(gy)+L(r3), data = consump2011.ts))

u_1 <- c(NA, NA, resid(iv.mod)[-length(resid(iv.mod))])
summary(dynlm(gc ~ gy+r3+ u_1|L(gc)+L(gy)+L(r3)+u_1, data = consump2011.ts))

## 计算题6
cement <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/cement.dta")

## ols估计
summary(lm(gprc ~ gcem+grprcpet+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec,
           data = cement))

##检查工具变量的相关性
summary(lm(gcem ~ rdefs+grprcpet+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec,
           data = cement))  #rdefs作为工具变量

summary(lm(gcem ~ grres+grnon+grprcpet+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec,
           data = cement))  #grres grnon作为工具变量
fvalue <- ((0.8722-0.8576)/2)/((1-0.8722)/294)
pf(fvalue, 2, 294, lower.tail = F)

## iv估计法
summary(ivreg(gprc ~ gcem+grprcpet+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec|
              grres+grnon+grprcpet+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec,
              data = cement))

##计算题7
crime4 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/crime4.dta")

summary(lm(clpolpc ~ cltaxpc+d83+d84+d85+d86+d87+
             clprbarr+clprbcon+clprbpri+clavgsen, data = crime4))

##计算题8
fish <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/fish.dta")  #此处书中的数据集错写为crime4

## 检查wave2 wave3作为工具变量的相关性
summary(lm(lavgprc ~ mon+tues+wed+thurs+wave2+wave3, data = fish))
summary(lm(lavgprc ~ mon+tues+wed+thurs, data = fish))

fvalue <- ((0.3041-0.008755)/2)/((1-0.3041)/90)
pf(fvalue, 2, 90, lower.tail = F)

## iv估计法
summary(iv.mod <- ivreg(ltotqty ~ mon+tues+wed+thurs+lavgprc|mon+tues+wed+thurs+wave2+wave3,
              data = fish))

## 95%置信区间
confint(iv.mod)

u_1 <- c(NA, resid(iv.mod)[-length(resid(iv.mod))])

## 检验残差是否存在序列相关
summary(iv.mod <- ivreg(ltotqty ~ mon+tues+wed+thurs+lavgprc+u_1|mon+tues+wed+thurs+
                          wave2+wave3+u_1, data = fish))

##尝试估计供给函数
summary(ivreg(ltotqty ~ wave2+wave3+lavgprc|wave2+wave3+mon+tues+wed+thurs, data = fish))

##计算题9
airfare <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/airfare.dta")
data1997 <- subset(airfare, year==1997)

## ols估计法
summary(lm(lpassen ~ lfare+ldist+ldistsq, data = data1997))

## 工具变量检验
summary(lm(lfare ~ concen+ldist+ldistsq, data = data1997))

## iv估计法
summary(ivreg(lpassen ~ lfare+ldist+ldistsq|concen+ldist+ldistsq, data = data1997))

exp(2.17567/(2*0.18703)) ## 日乘客量最少

##计算题10
prison <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/prison.dta")
prison.p <- pdata.frame(prison, c('state', 'year'))

## ols估计法
# summary(plm(lcriv ~ lpris+I(log(polpc))+I(log(incpc))+unem+black+metro+ag0_14+ag15_17+ag18_24+ag25_34, data = prison.p, model = 'fd'))

summary(lm(gcriv ~ gpris+gpolpc+gincpc+cunem+cblack+cmetro+cag0_14+cag15_17+cag18_24+
             +cag25_34, data = prison))
## 2sls估计法
summary(iv.mod <- ivreg(gcriv ~ gpris+gpolpc+gincpc+cunem+cblack+cmetro+cag0_14+cag15_17+cag18_24+
                +cag25_34|gpolpc+gincpc+cunem+cblack+cmetro+cag0_14+cag15_17+cag18_24+
                +cag25_34+final1+final2, data = prison))

## \Delta log(prison)约简型方程
summary(red.mod <- lm(gpris ~ gpolpc+gincpc+cunem+cblack+cmetro+cag0_14+cag15_17+cag18_24+
                        +cag25_34+final1+final2, data = prison))
## 内生性检验
summary(lm(gcriv ~ gpris+gpolpc+gincpc+cunem+cblack+cmetro+cag0_14+cag15_17+cag18_24+
             +cag25_34 + resid(red.mod), data = prison))

## 过度约束识别
summary(lm(resid(iv.mod) ~ gpolpc+gincpc+cunem+cblack+cmetro+cag0_14+cag15_17+cag18_24+
          +cag25_34+final1+final2, data = prison))

pchisq(0.000184*nobs(iv.mod), 1, lower.tail = F)
