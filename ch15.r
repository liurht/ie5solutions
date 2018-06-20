library(AER);library(stargazer);library(dynlm);library(foreign)

## 计算题1
load('e:/wooldridge/ie5solutions/datasets/wage2.RData')
wage2= data

ols.mod <- lm(lwage ~ sibs, data=wage2)
iv.mod <- ivreg(lwage ~ educ | sibs, data = wage2)

stargazer(ols.mod, iv.mod, type = 'text')

summary(lm(educ ~ brthord, data=wage2))

summary(ivreg(lwage ~ educ|brthord, data=wage2))

educ.test <- lm(educ ~ sibs + brthord, data=wage2)

summary(ivreg(lwage ~ educ + sibs | brthord + sibs, data = wage2))

summary(lm(fitted(educ.test) ~ sibs, data = subset(wage2,!is.na(brthord))))

length(fitted(educ.test))

## 计算题2
load('e:/wooldridge/ie5solutions/datasets/fertil2.RData')
fertil2  <- data

ols.mod <- lm(children ~ educ + age + agesq, data = fertil2)
summary(ols.mod)

summary(lm(educ ~ frsthalf +age+agesq, data = fertil2))

iv.mod <- ivreg(children ~ educ + age + agesq | frsthalf + age + agesq, data = fertil2)

stargazer(ols.mod, iv.mod, type = 'text')

ols.mod2 <- lm(children ~ educ + age + agesq + electric + tv + bicycle, data = fertil2)
iv.mod2 <- ivreg(children ~ educ + age + agesq + electric + tv + bicycle | 
                  frsthalf + age + agesq + electric + tv + bicycle, data = fertil2)
stargazer(ols.mod2, iv.mod2, type = 'text')

## 计算机题3
load('e:/wooldridge/ie5solutions/datasets/card.RData')
card <- data

summary(lm(IQ ~ nearc4, data = card))
summary(lm(IQ ~ nearc4+smsa66+reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669, data = card))

## 
load('e:/wooldridge/ie5solutions/datasets/intdef.RData')
intdef <- data

intdef.ts <- ts(intdef, start = 1948)
summary(lm(i3 ~ inf, data = intdef[-1,]))
# summary(ivreg(i3 ~ inf | lag(inf, -1), na.action = na.omit, data = intdef.ts))
# summary(ivreg(i3 ~ inf | inf_1, na.action = na.omit, data = intdef.ts))

summary(lm(i3 ~ lag(inf, -1), data = intdef.ts)) # not working
summary(dynlm(i3 ~ inf | lag(inf, -1), data = intdef.ts))
summary(dynlm(i3 ~ inf | L(inf, 1), data = intdef.ts))
summary(ivreg(i3 ~ inf | inf_1, data = intdef))
summary(lm(diff(i3) ~ diff(inf), data = intdef.ts))

## 计算机题5
load('e:/wooldridge/ie5solutions/datasets/card.RData')
card <- data

## 内生性检验
end.test <- lm(educ ~ exper+expersq+black+smsa+south+nearc4, data = card)
summary(lm(lwage~educ+exper+expersq+black+smsa+south+resid(end.test), data = card))

## 添加工具变量nearc2
iv.mod <- ivreg(lwage ~ educ+exper+expersq+black+smsa+south | 
                nearc2+nearc4 +exper+expersq+black+smsa+south, data = card)

## 过度识别约束检验
oi.test <- lm(resid(iv.mod) ~ nearc2+nearc4 +exper+expersq+black+smsa+south, data = card)
1-pchisq(summary(oi.test)$r.squared*nobs(iv.mod),1) #无法拒绝原假设，没有证据证明工具变量是内生的

## 计算机题5
load('e:/wooldridge/ie5solutions/datasets/murder.RData')
murder <- data

data.y93 <- subset(murder, year==93)
data.y93$state[which.max(data.y93$exec)]
sum(data.y93$exec>0)

## 混合回归（90和93年数据）
summary(lm(mrdrte ~ d93+exec+unem, data = murder, subset = (year != 87)))

## 基于90和93年的差分数据进行回归（ols）
summary(lm(cmrdrte ~ cexec+cunem, data=data.y93))

## 检验工具变量cexec_1的有效性
summary(lm(cexec ~ cexec_1, data = murder))  # 具有相关性
summary(lm(cexec ~ cexec_1, data = data.y93))

## iv回归
summary(ivreg(cmrdrte ~ cexec+cunem | cexec_1+cunem, data = murder))

## 利用plm包
library(plm)
murder.p <- pdata.frame(murder, c('state', 'year'))
summary(plm(mrdrte ~ exec+unem, data = murder.p, model = 'fd'))

## 计算题7
load('e:/wooldridge/ie5solutions/datasets/phillips.RData')
phillips <- data

## 附加预期的菲利普曲线
summary(lm(cinf ~ unem, data = phillips))

## 检查与unem滞后项的相关性
summary(lm(unem ~ unem_1, data = phillips))
phillips.ts <- ts(phillips, start = 1948)

summary(dynlm(unem ~ L(unem, 1), data = phillips.ts))

## iv回归
summary(dynlm(cinf ~ unem | L(unem, 1), data = phillips.ts))
summary(ivreg(cinf ~ unem | unem_1, data = phillips))

## 计算题8
load('e:/wooldridge/ie5solutions/datasets/401ksubs.RData')
ksubs401 <- data

summary(lm(pira ~ p401k+inc+incsq+age+agesq, data = ksubs401))

## 工具变量有效性检验
e401k.test <- lm(p401k ~ inc+incsq+age+agesq+e401k, data = ksubs401)
coeftest(e401k.test, vcov. = vcovHC(e401k.test, type='HC0'))

## iv估计法
iv.mod <- ivreg(pira ~ p401k+inc+incsq+age+agesq|e401k+inc+incsq+age+agesq, data = ksubs401)
coeftest(iv.mod, vcov. = vcovHC(iv.mod, type='HC0'))

## p401k内生性检验
p401k.rd <- lm(p401k ~ inc+incsq+age+agesq+e401k, data = ksubs401)
v <- resid(p401k.rd)
p401k.st <-lm(pira ~ p401k+inc+incsq+age+agesq+v, data = ksubs401)
coeftest(p401k.st, vcov. = vcovHC(p401k.st, type = 'HC0'))

## 计算题9
load('e:/wooldridge/ie5solutions/datasets/wage2.RData')
wage2= data

## 2sls估计法
summary(aut.2sls <- ivreg(lwage~educ+exper+tenure+black|sibs+exper+tenure+black, 
              data = wage2))

## 手动2sls估计法
stage1 <- lm(educ ~ sibs+exper+tenure+black, data = wage2)
educ.hat <- fitted(stage1)

man.2sls <- lm(lwage ~ educ.hat+exper+tenure+black, data = wage2)

## 错误的2sls估计法
stage1.wrong <- lm(educ ~ sibs, data = wage2)
educ.hat.wrong <- fitted(stage1.wrong)

man.2sls.wrong <- lm(lwage ~ educ.hat.wrong+exper+tenure+black, data = wage2)

## 计算题10
# load('e:/wooldridge/ie5solutions/datasets/labsup.RData')
# labsup = data
# labsup <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/labsup.dta")
labsup <- read.dta('e:/wooldridge/ie5solutions/datasets/Wooldridge_5th_stata_data/labsup.dta')

## 生育两个孩子的妇女占比
table(labsup$kidcount)[1]/nrow(labsup)

## ols估计法
summary(ols.mod <- lm(hours ~ kidcount+educ+age+I(age^2)+nonmomi, data=labsup))

## 工具变量识别
summary(iv.test.full <- lm(kidcount ~ multi2nd+samesex+educ+age+I(age^2)+nonmomi, data=labsup))
summary(iv.test.red <- lm(kidcount ~ educ+age+I(age^2)+nonmomi, data=labsup))

fvalue <- ((0.1238-0.1172)/2)/((1-0.1238)/31850)
1 - pf(fvalue, 2, 31850)

## iv估计法
summary(iv.mod <- ivreg(hours ~ kidcount+educ+age+I(age^2)+nonmomi|multi2nd+samesex+
                educ+age+I(age^2)+nonmomi, data=labsup))
