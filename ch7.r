setwd('e:/wooldridge/datasets')

# eg 1
rm(list = ls())
load('wage1.RData')

modeleg10 <- lm(wage~female+educ, data)
summary(modeleg10)

fdata <- subset(data, female==1)
mdata <- subset(data, female==0)

modeleg11 <- lm(wage~educ, fdata)
summary(modeleg11)
modeleg12 <- lm(wage~educ, mdata)
summary(modeleg12)

modeleg13 <- lm(wage~ female +educ+exper +tenure, data)
summary(modeleg13)

load('mroz.RData')
modeleg75 <- lm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6, data)
summary(modeleg75)
modeleg752 <- glm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6,family=binomial(link='logit'),data)
summary(modeleg752)
summary(modeleg752$fitted.values)

#ex c1
rm(list=ls())
load('gpa1.RData')
modelc10 <- lm(colGPA ~ PC+hsGPA+ACT, data)
summary(modelc10)

modelc11 <- lm(colGPA ~ PC+hsGPA+ACT+mothcoll+fathcoll, data)
summary(modelc11)

# F-test for mothcoll and fathcoll
fvalue <- ((0.2222-0.2194)/2)/((1-0.2222)/135)
pf(fvalue, 2, 135, lower.tail = FALSE)

data$hsGPAsq <- data$hsGPA^2
modelc12 <- lm(colGPA ~ PC+hsGPA+ACT+mothcoll+fathcoll+hsGPAsq, data)
summary(modelc12)

#ex c2
rm(list = ls())
load('wage2.RData')

modelc21 <- lm(lwage ~ educ+exper+tenure+married+black+south+urban, data)
summary(modelc21)

# add tenure^2 and exper^2 in the model
data$expersq <- data$exper^2
data$tenuresq <- data$tenure^2
modelc22 <- lm(lwage ~ educ+exper+expersq+tenure+tenuresq+married+black+south+urban, data)
summary(modelc22)

## F-test for two squared items 
fvalue <- ((0.255-0.2526)/2)/((1-0.255)/925)
pf(fvalue, 2, 925, lower.tail = FALSE)

#test whether the return of education depends on race
modelc23 <- lm(lwage ~ educ+exper+tenure+married+black+south+urban + educ:black, data)
summary(modelc23)

#the estimated wage difference between married blacks and married nonblacks
modelc24 <- lm(lwage ~ educ+exper+tenure+married+black+south+urban +married:black, data)
summary(modelc24)

0.061354+0.188915-0.240820-0.188915

# ex c3
rm(list=ls())
load('mlb1.RData')

## earn difference of cathers vs outfielder (H0: \beta_13 =0)
modelc31 <- lm(lsalary ~ years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar+frstbase+scndbase+thrdbase+shrtstop+catcher, data)
summary(modelc31)

## test if any earn difference between difference position
modelc32 <- lm(lsalary ~ years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar, data)
summary(modelc32)

fvalue <- ((0.6535-0.6445)/5)/((1-0.6535)/339)
pf(fvalue, 5, 339, lower.tail = FALSE)

# ex c4
rm(list = ls())
load('gpa2.RData')

## fit the base model
modelc41 <- lm(colgpa ~ hsize+hsizesq+hsperc+sat+female+athlete, data)
summary(modelc41)

## drop sat
modelc42 <- lm(colgpa ~ hsize+hsizesq+hsperc+female+athlete, data)
summary(modelc42)

## difference between female athlete vs male athlete
modelc43 <- lm(colgpa ~ hsize+hsizesq+hsperc+sat+female+athlete+female:athlete, data)
summary(modelc43)

data$fath <- data$female*data$athlete
data$math <- (1-data$female)*data$athlete
data$fnath <- data$female*(1-data$athlete)
data$mnath <- (1-data$female)*(1-data$athlete)

modelc44 <- lm(colgpa ~ hsize+hsizesq+hsperc+sat+math+fnath+mnath, data)
summary(modelc44)

## test if the effect of sat depends on gender
modelc45 <- lm(colgpa ~ hsize+hsizesq+hsperc+sat+female+athlete+sat:female, data)
summary(modelc45)

# ex c5
rm(list=ls())
load('ceosal1.RData')

##define dummy variable rosneg
data$rosneg <- ifelse(data$ros<0, 1, 0)
modelc51 <- lm(lsalary ~ lsales+roe+rosneg, data)
summary(modelc51)

# ex c6
rm(list = ls())
load('sleep75.RData')

##fit the base model
modelc61 <- lm(sleep ~ totwrk+educ+age+agesq+yngkid, data=subset(data, male==1))
summary(modelc61)
modelc62 <- lm(sleep ~ totwrk+educ+age+agesq+yngkid, data=subset(data, male==0))
summary(modelc62)

## Chow test
modelc63 <- lm(sleep ~ totwrk+educ+age+agesq+yngkid, data)
summary(modelc63)

# m1 <- mean(modelc61$fitted.values)
# ssr1 <- sum((modelc61$fitted.values-m1)^2)
# m2 <- mean(modelc62$fitted.values)
# ssr2 <- sum((modelc62$fitted.values-m2)^2)
# m3 <- mean(modelc63$fitted.values)
# ssrp <- sum((modelc63$fitted.values-m3)^2)

ssr1 <- sum(modelc61$residuals^2)
ssr2 <- sum(modelc62$residuals^2)
ssrp <- sum(modelc63$residuals^2)

fvalue_chow <- (ssrp-ssr1-ssr2)/(ssr1+ssr2)*(706-2*6)/6
pf(fvalue_chow, 6, 694, lower.tail = FALSE)

## F-test
modelc64 <- lm(sleep ~ totwrk+educ+age+agesq+yngkid+male+male:totwrk+male:educ+male:age+male:agesq+male:yngkid, data)
summary(modelc64)

modelc65 <- lm(sleep ~ totwrk+educ+age+agesq+yngkid, data)
summary(modelc65)

fvalue <- ((0.1306-0.1147)/6)/((1-0.1306)/694)
pf(fvalue,6, 694, lower.tail = FALSE)

## with different intercept, test the interaction items
modelc66 <- lm(sleep ~ totwrk+educ+age+agesq+yngkid+male, data)
summary(modelc66)

fvalue2 <- ((0.1306-0.1228)/5)/((1-.1306)/694)
pf(fvalue2, 5, 694, lower.tail = FALSE)

## THE final model should be modelc66

# ex c7
rm(list=ls())
load('wage1.RData')

## estimate the equation with (educ-12.5)
data$educdm <- data$educ-12.5

modelc71 <- lm(lwage ~ female+educ+female:educdm+exper+expersq+tenure+tenursq, data)
summary(modelc71)

# ex c8
rm(list=ls())
load('loanapp.RData')

modelc81 <- lm(approve ~ white, data)
summary(modelc81)

## control other variables
modelc82 <- lm(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+
                 cosign+chist+pubrec+mortlat1+mortlat2+vr, data)
summary(modelc82)

modelc83 <- lm(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+
                 cosign+chist+pubrec+mortlat1+mortlat2+vr +white:obrat, data)
summary(modelc83)

## effect when obrat=32
data$obratdm <- data$obrat-32
modelc84 <- lm(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+
                 cosign+chist+pubrec+mortlat1+mortlat2+vr +white:obratdm, data)
summary(modelc84)

# ex c9
rm(list=ls())
load('401ksubs.RData')

modelc91 <- lm(e401k ~ inc+incsq+age+agesq+male, data)
summary(modelc91)

data$e401kp <- ifelse(modelc91$fitted.values>=.5, 1, 0)

modelc92 <- lm(e401k ~ inc+incsq+age+agesq+male+pira, data)
summary(modelc92)

# ex c10
rm(list = ls())
load('nbasal.RData')

modelc101 <- lm(points ~ exper+expersq+guard+forward, data)
summary(modelc101)

modelc102 <- lm(points ~ exper+expersq+guard+forward +marr, data)
summary(modelc102)

modelc103 <- lm(points ~ exper+expersq+guard+forward +marr +
                  marr:exper+marr:expersq, data)
summary(modelc103)

fvalue = ((0.1058-0.09098)/3)/((1-0.1058)/261)
pf(fvalue, 3, 261, lower.tail = FALSE)

modelc104 <- lm(assists ~ exper+expersq+guard+forward +marr +
                  marr:exper+marr:expersq, data)
summary(modelc104)

modelc105 <- lm(assists ~ exper+expersq+guard+forward +marr, data)
summary(modelc105)

# ex c11
rm(list = ls())
load('401ksubs.RData')

summary(data$nettfa)
sd(data$nettfa)

modelc111 <- lm(nettfa ~ e401k, data)
summary(modelc111)

modelc112 <- lm(nettfa ~ inc+incsq+age+agesq+e401k, data)
summary(modelc112)

data$agedm <- data$age-41
data$agedmsq <- data$agedm^2

modelc113 <- lm(nettfa ~ inc+incsq+e401k*agedm+e401k:agedmsq, data)
summary(modelc113)

# create dummy variables for fsize
data$fsize1 = ifelse(data$fsize==1, 1, 0)
data$fsize2 = ifelse(data$fsize==2, 1, 0)
data$fsize3 = ifelse(data$fsize==3, 1, 0)
data$fsize4 = ifelse(data$fsize==4, 1, 0)
data$fsize5 = ifelse(data$fsize>=5, 1, 0)

modelc114 <- lm(nettfa ~ inc+incsq+e401k+age+agesq+
                  fsize1+fsize2+fsize3+fsize4, data)
summary(modelc114)

modelc1151 <- lm(nettfa ~ inc+incsq+e401k+age+agesq, data=subset(data, fsize==1))
ssr1 <- sum(modelc1151$residuals^2)
modelc1152 <- lm(nettfa ~ inc+incsq+e401k+age+agesq, data=subset(data, fsize==2))
ssr2 <- sum(modelc1152$residuals^2)
modelc1153 <- lm(nettfa ~ inc+incsq+e401k+age+agesq, data=subset(data, fsize==3))
ssr3 <- sum(modelc1153$residuals^2)
modelc1154 <- lm(nettfa ~ inc+incsq+e401k+age+agesq, data=subset(data, fsize==4))
ssr4 <- sum(modelc1154$residuals^2)
modelc1155 <- lm(nettfa ~ inc+incsq+e401k+age+agesq, data=subset(data, fsize>=5))
ssr5 <- sum(modelc1155$residuals^2)

modelc1156 <- lm(nettfa ~ inc+incsq+e401k+age+agesq, data)
ssrp2 <- sum(modelc1156$residuals^2)

ssrur <- ssr1+ssr2+ssr3+ssr4+ssr5
ssrp <- sum(modelc112$residuals^2)
n <- nrow(data)
k <- 5

fvalue <- (ssrp-ssrur)/ssrur*((n-5*(k+1))/20)
pf(fvalue, k+1, n-5*(k+1),lower.tail = FALSE)

# ex c12
rm(list = ls())
load('beauty.RData')

sum(subset(data, belavg==1)$female==1)
sum(subset(data, belavg==1)$female==0)
sum(data$belavg==1)
sum(data$abvavg==1)

modelc121 <- lm(abvavg ~ female, data)
summary(modelc121)

modelc122 <- lm(lwage ~ belavg + abvavg, data=subset(data, female==0)) # for man
modelc123 <- lm(lwage ~ belavg + abvavg, data=subset(data, female==1)) # for feman

summary(modelc122)
summary(modelc123)

modelc124 <- lm(lwage ~ belavg + abvavg +
                  educ+exper+expersq+union+goodhlth+black+married+south+bigcity+smllcity+service, data=subset(data, female==0)) # for man
modelc125 <- lm(lwage ~ belavg + abvavg +
                  educ+exper+expersq+union+goodhlth+black+married+south+bigcity+smllcity+service, data=subset(data, female==1)) # for feman
summary(modelc124)
summary(modelc125)

##chow test for wage regression model between male and female
ssr1 <- sum(modelc124$residuals^2)
ssr2 <- sum(modelc125$residuals^2)
ssrur <- ssr1+ssr2

modelc126 <- lm(lwage ~ belavg + abvavg +
                  educ+exper+expersq+union+goodhlth+black+married+south+bigcity+smllcity+service + female, data)

ssrp <- sum(modelc126$residuals^2)

n <- nrow(data)
k <- 13

fvalue <- (ssrp-ssrur)/ssrur*((n-2*(k+1))/k)
pf(fvalue, k, n-2*(k+1), lower.tail = FALSE)

# ex c13
rm(list = ls())
load('apple.RData')

data$ecobuy <- ifelse(data$ecolbs>0, 1, 0)
100*sum(data$ecobuy)/nrow(data)

modelc131 <- lm(ecobuy ~ ecoprc+regprc+faminc+hhsize+educ+age, data)
summary(modelc131)

#test the joint significance of nonprice variables
modelc132 <- lm(ecobuy ~ ecoprc+regprc, data)
summary(modelc132) 

#f test
fvalue <- ((0.1098-0.08568)/4)/((1-0.1098)/653)
pf(fvalue, 4, 653, lower.tail = FALSE)

## replace faminc with log(faminc)
modelc133 <- lm(ecobuy ~ ecoprc+regprc+log(faminc)+hhsize+educ+age, data)
summary(modelc133)

sum(modelc133$fitted.values<0)
sum(modelc133$fitted.values>1)
data$ecobuyp <- ifelse(modelc133$fitted.values>=0.5, 1, 0)

table(data$ecobuy-data$ecobuyp)
table(subset(data, ecobuy==0)$ecobuyp)
table(subset(data, ecobuy==1)$ecobuyp)
