# Imports 
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("dotwhisker")
library(dotwhisker)
library(sandwich)
library(pscl)
library(arm)
library(AER)
#rm(list=ls()) 

##part 2, ACTUALLY ALL DATA
#Import data 
ltc_raw <- read.csv("./Data/HomePerformanceMetricsCombined.csv", stringsAsFactors = FALSE)
#Add councils
ltc_raw$FamCouncil[ltc_raw$FamCouncil %in% "Yes"] <- 1
ltc_raw$FamCouncil[ltc_raw$FamCouncil %in% "No"] <- 0
ltc_raw$ResCouncil[ltc_raw$ResCouncil %in% "Yes"] <- 1
ltc_raw$ResCouncil[ltc_raw$ResCouncil %in% "No"] <- 0
ltc_raw$Accreditation[ltc_raw$Accreditation %in% "Yes"] <- 1
ltc_raw$Accreditation[ltc_raw$Accreditation %in% "No"] <- 0
ltc_raw$homeType[ltc_raw$homeType %in% "For-Profit"] <- 2
ltc_raw$homeType[ltc_raw$homeType %in% "Non-Profit"] <- 1
ltc_raw$homeType[ltc_raw$homeType %in% "Municipal"] <- 0

#Look 
colnames(ltc_raw)
table(ltc_raw$FamCouncil)
table(ltc_raw$ResCouncil)
table(ltc_raw$homeType)
#mutate(ltc_raw, councilNum = ltc_raw$ResCouncil + ltc_raw$FamCouncil)
ltc_raw <- mutate(ltc_raw, councilNum=as.numeric(ResCouncil)+as.numeric(FamCouncil))

#Get rid of some rows: 
ltc_noNa <- drop_na(ltc_raw)
ltc_noNa <- subset(ltc_noNa, LTC_Home!="royal ottawa place")


#Change new columns to floats
colnames(ltc_noNa)
ltc_noNa$Depression 
str(ltc_noNa)
ltc_noNa <- transform(ltc_noNa, Pain = as.numeric(Pain), 
                      Depression = as.numeric(Depression),
                      PressureUlcers = as.numeric(PressureUlcers),
                      PhysicalRestraintUse = as.numeric(PhysicalRestraintUse),
                      Falls = as.numeric(Falls),
                      AntiPhsychoticMedUse = as.numeric(AntiPhsychoticMedUse)
)
ltc_noNa <- subset(ltc_noNa, LTC_Home!="royal ottawa place")
ltc_noNa <- drop_na(ltc_noNa)

#MAYBE do deaths by cases and bed numbers?

#View all factors and summary
summary(ltc_noNa)
colnames(ltc_noNa)
selected <-  ltc_noNa %>% dplyr::select(Total_LTC_Resident_Deaths, FamCouncil, ResCouncil, Beds, Annual_Non.compliance, Annual_Orders, Targeted_Inspect_Num, Targeted_Orders, Targeted_Non.compliance, councilNum, Accreditation, homeType, Pain, Depression, PressureUlcers, PhysicalRestraintUse, Falls, AntiPhsychoticMedUse)

#Make poisson 
model1 <- glm(Total_LTC_Resident_Deaths ~ Beds + Annual_Non.compliance + Annual_Orders +  Targeted_Inspect_Num +  Targeted_Orders +  Targeted_Non.compliance +  councilNum +  Accreditation +  homeType +  Pain +  Depression +  PressureUlcers + PhysicalRestraintUse + Falls +  AntiPhsychoticMedUse, family="poisson", data = selected)
summary(model1)
with(model1, cbind(res.deviance = deviance, df = df.residual,
                   p = pchisq(deviance, df.residual, lower.tail=FALSE)))
#Residual deviance: 5918.8  on 463  degrees of freedom

#Check model standard error ect. with sandwitch 
cov.model1 <- vcovHC(model1, type="HC0")
std.err <- sqrt(diag(cov.model1))
r.est <- cbind(Estimate= coef(model1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(model1)/std.err), lower.tail=FALSE),
               LL = coef(model1) - 1.96 * std.err,
               UL = coef(model1) + 1.96 * std.err)
r.est
#5918.808 
# residual deviance Residual deviance: 5918.8  on 463  degrees of freedom
# res deviance is greater than df so it has over-dispersion  

#Visually check overdispersoin
## R code
plot(log(fitted(model1)),log((selected$Total_LTC_Resident_Deaths-fitted(model1))^2),xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2),pch=20,col="blue")
abline(0,1) ## 'varianc = mean' line

#Looks like overdispersion
dp = sum(residuals(model1,type ="pearson")^2)/model1$df.residual
dp

dispersiontest(model1)
summary(model1,dispersion = dp)

#Allow dispersion estimate
qpoi_mod = glm(Total_LTC_Resident_Deaths ~ .,family=quasipoisson, selected)
summary(qpoi_mod)

poi_mod2 = glm(Total_LTC_Resident_Deaths ~ Beds +  Targeted_Non.compliance +  councilNum  +  Depression + PhysicalRestraintUse,family=poisson, selected)
qpoi_mod2 = glm(Total_LTC_Resident_Deaths ~ Beds +  Targeted_Non.compliance +  councilNum  +  Depression + PhysicalRestraintUse,family=quasipoisson, selected)
summary(qpoi_mod2)
vuong (poi_mod2, qpoi_mod2)
qpoi_mod2$coefficients  
with(qpoi_mod2, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))



#try zero inflation
model1 <- glm(Total_LTC_Resident_Deaths ~ Beds + Annual_Non.compliance + Annual_Orders +  Targeted_Inspect_Num +  Targeted_Orders +  Targeted_Non.compliance +  councilNum +  Accreditation +  homeType +  Pain +  Depression +  PressureUlcers + PhysicalRestraintUse + Falls +  AntiPhsychoticMedUse, family="poisson", data = selected)
summary(m1)
car::vif(model1)
library(DHARMa)
testDispersion(model1)
simulationOutput <- simulateResiduals(fittedModel = model1, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)
testZeroInflation(simulationOutput)
library(glmmTMB)
fittedModel <- glmmTMB(Total_LTC_Resident_Deaths ~ Beds + Annual_Non.compliance + Annual_Orders +  Targeted_Inspect_Num +  Targeted_Orders +  Targeted_Non.compliance +  councilNum +  Accreditation +  homeType +  Pain +  Depression +  PressureUlcers + PhysicalRestraintUse + Falls +  AntiPhsychoticMedUse, ziformula = ~1 , family = "poisson", data = selected)
summary(fittedModel)

m1 <- zeroinfl(Total_LTC_Resident_Deaths ~  Annual_Non.compliance + Annual_Orders +  Targeted_Inspect_Num +  Targeted_Orders +  Targeted_Non.compliance +  councilNum +  Accreditation +  Pain +  Depression +  PressureUlcers + PhysicalRestraintUse + Falls, data = selected)
summary(m1)

install.packages("countreg", repos = "http://R-Forge.R-project.org")
library("countreg")
m1 <- countreg::zeroinfl(Total_LTC_Resident_Deaths ~ Targeted_Non.compliance +  Pain +  Depression  + Falls, dist = 'poisson', data = selected)
summary(m1)
E2 <- resid(m1, type = "pearson")
N  <- nrow(selected)
p  <- length(coef(m1))  
sum(E2^2) / (N - p)


m4 <- pscl::zeroinfl(Total_LTC_Resident_Deaths ~    Targeted_Orders  +  Accreditation   +  Depression + Falls + offset(log(Beds)) , dist = 'negbin', data = selected)
summary(m4)
E2 <- resid(m4, type = "pearson")
N  <- nrow(selected)
p  <- length(coef(m4))  
sum(E2^2) / (N - p)
rootogram(m4, main = "Zero Inflated", ylim = c(-5, 15), max = 50)
qqrplot(m4, main = "Zero Inflated", ylim = c(-5, 15), max = 50)
(est <- cbind(Estimate = coef(m4), confint(nb_mod)))
exp(est)

inFinalModel <- selected %>%  dplyr::select(Accreditation, Targeted_Non.compliance,  Pain,  PressureUlcers)
m1 <- zeroinfl(Total_LTC_Resident_Deaths ~  Annual_Non.compliance + Annual_Orders +  Targeted_Inspect_Num +  Targeted_Orders +  Targeted_Non.compliance +  councilNum +  Accreditation +  Pain +  Depression +  PressureUlcers + PhysicalRestraintUse + Falls, data = selected)


m5 <- pscl::zeroinfl(Total_LTC_Resident_Deaths ~   Targeted_Non.compliance  +  Accreditation +  Pain  +  PressureUlcers   + offset(log(Beds)) , dist = 'negbin', data = selected)
summary(m5)
E2 <- resid(m5, type = "pearson")
N  <- nrow(selected)
p  <- length(coef(m5))  
library(MASS)
library(vcd)
res <- residuals(m5, type="pearson")
plot(log(predict(m5)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)sum(E2^2) / (N - p)
library(faraway)
halfnorm(residuals(m5))
plot(Total_LTC_Resident_Deaths ~   Targeted_Orders  +  Accreditation +  Pain  +  PressureUlcers   + offset(log(Beds)) , data=selected) 
rootogram(m5, main = "Zero Inflated", ylim = c(-5, 15), max = 50)
qqrplot(m5, main = "Zero Inflated", ylim = c(-5, 15), max = 50)
(est <- cbind(Estimate = coef(m5), confint(m5)))
exp(est)
library("Hmisc")
res2 <- rcorr(as.matrix(inFinalModel))
res2
res3 <- rcorr(as.matrix(check))
res3



yhat <- predict(m5,newdata=selected,
                se.fit=TRUE,MC=2500)

plot(x=newdata$ment,
     y=yhat$yhat,
     xlab="Mentor Articles",
     ylab="Predicted Counts",
     ylim=range(zip$y),
     type="n")
polygon(x=c(newdata$ment,rev(newdata$ment)),
        y=c(yhat$lower,rev(yhat$upper)),
        border=FALSE,
        col=gray(.75))
lines(x=newdata$ment,
      y=yhat$yhat,
      lwd=2)
rug(quantile(bioChemists$ment,c(.05,.50,.95)))
title("Predicted Counts and 95


pchisq(2 * (logLik(m1) - logLik(mnull)), df = 3, lower.tail = FALSE)
m2 <- zeroinfl(Total_LTC_Resident_Deaths ~  Targeted_Orders  +  Accreditation   +  Depression + Falls + offset(log(Beds)), dist = 'poisson', data = selected)
summary(m2)
rootogram(m2, main = "Zero Inflated", ylim = c(-5, 15), max = 50)
qqrplot(m2, main = "Zero Inflated", ylim = c(-5, 15), max = 50)


vuong(m1, nb_mod4)
pchisq(2 * (logLik(m2) - logLik(mnull)), df = 3, lower.tail = FALSE)



#Compare 0 inf
mnull <- update(m1, . ~ 1)
pchisq(2 * (logLik(m1) - logLik(mnull)), df = 3, lower.tail = FALSE)
vuong(model1, m1)
vuong(m1, nb_mod2)


plot(log(fitted(nb_mod2)),log((selected$Total_LTC_Resident_Deaths-fitted(nb_mod2))^2),xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2),pch=20,col="blue")
abline(0,1) 

#Check BN is better 
odTest(nb_mod2) #this means neg binomial is better
mod2 <- glm(Total_LTC_Resident_Deaths ~ Beds + total_orders_nc + councilNum +  Depression , family=poisson, data = selected)
library(AER)
dispersiontest(mod2,trafo=1)

#Try negative binomial model
library(MASS)
nb_mod = glm.nb(Total_LTC_Resident_Deaths ~ Annual_Non.compliance + Annual_Orders +  Targeted_Inspect_Num +  Targeted_Non.compliance +  councilNum +  Accreditation  +  Pain +  Depression +  PressureUlcers + PhysicalRestraintUse + Falls +  AntiPhsychoticMedUse , data = selected)
summary(nb_mod)
(est <- cbind(Estimate = coef(nb_mod), confint(nb_mod)))
exp(est)
selected <- mutate(selected, total_orders_nc=(Annual_Non.compliance+Annual_Orders) + (Targeted_Non.compliance+Targeted_Orders) )

nb_mod = glm.nb(Total_LTC_Resident_Deaths ~ Beds + Annual_Non.compliance + Annual_Orders +  Targeted_Inspect_Num +  Targeted_Orders +  Targeted_Non.compliance +  councilNum +  Accreditation  +  Pain +  Depression +  PressureUlcers + PhysicalRestraintUse + Falls +  AntiPhsychoticMedUse, data = selected)
summary(nb_mod) # + total_orders_nc

nb_mod2 <- glm.nb(Total_LTC_Resident_Deaths ~ Beds + total_orders_nc + councilNum +  Depression , data = selected)
summary(nb_mod2) # + total_orders_nc
with(nb_mod2, cbind(res.deviance = deviance, df = df.residual,
                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))
dp = sum(residuals(nb_mod2,type ="pearson")^2)/nb_mod2$df.residual
dp
plot(log(fitted(nb_mod2)),log((selected$Total_LTC_Resident_Deaths-fitted(nb_mod2))^2),xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2),pch=20,col="blue")
abline(0,1) 
#Check co-linearity
car::vif(nb_mod2)

#Annual orders is important 

#Check co-linearity
selected <- mutate(selected, total_orders=Annual_Orders+Targeted_Orders)
selected <- mutate(selected, total_Non.compliance=Annual_Non.compliance+Targeted_Non.compliance)
selected <- mutate(selected, targeted=Targeted_Orders+Targeted_Non.compliance) 
selected <- mutate(selected, annual=Annual_Orders+Annual_Non.compliance) 
car::vif(nb_mod3)
nb_mod3 <- glm.nb(Total_LTC_Resident_Deaths ~ I(Annual_Non.compliance^10) + I(Annual_Orders^10)  +  I(Targeted_Orders^10) + I(Targeted_Non.compliance^10) + councilNum +  Accreditation   +  Depression + Falls + offset(log(Beds)), data = selected)
summary(nb_mod3)
library(DHARMa)
testDispersion(nb_mod3)
simulationOutput <- simulateResiduals(fittedModel = nb_mod3, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

with(nb_mod2, cbind(res.deviance = deviance, df = df.residual,
                    p = pchisq(deviance, df.residual, lower.tail=FALSE)))

nb_mod4 <- glm.nb(Total_LTC_Resident_Deaths ~  I(Annual_Orders^6) +  I(Targeted_Orders^6) + councilNum +  Accreditation   +  Depression + Falls + offset(log(Beds)), data = selected)
summary(nb_mod4)
simulationOutput <- simulateResiduals(fittedModel = nb_mod4, plot = F)
plot(simulationOutput)
testDispersion(nb_mod4)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
nb_mod4$coefficients
(est <- cbind(Estimate = coef(nb_mod4), confint(nb_mod4)))
exp(est)
plotQQunif(nb_mod4) # left plot in plot.DHARMa()
plotResiduals(nb_mod4) # rig

selected <- (subset(selected, selected$AntiPhsychoticMedUse != 28.436) )  # Apply subset function
selected <- (subset(selected, selected$AntiPhsychoticMedUse != 15.200) )  # Apply subset function

par(mfrow = c(2, 2))
plot(nb_mod4)
plot(nb_mod4, 1)
plot(nb_mod4, 2)
plot(nb_mod4, 3)
plot(nb_mod4, 5)

#Check residuals
library(DHARMa)
testDispersion(nb_mod2)
simulationOutput <- simulateResiduals(fittedModel = nb_mod2, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)
testZeroInflation(simulationOutput)


#MAKE HISTOGRAMS FOR ALL 
inModel <- selected %>% dplyr::select(Total_LTC_Resident_Deaths, Annual_Non.compliance, Annual_Orders +  Targeted_Inspect_Num +  Targeted_Orders, + Targeted_Non.compliance + councilNum +  Accreditation, Pain, Depression, Falls, Beds)
library(Hmisc)
hist.data.frame(inModel)
hist(selected$Total_LTC_Resident_Deaths)
hist(selected$Accreditation)
table(selected$Accreditation)

#Edit selected
selected <- mutate(selected, total_orders=Annual_Orders+Targeted_Orders)
selected <- mutate(selected, total_Non.compliance=Annual_Non.compliance+Targeted_Non.compliance)
selected <- mutate(selected, targeted=(Targeted_Orders) + (Targeted_Non.compliance+Targeted_Orders)*Targeted_Inspect_Num)

###Check for outliers 
max(rstandard(nb_mod2))
min(rstandard(nb_mod2))
selected$standardized.residuals <- rstandard(nb_mod2)
possible.outliers <- subset(selected, standardized.residuals < -1.96 | standardized.residuals > 1.96)
possible.outliers

#Check influential cases
nb_mod2$cooks <- cooks.distance(nb_mod2)
plot(sort(nb_mod2$cooks, decreasing=TRUE))
max(nb_mod2$cooks)



#REferences
https://towardsdatascience.com/adjust-for-overdispersion-in-poisson-regression-4b1f52baa2f1






#try quassi poisson 
model2 <- glm(Total_LTC_Resident_Deaths ~ ., quasipoisson(link = "log"), data = selected)
summary(model2) #Residual deviance: 5918.8  on 463  degrees of freedom
with(model2, cbind(res.deviance = deviance, df = df.residual,
                   p = pchisq(deviance, df.residual, lower.tail=FALSE)))


#compare coefficients 
coef1 = coef(model1)
coef2 = coef(model2)
se.coef1 = se.coef(model1)
se.coef2 = se.coef(model2)
# use 'cbind()' to combine values into one dataframe
cbind(coef1, se.coef1, coef2, se.coef2, exponent = exp(coef1))

#model 3
model3 <- glm(Total_LTC_Resident_Deaths ~ Beds + Annual_Non.compliance +  Targeted_Non.compliance +  councilNum +  Pain +  Depression +  PressureUlcers + PhysicalRestraintUse + Falls +  AntiPhsychoticMedUse, family="poisson", data = selected)
summary(model3) #Residual deviance: 6342.1  on 469  degrees of freedom
with(model3, cbind(res.deviance = deviance, df = df.residual,
                   p = pchisq(deviance, df.residual, lower.tail=FALSE)))



#Plots
nb_mod2 <- glm.nb(Total_LTC_Resident_Deaths ~ Beds  + councilNum   +  Pain +  Depression , data = selected)
summary(nb_mod2)

ggplot(selected, aes(Total_LTC_Resident_Deaths, fill = Beds)) + geom_histogram(binwidth = 1) + facet_grid(Beds ~ ., margins = TRUE, scales = "free")
ggplot(selected, aes(Total_LTC_Resident_Deaths, fill = Depression)) + geom_histogram(binwidth = 1) + facet_grid(Depression ~ ., margins = TRUE, scales = "free")

summary()
with(selected, tapply(Falls, Total_LTC_Resident_Deaths, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

#Check assumptions
m3 <- glm(Total_LTC_Resident_Deaths ~ Beds  + councilNum   +  Pain +  Depression , family="poisson", data = selected)
pchisq(2 * (logLik(nb_mod2) - logLik(m3)), df = 1, lower.tail = FALSE)
dispersiontest(mod2,trafo=1)


#Check residuals:
#install.packages("DHARMa")
