OJ <- read.csv("oj.csv")
library(dplyr)
library(ggplot2)
library(tidyr)
#install.packages('tidyr')
OJ$logp <- log(OJ$price)

regMod <-  lm(logmove ~ logp + (logp * brand) + (brand * feat) + brand + feat + EDUC + INCOME + ETHNIC + AGE60 + HHLARGE, data = OJ)
summary(regMod)

##All the demigraphic factors are significant at the 99.9% confidence level. 

logmove_hat <- predict(regMod)

#########

Fair_R2 <- cor(logmove_hat , OJ$logmove)^2

regMod_old <-  lm(logmove ~ logp + (logp * brand) + (brand * feat) + brand + feat  , data = OJ)
summary(regMod_old)

logmove_hat_old <- predict(regMod_old)
Fair_R2_old <- cor(logmove_hat_old , OJ$logmove)^2

###The Fair R^2 increases by about 3% when the demographic features are added. 

####################splitting the data in training and test
# Split Data into Training and Testing in R 
sample_size = floor(0.8*nrow(OJ))
set.seed(777)

# randomly split data in r
picked = sample(seq_len(nrow(OJ)),size = sample_size)
Training =OJ[picked,]
Test =OJ[-picked,]

######################## MSE of two models############
regDemo <-  lm(logmove ~ logp + (logp * brand) + (brand * feat) + brand + feat + EDUC + INCOME + ETHNIC + HHLARGE +AGE60 , data = Training)
summary(regDemo)

regNoDemo <-  lm(logmove ~ logp + (logp * brand) + (brand * feat) + brand + feat  , data = Training)
summary(regNoDemo)

MSE_training_demo <- mean((regDemo$residuals)^2)
MSE_training_nodemo <- mean((regNoDemo$residuals)^2)

MSE_test_demo <- mean((Test$logmove - predict.lm(regDemo, Test)) ^ 2)
MSE_test_nodemo <- mean((Test$logmove - predict.lm(regNoDemo, Test)) ^ 2)
#The model with the demographics has a lower out of sample MSE implying that it is a better prediction for the true model.

####################################################
##Question 2a.) Summary of EDUC and HHLARGE
summary(OJ$EDUC)
summary(OJ$HHLARGE)

#2b  i 

exp(coef(regDemo)["HHLARGE"]*((summary(OJ$HHLARGE))["3rd Qu."] - summary(OJ$HHLARGE)["Median"]))

#2b ii
exp(coef(regDemo)["EDUC"]*((summary(OJ$EDUC))["3rd Qu."] - summary(OJ$EDUC)["Median"]))

#2c i & ii

regMod_PED <-  lm(logmove ~ logp + (logp * brand) + (brand * feat) + brand + feat + EDUC + INCOME + ETHNIC + AGE60 + HHLARGE + (HHLARGE * logp)+(EDUC * logp), data = OJ)
summary(regMod_PED)

## do the signs make sense?
#Yes they make sense higher proportion of big households results in a much more negative 
#price elasticity because they probably have lower disposable income. 
#Higher EDUC resultsin lower price elasticity since the sign is opposite to the base price elasticity. 
#The reason for this is unclear. 

#2c iii
#EDUC goes from 1.25 to -1.61
#HHLARGE goes from -0.29 to 3.92

#2c iv 

coef(regMod_PED)["logp:HHLARGE"]*((summary(OJ$HHLARGE))["3rd Qu."] - summary(OJ$HHLARGE)["Median"])
coef(regMod_PED)["logp:EDUC"]*((summary(OJ$EDUC))["3rd Qu."] - summary(OJ$EDUC)["Median"])

#####################q3
#3a
Df1 <- OJ
Df1$week <- Df1$week+1

Df2 <- merge(OJ, Df1, by=c("brand","store","week"))

Df2 <- Df2 %>% 
  rename(
    lastweekprice = price.x ,
    lastweeklogp = logp.x,
    lastweekfeat = feat.x 
  )

#3b
multipricemod <- lm(logmove.y ~ lastweekprice + price.y , data = Df2)
summary(multipricemod)

#3c
#coefficient for lastweekprice is 0.47. Therefore, it is relatively inelastic. However, the sign is positive so it is
#positively related to lastweeksprice. 

######BONUS

OJnew <- Df2 %>%  select( brand , store , logp, week) %>% 
  spread(brand, logp) 

Df3 <- Df2 %>% select(brand, store, logmove.x, logmove, week , logp, feat, lastweeklogp, INCOME, EDUC, AGE60, WORKWOM.y, ETHNIC)

OJ2 <- full_join(OJnew , Df3, by = c("store","week")) 


bonusreg <- lm(logmove.y ~ logp.y + (dominicks) + (minute.maid) + (tropicana), data = OJ2)
summary(bonusreg)
