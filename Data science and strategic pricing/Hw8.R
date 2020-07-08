OJ <- read.csv("oj.csv")
#install.packages('dplyr')
#install.packages("ggplot2")
library(dplyr)
library(ggplot2)
library(tidyr)
#install.packages("randomForest")
#install.packages('glmnet')
library("randomForest")
library(glmnet)
OJ$logp <- log(OJ$price)

Df1 <- OJ
Df1$week <- Df1$week+1

Df2 <- merge(OJ, Df1, by=c("brand","store","week"))

Df2 <- Df2 %>% 
  rename(
    lastweekprice = price.x ,
    lastweeklogp = logp.x,
    lastweekfeat = feat.x, 
    logmove= logmove.y, 
    logp= logp.y,
    feat = feat.y , 
    EDUC = EDUC.y, 
    INCOME = INCOME.y, 
    ETHNIC = ETHNIC.y , 
    AGE60 = AGE60.y , 
    HHLARGE = HHLARGE.y
  )

tropicana <- Df2 %>% filter(brand == "tropicana")
minutemaid <- Df2 %>% filter(brand == "minute.maid")
dominicks <- Df2 %>% filter(brand == "dominicks")

oj.rftrop1 <- randomForest(logmove ~ logp  + 
                        lastweeklogp + lastweekfeat + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                         feat + EDUC + INCOME + ETHNIC +
                        HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = tropicana, ntree = 	100, keep.forest = TRUE) 
oj.rftrop2 <- randomForest(logp ~ lastweeklogp + lastweekfeat + 
                         feat + EDUC + INCOME + ETHNIC +
                        HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = tropicana, ntree = 	100, keep.forest = TRUE) 

oj.rfmm1 <- randomForest(logmove ~ logp  + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                        lastweeklogp + lastweekfeat +  (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                         feat + EDUC + INCOME + ETHNIC +
                        HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = minutemaid, ntree = 	100, keep.forest = TRUE) 
oj.rfmm2 <- randomForest(logp ~ lastweeklogp + lastweekfeat + 
                         feat + EDUC + INCOME + ETHNIC +(lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                        HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = minutemaid, ntree = 	100, keep.forest = TRUE) 
oj.rfdom1 <- randomForest(logmove ~ logp  + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                        lastweeklogp + lastweekfeat 
                         + feat + EDUC + INCOME + ETHNIC +
                        HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = dominicks, ntree = 	100, keep.forest = TRUE) 
oj.rfdom2 <- randomForest(logp ~ lastweeklogp + lastweekfeat + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                         feat + EDUC + INCOME + ETHNIC +
                        HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = dominicks, ntree = 	100, keep.forest = TRUE) 

residuals <- list(predict(oj.rftrop1, tropicana) - tropicana$logmove ,predict(oj.rftrop2, tropicana) - tropicana$logp, predict(oj.rfmm1, minutemaid) - minutemaid$logmove,
               predict(oj.rfmm2, minutemaid) - minutemaid$logp, predict(oj.rfdom1, dominicks) - dominicks$logmove, predict(oj.rfdom2, dominicks) - dominicks$logp)

myDf <- as.data.frame(residuals)

names(myDf)[1] <- "tropres1"
names(myDf)[2] <- "tropres2"
names(myDf)[3] <- "mmres1"
names(myDf)[4] <- "mmres2"
names(myDf)[5] <- "domres1"
names(myDf)[6] <- "domres2"

tropOLS <- lm(tropres1 ~ tropres2 + mmres2+ domres2, data = myDf)
domOLS <- lm(domres1 ~ tropres2 + mmres2+ domres2, data = myDf)
mmOLS <- lm(mmres1 ~ tropres2 + mmres2+ domres2, data = myDf)


summary(tropOLS)
summary(mmOLS)
summary(domOLS)

elasticities_mat <- matrix(1:9, nrow = 3)
rownames(elasticities_mat) <- c("tropicana","minutemaid","dominicks")
colnames(elasticities_mat) <- c("PED_tropicana", "PED_MinuteMaid", "PED_dominicks")

elasticities_mat[1,1] <- coef(tropOLS)["tropres2"]
elasticities_mat[1,2] <- coef(tropOLS)["mmres2"]
elasticities_mat[1,3] <- coef(tropOLS)["domres2"]
elasticities_mat[2,1] <- coef(mmOLS)["tropres2"] 
elasticities_mat[2,2] <- coef(mmOLS)["mmres2"] 
elasticities_mat[2,3] <- coef(mmOLS)["domres2"]
elasticities_mat[3,1] <- coef(domOLS)["tropres2"] 
elasticities_mat[3,2] <- coef(domOLS)["mmres2"] 
elasticities_mat[3,3] <- coef(domOLS)["domres2"] 

elasticities_mat
