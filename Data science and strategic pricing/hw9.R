OJ <- read.csv("oj.csv")
#install.packages('dplyr')
#install.packages("ggplot2")
library(dplyr)
library(ggplot2)
library(tidyr)
#install.packages("randomForest")
#install.packages('glmnet')
library(randomForest)
library(glmnet)
library(plyr)
library(rpart)
library(maptree)
library(cluster)
OJ$logp <- log(OJ$price)

OJ$Q <- exp(OJ$logmove)
Df1 <- ddply(OJ, c('store','week'),function(x) c(weighted_price = weighted.mean(x$price,x$Q)))
OJ <- merge(OJ, Df1)
dataToPass<-OJ[,c("weighted_price","AGE60","EDUC",
                  "ETHNIC","INCOME","HHLARGE","WORKWOM",
                  "HVAL150","SSTRDIST","SSTRVOL","CPDIST5","CPWVOL5", "store", 'week','logmove', 'price', 'logp', 'brand', 'feat')]
fit<-rpart(as.formula(weighted_price ~ AGE60 + EDUC + ETHNIC+ INCOME + HHLARGE + WORKWOM
                      + HVAL150 + SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5) ,data=dataToPass,method="anova",cp=0.007)
draw.tree(fit) 

dataToPass$leaf = fit$where 
OJFinal <- dataToPass
draw.tree(fit) 


OJFinal$week <- OJFinal$week+1

Df2 <- merge(OJ, OJFinal, by=c("brand","store","week"))
##fix this

names(Df2)[names(Df2) == "logp.x"] <- "lastweeklogp"
names(Df2)[names(Df2) == "feat.x"] <- "lastweekfeat"
names(Df2)[names(Df2) == "logmove.y"] <- "logmove"
names(Df2)[names(Df2) == "logp.y"] <- "logp"
names(Df2)[names(Df2) == "feat.y"] <- "feat"
names(Df2)[names(Df2) == "EDUC.y"] <- "EDUC"
names(Df2)[names(Df2) == "INCOME.y"] <- "INCOME"
names(Df2)[names(Df2) == "ETHNIC.y"] <- "ETHNIC"
names(Df2)[names(Df2) == "AGE60.y"] <- "AGE60"
names(Df2)[names(Df2) == "HHLARGE.y"] <- "HHLARGE"


oj_leaf1 <- Df2 %>% filter(leaf == 2)
tropicana_l1 <- oj_leaf1 %>% filter(brand == "tropicana")
minutemaid_l1 <- oj_leaf1 %>% filter(brand == "minute.maid")
dominicks_l1 <- oj_leaf1 %>% filter(brand == "dominicks")

oj_leaf2 <- Df2 %>% filter(leaf == 4)
tropicana_l2 <- oj_leaf2 %>% filter(brand == "tropicana")
minutemaid_l2 <- oj_leaf2 %>% filter(brand == "minute.maid")
dominicks_l2 <- oj_leaf2 %>% filter(brand == "dominicks")

oj_leaf3 <- Df2 %>% filter(leaf == 5)
tropicana_l3 <- oj_leaf3 %>% filter(brand == "tropicana")
minutemaid_l3 <- oj_leaf3 %>% filter(brand == "minute.maid")
dominicks_l3 <- oj_leaf3 %>% filter(brand == "dominicks")


##########leaf1
oj.rftrop11 <- randomForest(logmove ~ logp  + 
                             lastweeklogp + lastweekfeat + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                             feat + EDUC + INCOME + ETHNIC +
                             HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = tropicana_l1, ntree = 	100, keep.forest = TRUE) 
oj.rftrop21 <- randomForest(logp ~ lastweeklogp + lastweekfeat + 
                             feat + EDUC + INCOME + ETHNIC +
                             HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = tropicana_l1, ntree = 	100, keep.forest = TRUE) 

oj.rfmm11 <- randomForest(logmove ~ logp  + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                           lastweeklogp + lastweekfeat +  (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                           feat + EDUC + INCOME + ETHNIC +
                           HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = minutemaid_l1, ntree = 	100, keep.forest = TRUE) 
oj.rfmm21 <- randomForest(logp ~ lastweeklogp + lastweekfeat + 
                           feat + EDUC + INCOME + ETHNIC +(lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                           HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = minutemaid_l1, ntree = 	100, keep.forest = TRUE) 
oj.rfdom11 <- randomForest(logmove ~ logp  + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                            lastweeklogp + lastweekfeat 
                          + feat + EDUC + INCOME + ETHNIC +
                            HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = dominicks_l1, ntree = 	100, keep.forest = TRUE) 
oj.rfdom21 <- randomForest(logp ~ lastweeklogp + lastweekfeat + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                            feat + EDUC + INCOME + ETHNIC +
                            HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = dominicks_l1, ntree = 	100, keep.forest = TRUE) 

residuals1 <- list(predict(oj.rftrop11, tropicana_l1) - tropicana_l1$logmove ,predict(oj.rftrop21, tropicana_l1) - tropicana_l1$logp, predict(oj.rfmm11, minutemaid_l1) - minutemaid_l1$logmove,
                  predict(oj.rfmm21, minutemaid_l1) - minutemaid_l1$logp, predict(oj.rfdom11, dominicks_l1) - dominicks_l1$logmove, predict(oj.rfdom21, dominicks_l1) - dominicks_l1$logp)

myDf1 <- as.data.frame(residuals1)

names(myDf1)[1] <- "tropres1"
names(myDf1)[2] <- "tropres2"
names(myDf1)[3] <- "mmres1"
names(myDf1)[4] <- "mmres2"
names(myDf1)[5] <- "domres1"
names(myDf1)[6] <- "domres2"

tropOLS1 <- lm(tropres1 ~ tropres2 + mmres2+ domres2, data = myDf1)
domOLS1 <- lm(domres1 ~ tropres2 + mmres2+ domres2, data = myDf1)
mmOLS1 <- lm(mmres1 ~ tropres2 + mmres2+ domres2, data = myDf1)


summary(tropOLS1)
summary(mmOLS1)
summary(domOLS1)

elasticities_mat1 <- matrix(1:9, nrow = 3)
rownames(elasticities_mat1) <- c("tropicana","minutemaid","dominicks")
colnames(elasticities_mat1) <- c("PED_tropicana", "PED_MinuteMaid", "PED_dominicks")

elasticities_mat1[1,1] <- coef(tropOLS1)["tropres2"]
elasticities_mat1[1,2] <- coef(tropOLS1)["mmres2"]
elasticities_mat1[1,3] <- coef(tropOLS1)["domres2"]
elasticities_mat1[2,1] <- coef(mmOLS1)["tropres2"] 
elasticities_mat1[2,2] <- coef(mmOLS1)["mmres2"] 
elasticities_mat1[2,3] <- coef(mmOLS1)["domres2"]
elasticities_mat1[3,1] <- coef(domOLS1)["tropres2"] 
elasticities_mat1[3,2] <- coef(domOLS1)["mmres2"] 
elasticities_mat1[3,3] <- coef(domOLS1)["domres2"] 

elasticities_mat1
###########################################leaf2

##########
oj.rftrop12 <- randomForest(logmove ~ logp  + 
                             lastweeklogp + lastweekfeat + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                             feat + EDUC + INCOME + ETHNIC +
                             HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = tropicana_l2, ntree = 	100, keep.forest = TRUE) 
oj.rftrop22 <- randomForest(logp ~ lastweeklogp + lastweekfeat + 
                             feat + EDUC + INCOME + ETHNIC +
                             HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = tropicana_l2, ntree = 	100, keep.forest = TRUE) 

oj.rfmm12 <- randomForest(logmove ~ logp  + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                           lastweeklogp + lastweekfeat +  (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                           feat + EDUC + INCOME + ETHNIC +
                           HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = minutemaid_l2, ntree = 	100, keep.forest = TRUE) 
oj.rfmm22 <- randomForest(logp ~ lastweeklogp + lastweekfeat + 
                           feat + EDUC + INCOME + ETHNIC +(lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                           HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = minutemaid_l2, ntree = 	100, keep.forest = TRUE) 
oj.rfdom12 <- randomForest(logmove ~ logp  + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                            lastweeklogp + lastweekfeat 
                          + feat + EDUC + INCOME + ETHNIC +
                            HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = dominicks_l2, ntree = 	100, keep.forest = TRUE) 
oj.rfdom22 <- randomForest(logp ~ lastweeklogp + lastweekfeat + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                            feat + EDUC + INCOME + ETHNIC +
                            HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = dominicks_l2, ntree = 	100, keep.forest = TRUE) 

residuals2 <- list(predict(oj.rftrop12, tropicana_l2) - tropicana_l2$logmove ,predict(oj.rftrop22, tropicana_l2) - tropicana_l2$logp, predict(oj.rfmm12, minutemaid_l2) - minutemaid_l2$logmove,
                  predict(oj.rfmm22, minutemaid_l2) - minutemaid_l2$logp, predict(oj.rfdom12, dominicks_l2) - dominicks_l2$logmove, predict(oj.rfdom22, dominicks_l2) - dominicks_l2$logp)

myDf2 <- as.data.frame(residuals2)

names(myDf2)[1] <- "tropres1"
names(myDf2)[2] <- "tropres2"
names(myDf2)[3] <- "mmres1"
names(myDf2)[4] <- "mmres2"
names(myDf2)[5] <- "domres1"
names(myDf2)[6] <- "domres2"

tropOLS2 <- lm(tropres1 ~ tropres2 + mmres2+ domres2, data = myDf2)
domOLS2 <- lm(domres1 ~ tropres2 + mmres2+ domres2, data = myDf2)
mmOLS2 <- lm(mmres1 ~ tropres2 + mmres2+ domres2, data = myDf2)


summary(tropOLS2)
summary(mmOLS2)
summary(domOLS2)

elasticities_mat2 <- matrix(1:9, nrow = 3)
rownames(elasticities_mat2) <- c("tropicana","minutemaid","dominicks")
colnames(elasticities_mat2) <- c("PED_tropicana", "PED_MinuteMaid", "PED_dominicks")

elasticities_mat2[1,1] <- coef(tropOLS2)["tropres2"]
elasticities_mat2[1,2] <- coef(tropOLS2)["mmres2"]
elasticities_mat2[1,3] <- coef(tropOLS2)["domres2"]
elasticities_mat2[2,1] <- coef(mmOLS2)["tropres2"] 
elasticities_mat2[2,2] <- coef(mmOLS2)["mmres2"] 
elasticities_mat2[2,3] <- coef(mmOLS2)["domres2"]
elasticities_mat2[3,1] <- coef(domOLS2)["tropres2"] 
elasticities_mat2[3,2] <- coef(domOLS2)["mmres2"] 
elasticities_mat2[3,3] <- coef(domOLS2)["domres2"] 

elasticities_mat2

##########leaf3
oj.rftrop13 <- randomForest(logmove ~ logp  + 
                             lastweeklogp + lastweekfeat + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                             feat + EDUC + INCOME + ETHNIC +
                             HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = tropicana_l3, ntree = 	100, keep.forest = TRUE) 
oj.rftrop23 <- randomForest(logp ~ lastweeklogp + lastweekfeat + 
                             feat + EDUC + INCOME + ETHNIC +
                             HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = tropicana_l3, ntree = 	100, keep.forest = TRUE) 

oj.rfmm13 <- randomForest(logmove ~ logp  + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                           lastweeklogp + lastweekfeat +  (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                           feat + EDUC + INCOME + ETHNIC +
                           HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = minutemaid_l3, ntree = 	100, keep.forest = TRUE) 
oj.rfmm23 <- randomForest(logp ~ lastweeklogp + lastweekfeat + 
                           feat + EDUC + INCOME + ETHNIC +(lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                           HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = minutemaid_l3, ntree = 	100, keep.forest = TRUE) 
oj.rfdom13 <- randomForest(logmove ~ logp  + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                            lastweeklogp + lastweekfeat 
                          + feat + EDUC + INCOME + ETHNIC +
                            HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = dominicks_l3, ntree = 	100, keep.forest = TRUE) 
oj.rfdom23 <- randomForest(logp ~ lastweeklogp + lastweekfeat + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                            feat + EDUC + INCOME + ETHNIC +
                            HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y), data = dominicks_l3, ntree = 	100, keep.forest = TRUE) 

residuals3 <- list(predict(oj.rftrop13, tropicana_l3) - tropicana_l3$logmove ,predict(oj.rftrop23, tropicana_l3) - tropicana_l3$logp, predict(oj.rfmm13, minutemaid_l3) - minutemaid_l3$logmove,
                  predict(oj.rfmm23, minutemaid_l3) - minutemaid_l3$logp, predict(oj.rfdom13, dominicks_l3) - dominicks_l3$logmove, predict(oj.rfdom23, dominicks_l3) - dominicks_l3$logp)

myDf3 <- as.data.frame(residuals3)

names(myDf3)[1] <- "tropres1"
names(myDf3)[2] <- "tropres2"
names(myDf3)[3] <- "mmres1"
names(myDf3)[4] <- "mmres2"
names(myDf3)[5] <- "domres1"
names(myDf3)[6] <- "domres2"

tropOLS3 <- lm(tropres1 ~ tropres2 + mmres2+ domres2, data = myDf3)
domOLS3 <- lm(domres1 ~ tropres2 + mmres2+ domres2, data = myDf3)
mmOLS3 <- lm(mmres1 ~ tropres2 + mmres2+ domres2, data = myDf3)


summary(tropOLS3)
summary(mmOLS3)
summary(domOLS3)

elasticities_mat3 <- matrix(1:9, nrow = 3)
rownames(elasticities_mat3) <- c("tropicana","minutemaid","dominicks")
colnames(elasticities_mat3) <- c("PED_tropicana", "PED_MinuteMaid", "PED_dominicks")

elasticities_mat3[1,1] <- coef(tropOLS3)["tropres2"]
elasticities_mat3[1,2] <- coef(tropOLS3)["mmres2"]
elasticities_mat3[1,3] <- coef(tropOLS3)["domres2"]
elasticities_mat3[2,1] <- coef(mmOLS3)["tropres2"] 
elasticities_mat3[2,2] <- coef(mmOLS3)["mmres2"] 
elasticities_mat3[2,3] <- coef(mmOLS3)["domres2"]
elasticities_mat3[3,1] <- coef(domOLS3)["tropres2"] 
elasticities_mat3[3,2] <- coef(domOLS3)["mmres2"] 
elasticities_mat3[3,3] <- coef(domOLS3)["domres2"] 

elasticities_mat3
elasticities_mat2
elasticities_mat1


###############################
X <- model.matrix(logmove ~ logp  + 
                    lastweeklogp + lastweekfeat + (lastweeklogp * INCOME)+ (lastweeklogp * AGE60)+
                    feat + EDUC + INCOME + ETHNIC +
                    HHLARGE + AGE60 + (AGE60 * EDUC) + (AGE60 * INCOME)  + WORKWOM.y + (INCOME * WORKWOM.y),  tropicana_l1)[,-1]
set.seed(720)
cvfit <- cv.glmnet(X, tropicana_l1$logmove, alpha=1)
fit =  glmnet(X, tropicana_l1$logmove, alpha=1, lambda = cvfit$lambda.min)



tropicana_l1$resid = tropicana_l1$logmove -  predict(oj.rftrop11)
tropicana_l1$resid2 <- tropicana_l1$logmove -predict(fit, X )

plot1 <- ggplot() + 
  geom_line(data = tropicana_l1, aes(x = logp, y = resid2 ), color = "blue") +
  geom_line(data = tropicana_l1, aes(x = logp, y = resid), color = "red") +
  xlab('log_price') +
  ylab('logmove') 

print(plot1)
