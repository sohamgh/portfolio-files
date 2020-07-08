library(dplyr)
library(plyr)
library(tidyr)
#install.packages('plyr')
#install.packages('rpart')
#install.packages('maptree')
#install.packages('cluster')
library(rpart)
library(maptree)
OJ <- read.csv("oj.csv")
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

oj_leaf1 <- dataToPass %>% filter(leaf == 2)
oj_leaf2 <- dataToPass %>% filter(leaf == 4)
oj_leaf3 <- dataToPass %>% filter(leaf == 5)

reg_int1 <- glm(logmove~log(price)*brand*feat, data=oj_leaf1)
reg_int2 <- glm(logmove~log(price)*brand*feat, data=oj_leaf2)
reg_int3 <- glm(logmove~log(price)*brand*feat, data=oj_leaf3)

OJnew <- OJFinal %>%  select( brand , store , logp, week) %>% 
  spread(brand, logp) 

Df3 <- OJFinal %>% select(brand, store, logmove, week , logp, feat, INCOME, EDUC, AGE60, WORKWOM, ETHNIC, leaf)

OJ2 <- full_join(OJnew , Df3, by = c("store","week")) 

#leaf 1 elasticities 
oj_leaf1_e <- OJ2 %>% filter(leaf == 2)
dom1 <- oj_leaf1_e %>% filter(brand == 'dominicks')
trop1 <- oj_leaf1_e %>% filter(brand == 'tropicana')
mm1 <- oj_leaf1_e %>% filter(brand == 'minute.maid')

dominicksreg1 <- lm(logmove ~ (dominicks) + (minute.maid) + (tropicana) + INCOME + EDUC + AGE60 + WORKWOM + ETHNIC , data = dom1)
summary(dominicksreg1)

tropicanareg1 <- lm(logmove ~ (dominicks) + (minute.maid) + (tropicana) + INCOME + EDUC + AGE60 + WORKWOM + ETHNIC , data = trop1)
summary(tropicanareg1)

minutemaidreg1 <- lm(logmove ~ (dominicks) + (minute.maid) + (tropicana) + INCOME + EDUC + AGE60 + WORKWOM + ETHNIC , data = mm1)
summary(minutemaidreg1)

elasticities1 <- matrix(1:9, nrow = 3)
rownames(elasticities1) <- c("tropicana","minutemaid","dominicks")
colnames(elasticities1) <- c("PED_tropicana", "PED_MinuteMaid", "PED_dominicks")

elasticities1[1,1] <- coef(tropicanareg1)["tropicana"]
elasticities1[1,2] <- coef(tropicanareg1)["minute.maid"]
elasticities1[1,3] <- coef(tropicanareg1)["dominicks"]
elasticities1[2,1] <- coef(minutemaidreg1)["tropicana"]
elasticities1[2,2] <- coef(minutemaidreg1)["minute.maid"]
elasticities1[2,3] <- coef(minutemaidreg1)["dominicks"]
elasticities1[3,1] <- coef(dominicksreg1)["tropicana"]
elasticities1[3,2] <- coef(dominicksreg1)["minute.maid"]
elasticities1[3,3] <- coef(dominicksreg1)["dominicks"]

#leaf 2 elasticities 
oj_leaf2_e <- OJ2 %>% filter(leaf == 4)
dom2 <- oj_leaf2_e %>% filter(brand == 'dominicks')
trop2 <- oj_leaf2_e %>% filter(brand == 'tropicana')
mm2 <- oj_leaf2_e %>% filter(brand == 'minute.maid')

dominicksreg2 <- lm(logmove ~ (dominicks) + (minute.maid) + (tropicana) + INCOME + EDUC + AGE60 + WORKWOM + ETHNIC , data = dom2)
summary(dominicksreg2)

tropicanareg2 <- lm(logmove ~ (dominicks) + (minute.maid) + (tropicana) + INCOME + EDUC + AGE60 + WORKWOM + ETHNIC , data = trop2)
summary(tropicanareg2)

minutemaidreg2 <- lm(logmove ~ (dominicks) + (minute.maid) + (tropicana) + INCOME + EDUC + AGE60 + WORKWOM + ETHNIC , data = mm2)
summary(minutemaidreg2)

elasticities2 <- matrix(1:9, nrow = 3)
rownames(elasticities2) <- c("tropicana","minutemaid","dominicks")
colnames(elasticities2) <- c("PED_tropicana", "PED_MinuteMaid", "PED_dominicks")

elasticities2[1,1] <- coef(tropicanareg2)["tropicana"]
elasticities2[1,2] <- coef(tropicanareg2)["minute.maid"]
elasticities2[1,3] <- coef(tropicanareg2)["dominicks"]
elasticities2[2,1] <- coef(minutemaidreg2)["tropicana"]
elasticities2[2,2] <- coef(minutemaidreg2)["minute.maid"]
elasticities2[2,3] <- coef(minutemaidreg2)["dominicks"]
elasticities2[3,1] <- coef(dominicksreg2)["tropicana"]
elasticities2[3,2] <- coef(dominicksreg2)["minute.maid"]
elasticities2[3,3] <- coef(dominicksreg2)["dominicks"]

#leaf 3 elasticities 

oj_leaf3_e <- OJ2 %>% filter(leaf == 5)
dom3 <- oj_leaf3_e %>% filter(brand == 'dominicks')
trop3 <- oj_leaf3_e %>% filter(brand == 'tropicana')
mm3 <- oj_leaf3_e %>% filter(brand == 'minute.maid')

dominicksreg3 <- lm(logmove ~ (dominicks) + (minute.maid) + (tropicana) + INCOME + EDUC + AGE60 + WORKWOM + ETHNIC , data = dom3)
summary(dominicksreg3)

tropicanareg3 <- lm(logmove ~ (dominicks) + (minute.maid) + (tropicana) + INCOME + EDUC + AGE60 + WORKWOM + ETHNIC , data = trop3)
summary(tropicanareg3)

minutemaidreg3 <- lm(logmove ~ (dominicks) + (minute.maid) + (tropicana) + INCOME + EDUC + AGE60 + WORKWOM + ETHNIC , data = mm3)
summary(minutemaidreg3)

elasticities3 <- matrix(1:9, nrow = 3)
rownames(elasticities3) <- c("tropicana","minutemaid","dominicks")
colnames(elasticities3) <- c("PED_tropicana", "PED_MinuteMaid", "PED_dominicks")

elasticities3[1,1] <- coef(tropicanareg3)["tropicana"]
elasticities3[1,2] <- coef(tropicanareg3)["minute.maid"]
elasticities3[1,3] <- coef(tropicanareg3)["dominicks"]
elasticities3[2,1] <- coef(minutemaidreg3)["tropicana"]
elasticities3[2,2] <- coef(minutemaidreg3)["minute.maid"]
elasticities3[2,3] <- coef(minutemaidreg3)["dominicks"]
elasticities3[3,1] <- coef(dominicksreg3)["tropicana"]
elasticities3[3,2] <- coef(dominicksreg3)["minute.maid"]
elasticities3[3,3] <- coef(dominicksreg3)["dominicks"]