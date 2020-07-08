OJ <- read.csv("oj.csv")
library(dplyr)
library(ggplot2)
ggplot( OJ, aes(x = "", y = OJ$price)) + 
  geom_boxplot()

ggplot(OJ, aes(factor(OJ$brand), OJ$price)) + geom_boxplot(aes(fill = factor(brand)))

OJ$logp <- log(OJ$price, 10)

ggplot( OJ, aes(x = "", y = OJ$logp)) + 
  geom_boxplot()
ggplot(OJ, aes(factor(OJ$brand), OJ$logp)) + geom_boxplot(aes(fill = factor(brand)))

ggplot(OJ, aes(x = logmove, y = logp))  + geom_point(aes(color = factor(brand)))

reg1 <- lm(logmove ~ logp , data = OJ)
summary(reg1)

reg2 <- lm(logmove ~ logp + brand , data = OJ)
summary(reg2)

reg3 <-  lm(logmove ~ logp + (logp * brand) + brand , data = OJ)
summary(reg3)

ggplot( OJ, aes( x= brand, y = feat, fill = factor(brand))) + geom_col()
###############
OJ %>%
  group_by(brand) %>%
  summarise(Sumfeat = sum(feat)) 

OJ %>%
  group_by(brand) %>%
  summarise(averfeat = mean(feat), averprice = mean(price)) 
######################

reg4 <-  lm(logmove ~ logp + (logp * brand) + brand + feat, data = OJ)
summary(reg4)

reg5 <-  lm(logmove ~ logp + (logp * brand * feat) + brand + feat, data = OJ)
summary(reg5)

reg6 <-  lm(logmove ~ logp + (logp * brand) + (brand * feat) + brand + feat, data = OJ)
summary(reg6)

reg7 <-  lm(logmove ~ logp + (logp * brand) + (brand * feat) + brand + feat + EDUC + INCOME + ETHNIC , data = OJ)
summary(reg7)
        