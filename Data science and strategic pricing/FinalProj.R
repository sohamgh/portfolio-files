#install.packages('dplyr')
#install.packages("ggplot2")
library(dplyr)
library(ggplot2)
library(tidyr)
#install.packages("randomForest")
#install.packages('glmnet')
library("randomForest")
library(glmnet)

retail <- read.csv("online_retail.csv")
retailsale <- retail %>% filter(Quantity > 0 ) %>% filter(UnitPrice > 0)
retailpurchase<- retail %>% filter(Quantity < 0)

retailsale$sales <- retailsale$Quantity * retailsale$UnitPrice

retailSum <- aggregate(retailsale$sales, by=list(Description=retailsale$Description), FUN=sum)
summary(retailSum$x)
quantiles <- quantile(retailSum$x , probs = (0.8))

top20perc <- retailSum %>% filter(x >= quantiles )
top20percratio <- sum(top20perc$x)/sum(retailSum$x)

## top 20% of items by sales account for 80% of the sales. 
##top 10% accounts for 63% sales

quantiles1 <- quantile(retailSum$x , probs = seq( 0.9, 1))

top10perc <- retailSum %>% filter(x >= quantiles1 )


top10percratio <- sum(top10perc$x)/sum(retailSum$x)
##############

retail10perc <- retailsale %>%
  filter(grepl(paste(top10perc$Description, collapse="|"), Description))

ratioTransactions <- nrow(retail10perc)/nrow(retailsale)
#40% of transactions are just the top 10% of items(403 items)


summary(retail10perc$UnitPrice)
summary(retailsale$UnitPrice)

boxplot(retail10perc$UnitPrice)
boxplot(retailsale$UnitPrice)


##so the price of these goods are quite evenly spread out. 
retailtop5 <-  retailSum %>% top_n(5)
ratiosale5 <- sum(retailtop5$x)/sum(retailSum$x) *100
retailtop5 <- retailsale %>%
  filter(grepl(paste(retailtop5$Description, collapse="|"), Description))
retailbottom <- 
  retailtop5cust <- retailsale %>%
  filter(grepl(paste(unique(retailtop5$CustomerID), collapse="|"), CustomerID))# %>% summary(group_by(Description),sum = sum(sales) ) 
write.csv(retailtop5cust,"~/econ487\\sample.csv", row.names = FALSE)

perc <- sum(retailtop5cust$sales)/sum(retailsale$sales) *100
perc2 <- length(unique(retailtop5cust$CustomerID))/length(unique(retailsale$CustomerID)) *100
###double regression 


regQ <- lm(log(Quantity) ~ log(UnitPrice) +  (Description*log(UnitPrice))   + Description, retailtop5)
summary(regQ)

plot(dist(retailtop5cus$UnitPrice))

elasticities_mat1 <- matrix(1:4, nrow = 4)
rownames(elasticities_mat1) <- c("DOTCOM POSTAGE", "PARTY BUNTING", "REGENCY CAKESTAND 3 TIER", "WHITE HANGING HEART T-LIGHT HOLDER")
colnames(elasticities_mat1) <- c("Price Elasticity")

elasticities_mat1[1,1] <- round(coef(regQ)["log(UnitPrice)"], 3)
elasticities_mat1[2,1] <- round(coef(regQ)["log(UnitPrice):DescriptionPARTY BUNTING"], 3)
elasticities_mat1[3,1] <- round(coef(regQ)["log(UnitPrice):DescriptionREGENCY CAKESTAND 3 TIER"], 3)
elasticities_mat1[4,1] <- round(coef(regQ)["log(UnitPrice):DescriptionWHITE HANGING HEART T-LIGHT HOLDER"], 3)




