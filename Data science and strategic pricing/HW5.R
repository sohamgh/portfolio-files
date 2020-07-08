OJ <- read.csv("oj.csv")
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
OJnew <- Df2 %>%  select( brand , store , logp, week) %>% 
  spread(brand, logp) 

Df3 <- Df2 %>% select(brand, store, logmove.x, logmove, week , logp, feat, lastweeklogp,HVAL150.y ,  SSTRDIST.y , SSTRVOL.y ,CPDIST5.y, CPWVOL5.y,
                      INCOME, EDUC, HHLARGE,  AGE60, WORKWOM.y, ETHNIC)

OJ2 <- full_join(OJnew , Df3, by = c("store","week")) 

dominicks <- OJ2 %>% filter(brand == "dominicks")

summary(dominicks$INCOME)

IncomeQ1 <- dominicks %>% filter(INCOME <= 10.456)
IncomeQ2 <- dominicks %>% filter(between(INCOME, 10.456, 10.635))
IncomeQ3 <- dominicks %>% filter(between(INCOME, 10.635, 10.797))
IncomeQ4 <- dominicks %>% filter(INCOME > 10.797)


m1 <- mean(IncomeQ1$logmove)
m2 <- mean(IncomeQ2$logmove)
m3 <- mean(IncomeQ3$logmove)
m4 <- mean(IncomeQ4$logmove)



MSE1 <- mean((IncomeQ1$logmove - m1) ^ 2)
MSE2 <- mean((IncomeQ2$logmove - m2) ^ 2)
MSE3 <- mean((IncomeQ3$logmove -m3) ^ 2)
MSE4 <- mean((IncomeQ4$logmove - m4) ^ 2)

#Quartile 3 has the lowest MSE
#Quartile 1 has the highest MSE 