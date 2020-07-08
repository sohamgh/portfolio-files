library("tidyr")
library("dplyr")
library("data.table")
library('mltools')
library("caret")
library("ggplot2")
library("lmtest")
library("sandwich")
mainTable <- read.csv('data.csv', stringsAsFactors = FALSE)
stats_table <- select(mainTable, Name, Age, Nationality, Overall, Value, Wage, Club, Release.Clause, League , spos, YAC, ContractValid , Pace, Stamina, Strength)
## some colums look wack
names(stats_table)[names(stats_table) == "Wage"] <- "Wage (in Thousands)"
names(stats_table)[names(stats_table) == "Value"] <- "Value (in Millions)"
names(stats_table)[names(stats_table) == "Release.Clause"] <- "Release (in Millions)"
stats_table$`Wage (in Thousands)` <- substring(stats_table$`Wage (in Thousands)`, 4, length(stats_table$`Wage (in Thousands)`)-1)##stop doesnt matter becuase it is somehow getting converted to char
#stats_table$`Value (in Millions)` <- substring(stats_table$`Value (in Millions)`, 4, length(stats_table$`Value (in Millions)`)-1)
stats_table$`Release (in Millions)` <- substring(stats_table$`Release (in Millions)`, 4, length(stats_table$`Release (in Millions)`)-1)
stats_table$`Wage (in Thousands)` <- substring(stats_table$`Wage (in Thousands)`, 1, nchar(stats_table$`Wage (in Thousands)`)-1)
#stats_table$`Value (in Millions)` <- substring(stats_table$`Value (in Millions)`, 1, nchar(stats_table$`Value (in Millions)`)-1)
stats_table$`Release (in Millions)` <- substring(stats_table$`Release (in Millions)`, 1, nchar(stats_table$`Release (in Millions)`)-1)
## character to numeric
stats_table$`Wage (in Thousands)`<- as.numeric(stats_table$`Wage (in Thousands)`)
stats_table$`Value (in Millions)`<- as.numeric(stats_table$`Value (in Millions)`)
stats_table$`Release (in Millions)`<- as.numeric(stats_table$`Release (in Millions)`)
stats_table <- stats_table %>% mutate(logwage = log(`Wage (in Thousands)`), logval = log(`Value (in Millions)`), logrel = log(`Release (in Millions)`), x2 = Overall^2)
biggestspend <- stats_table %>% select(`Wage (in Thousands)`,Overall, Club) %>% group_by(Club) %>% summarise(sumwage = sum(`Wage (in Thousands)`) , meanoverall = round(mean(Overall)))
bigspendleag <- stats_table %>% select(`Wage (in Thousands)`,Overall, League , `Value (in Millions)`) %>% group_by(League) %>% 
  summarise(sumwage = sum(`Wage (in Thousands)`) , meanoverall = round(mean(Overall)), sumval = sum(`Value (in Millions)` , na.rm = TRUE)) %>% filter(League != "Other")
bigspendpos <- stats_table %>% select(`Wage (in Thousands)`,spos , `Value (in Millions)`) %>% group_by(spos) %>% 
  summarise(meanwage = mean(`Wage (in Thousands)` , na.rm = TRUE), meanval = mean(`Value (in Millions)` , na.rm = TRUE))  %>%  filter(spos != "0")
##########################################################
plot( stats_table$Overall , stats_table$`Wage (in Thousands)`, xlab = "Overall Rating", ylab = "Wage(In Thousands)")
plot( stats_table$ContractValid , stats_table$`Wage (in Thousands)`, xlab = "Years Left", ylab = "Wage(In Thousands)")
plot(stats_table$Overall, stats_table$logwage, xlab = "Overall Rating", ylab = "Wage(In Thousands)")
plot(stats_table$x2 , stats_table$logwage, xlab = "Overall Rating", ylab = "Wage(In Thousands)")
plot(stats_table$Overall, stats_table$`Value (in Millions)`, xlab = "Overall Rating", ylab = "Market Value (in Millions)")
plot(stats_table$logwage, stats_table$logval, xlab = "log(wage)", ylab = "log(Market Value)")
plot(stats_table$`Wage (in Thousands)`, stats_table$`Value (in Millions)`, xlab = "Wage(In Thousands)", ylab = "Value (In Millions)")
plot(biggestspend$meanoverall, biggestspend$sumwage ,xlab = "Mean Overall Rating", ylab = "Total Wages Paid")
plot( stats_table$Pace , stats_table$`Wage (in Thousands)`, xlab = "Overall Rating", ylab = "Wage(In Thousands)")
barplot((bigspendleag$sumwage)/1000,
        names.arg= bigspendleag$League , ylab = "Wages in Millions"  )
barplot((bigspendleag$sumval)/1000,
        names.arg= bigspendleag$League , ylab = "Value in Billions"  )
barplot((bigspendpos$meanwage),
       names.arg= bigspendpos$spos , ylab = "Mean Wage (In Thousands) "  )
barplot((bigspendpos$meanval),
        names.arg= bigspendpos$spos , ylab = "Mean Value (in Millions) "  )
#hist(stats_table$`Wage (in Thousands)`  , xlim = range(0:500),breaks = 20 , main = "Wage Distribution")
#hist(stats_table$logwage  , xlim = range(0:500),breaks = 20 , main = "Wage Distribution")
topleagues <- stats_table %>% filter(League != "Other")
ggplot(topleagues, aes(x=`Wage (in Thousands)` , color = League)) + 
  geom_histogram(binwidth=1)+ xlim(range(0,200))

ggplot(topleagues, aes(x=logwage , fill = League)) + 
  geom_histogram(binwidth=0.4 )+ xlim(range(0,8))

ggplot(stats_table, aes(x=`Wage (in Thousands)` , fill = League )) + 
  geom_histogram(binwidth=1)+ xlim(range(0,100)) + xlab("Wage (in Thousands)")
ggplot(stats_table, aes(x=`Value (in Millions)` , fill = League )) + 
  geom_histogram(binwidth=0.5)+ xlim(range(0,25)) + xlab("Value (in Millions)")
plot(density(stats_table$logwage, na.rm = TRUE))

#hist(stats_table$`Wage (in Thousands)`, 1000, xlim = 100)
############################################################
##make graphs look good
la_liga <- stats_table %>% filter(League == "La_liga")
EPL <- stats_table %>% filter(League == "EPL")
ligue1 <- stats_table %>% filter(League == "Ligue 1")

plot(density(la_liga$`Wage (in Thousands)`), xlab = "Wages(in Thousands)", xlim = range(-0,200), ylim = range(0,0.04), main = "EPL vs La Liga vs Ligue1")
lines(density(EPL$`Wage (in Thousands)`), col = 'red')
lines(density(ligue1$`Wage (in Thousands)`, na.rm = TRUE), col = 'blue')
text(locator(), labels = c("EPL", "Ligue1" , "La Liga"))

plot(density(la_liga$`Value (in Millions)`), xlim = range(0,40), ylim = range(0,0.2), xlab = "Value(In Millions))", main = "EPL vs La Liga vs Ligue1" )
lines(density(EPL$`Value (in Millions)`), col = 'red')
lines(density(ligue1$`Value (in Millions)`, na.rm = TRUE), col = 'blue') ##missing values
text(locator(), labels = c("EPL", "Ligue1" , "La Liga"))

boxplot(EPL$`Wage (in Thousands)`)

#plot(hist(EPL$`Wage (in Thousands)`))
#line(hist(la_liga$`Wage (in Thousands)`))

plot(EPL$Overall , EPL$`Wage (in Thousands)`)
line()
##########################################################testing top 3 ligues seperately
#model1 <- lm(logwage ~ Overall + spos + League + Age + `Release (in Millions)` = stats_table)
model1_1 <- lm(logwage ~ Overall + spos + League + Age + logrel , data = stats_table)
#model1_2 <- lm(`Wage (in Thousands)` ~ x2 + spos + League + Age + logrel, data = stats_table)
#model1_3 <- lm(logwage ~ Overall + spos + League + Age + spos * League , data = stats_table)
model1_4 <- lm(logwage ~ Overall + spos + League + Age + logrel + YAC + ContractValid + Strength + Pace + Stamina, data = stats_table)
model1_5 <- lm(logwage ~ Overall + spos + League + Age + logrel + YAC + ContractValid + spos * League  + Strength + Pace + Stamina, data = stats_table)
model2 <- lm(logval ~ Overall + spos + League + Age + YAC + ContractValid + spos * League + logrel +Strength + Pace + Stamina, data = stats_table)
model2_1 <- lm(logval ~ Overall + spos + League + Age + YAC + ContractValid +logrel  +Strength + Pace + Stamina, data = stats_table)

model1_6 <- lm(logwage ~ Overall + Age + logrel + YAC + ContractValid + spos * League  + Strength + Pace + Stamina, data = stats_table)

summary(lm(logwage ~ Overall + spos + League + Age + `Release (in Millions)`, data = stats_table))
summary(model2)
summary(model2_1)
summary(model1_2)
summary(model1)
summary(model1_1)
summary(model1_4)
summary(model1_5)
summary(model1_6)
x <- stats_table$`Wage (in Thousands)`
y <- model1_1$residual
x = x[1:length(y -1 )]
plot(x, y , xlim = range(0,50), xlab = "Wage" , ylab = "residuals" )
#wage regression
mlaliga <- lm(logwage ~ Overall + spos +  Age + logrel + YAC + ContractValid + Strength + Pace + Stamina, data = la_liga)
summary(mlaliga)
plot(mlaliga)
mepl <- lm(logwage ~ Overall + spos +  Age + logrel + YAC + ContractValid + Strength + Pace + Stamina, data = EPL)
summary(mepl)
plot(mepl)
mlig1 <- lm(logwage ~ Overall + spos +  Age + logrel + YAC + ContractValid + Strength + Pace + Stamina, data = ligue1)
summary(mlig1)
plot(mlig1)

#mktval regression (shit R^2 as expected)
m2laliga <- lm(logval ~ Overall + spos +  Age + logrel, data = la_liga)
summary(m2laliga)
m2epl <- lm(logval ~ Overall + spos +  Age + logrel, data = EPL)
summary(m2epl)
m2lig1 <- lm(logval ~ Overall + spos +  Age + logrel, data = ligue1)
summary(m2lig1)
#library(data.table)

summary(lm(logval ~ Overall + spos + League + Age + `Release (in Millions)`, data = stats_table))
#library(data.table)

summary(lm(logval ~ Overall + spos + League + Age + logrel, data = stats_table))
summary(lm(logwage ~ Overall + spos + League + Age + logrel, data = stats_table))
summary(lm(logwage ~ Overall + spos + League + Age , data = stats_table))
summary(lm(logval ~ Overall + spos + League + Age, data = stats_table)) # very low R^2

###########################################################################
model1res <- residuals(model1)
##lowkey normal
plot(model1)
plot(model1_1 , main = "Wage Regression Model 1")
plot(model1_3)
plot(model1_4)
plot(model1_5, main = "Wage Regression Model")
plot(model2_1, main = "Market Value Regression Model")
#normal
plot(density(model1_1$residuals), main = "Residual Distribution" , xlab = "Residuals" , col = c("Red", "Blue", "Black" , "Orange") , ylim = range(0:0.8))
lines(density(model1_4$residuals))
lines(density(model1_5$residuals))
lines(density(model2$residuals))
lines(density(model2_1$residuals))
#not at all normal
plot(density(model2$residuals) ,main = "Value Model Residual Distribution" , xlab = "Residuals" , col = c("Red", "Blue", "Black" , "Orange") )
lines(density(model2_1$residuals))
#can be seen as normal
plot(density(mlaliga$residuals), main = "Wage Regressions Residual Distribution" , xlab = "Residuals"  )
lines(density(mepl$residuals))
lines(density(mlig1$residuals))
plot(la_liga$Overall , la_liga$`Wage (in Thousands)`)

##bptest 
bptest(model1_1)
bptest(mlaliga)
bptest(mepl)
bptest(mlig1)
#homoskedasticity is rejected for all. we dont bother testing model2 and model 2 variants. 
#testing coefficients uder robustness
coeftest(model1_1, vcov = vcovHC(model1_1, type="HC1"))
coeftest(model1_3, vcov = vcovHC(model1_3, type="HC1"))
coeftest(model1_4, vcov = vcovHC(model1_4, type="HC1"))
coeftest(model1_5, vcov = vcovHC(model1_5, type="HC1"))
coeftest(mlaliga, vcov = vcovHC(mlaliga, type="HC1"))
coeftest(mepl, vcov = vcovHC(mepl, type="HC1"))
coeftest(mlig1, vcov = vcovHC(mlig1, type="HC1"))
coeftest(model2, vcov = vcovHC(model2, type="HC1"))
coeftest(model2_1, vcov = vcovHC(model2_1, type="HC1"))
###########################model3
realstats <- read.csv("eplstats.csv")
names(realstats)[names(realstats) == "full_name"] <- "Name"
mergedstats <- merge.data.frame(realstats , EPL, by = "Name") 

anova(model1_4, model1_5)
anova(model1_1, model1_5)
anova(model1_1, model1_4)
anova(model2, model2_1)
#####################for correlation table
for_cor <- stats_table %>% select(Overall  , Age, logrel, YAC , ContractValid ,Strength , Pace , Stamina)

res <- cor(for_cor, use = "pairwise.complete.obs")
round(res , 2)
  