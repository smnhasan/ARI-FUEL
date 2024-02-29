library(foreign)
library(MASS)
library(pROC)
library(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)

## importing dataset

setwd('E:\\ResearchProject\\Data\\BDHS 2014\\BD_2014_DHS_SPSS\\bdkr70sv')

kr <- as.data.frame(read.spss('BDKR70FL.sav',use.value.labels=F),stringsAsFactors = FALSE)


## subsetting datasets

f <- c('V005','V021','V022','H31B','H31C','H43','HW1','V106',
       'B4','B5','V013','H10','V190','V161','V025',
       'V113','V116','HW70','HW72','V101','V135',
       'V445','B3','S803A','BORD','M17','M15','M43','M18',
       'V218','V120','V121','BIDX','B8')

kr <- kr[,f]
data.frame(kr)
kr$M18 <- ifelse(kr$M18==9|kr$M18==8 ,NA,kr$M18)
kr$M43 <- ifelse(kr$M43==9|kr$M43==8,NA,kr$M43)
kr$M15 <- ifelse(kr$M15==99,NA,kr$M15)
kr$M17 <- ifelse(kr$M17==9,NA,kr$M17)


kr$HW70 <- ifelse(kr$HW70==9996|kr$HW70==9997|kr$HW70==9998|kr$HW70==9999,NA,kr$HW70)
kr$HW72 <- ifelse(kr$HW72==9996|kr$HW72==9997|kr$HW72==9998|kr$HW72==9999,NA,kr$HW72)
kr$H31B <- ifelse(kr$H31B==9|kr$H31B==8|is.na(kr$H31B)==1 ,0,kr$H31B)
kr$H31C <- ifelse(kr$H31C==9|kr$H31C==8|is.na(kr$H31C)==1 ,0,kr$H31C)
kr$H43  <- ifelse(kr$H43==9 | kr$H43==8 ,NA,kr$H43)
kr$V106 <- ifelse(kr$V106==9,NA,kr$V106)
kr$H10  <- ifelse(kr$H10==9|kr$H10==8,NA,kr$H10)
kr$V161 <- ifelse(kr$V161==99|kr$V161==97|kr$V161==95|kr$V161==96,NA,kr$V161)

kr$V113 <- ifelse(kr$V113==99|kr$V113==97,NA,kr$V113)
kr$V116 <- ifelse(kr$V116==99|kr$V116==97,NA,kr$V116)
kr$V135 <- ifelse(kr$V135==9,NA,kr$V135)
kr$V445 <- ifelse(kr$V445==9998|kr$V445==9999,NA,kr$V445)
kr$V121 <- ifelse(kr$V121==9,NA,kr$V121)
kr$V120 <- ifelse(kr$V120==9,NA,kr$V120)





kr$V022  <- factor(kr$V022,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21),labels = c("Barisal city corp.","Chittagong city corp.","Dhaka city corp.","Khulna city corp.",
                                                                                                      "Rajshahi city corp.","Rangpur city corp.","Sylhet city corp.","Barisal other urban",
                                                                                                      "Chittagong other urban","Dhaka other urban","Khulna other urban","Rajshahi other urban",                                                                                                    "Rangpur other urban","Sylhet other urban","Barisal rural","Chittagong rural","Dhaka rural",
                                                                                                      "Khulna rural","Rajshahi rural","Rangpur rural","Sylhet rural"))
kr$H31B <- factor(kr$H31B,levels=c(1,0),labels = c('Yes','No'))
kr$H31C <- factor(kr$H31C,levels=c(1,2,3,6),labels = c('Chest only','Nose only','Both','Other'))
kr$H43  <- factor(kr$H43,levels=c(0,1),labels = c('No','Yes'))
kr$V106 <- ifelse(kr$V106==3|kr$V106==3,2,kr$V106)
kr$V106 <- factor(kr$V106,levels=c(2,1,0),labels = c('Secondary or Higher','Primary','No education'))
kr$B4   <- factor(kr$B4,levels = c(1,2),labels = c('Male','Female'))
kr$H10  <- factor(kr$H10,levels=c(1,0),labels = c('Yes','No'))

#kr$V190 <- ifelse(kr$V190==1|kr$V190==2|kr$V190==3,0,1)
#kr$V190 <- factor(kr$V190,levels=c(1,0),labels = c('High economic class','Low economic class'))
kr$V190 <- factor(kr$V190,levels=c(5,4,3,2,1),labels = c("High","High","Middle","Poor","Poor"))



kr$V025 <- factor(kr$V025,levels = c(1,2),labels = c('Urban','Rural'))
kr$V101 <- factor(kr$V101,levels=c(1,2,3,4,5,6,7),labels = c("Barisal","Chittagong","Dhaka","Khulna","Rajshahi","Rangpur","Sylhet"))
kr$V121   <- factor(kr$V121,levels=c(1,0),labels = c('Yes','No'))
kr$V120   <- factor(kr$V120,levels=c(1,0),labels = c('Yes','No'))
kr$YY <- as.integer(((kr$B3-1)/12))+1900
kr$MM <- kr$B3-((kr$YY-1900)*12)
kr$season <- ifelse(kr$MM==12|kr$MM==1|kr$MM==2,1,ifelse(kr$MM==3|kr$MM==4|kr$MM==5,2,ifelse(kr$MM==6|kr$MM==7|kr$MM==8,3,4)))
kr$season <- factor(kr$season,levels=c(1,2,3,4),labels=c('Summer','Autumn',"Winter","Spring"))
kr$M17 <- factor(kr$M17,levels=c(1,0),labels=c('caesarean','non-caesarean'))
kr$M43 <- factor(kr$M43,levels=c(1,0),labels=c('Yes','No'))

kr$M18 <- ifelse(kr$M18==4|kr$M18==5,1,ifelse(kr$M18==3,2,3))
kr$M18 <- factor(kr$M18,levels=c(1,2,3),labels=c('Below average','Average','Above average'))
kr$BORD <- ifelse(kr$BORD==1|kr$BORD==2|kr$BORD==3,1,ifelse(kr$BORD==4|kr$BORD==5|kr$BORD==6,2,3))
kr$BORD <- factor(kr$BORD,levels=c(1,2,3),labels=c('1-3','4-6','6+'))
kr$M15 <- ifelse(kr$M15==10|kr$M15==11,1,2)
kr$M15 <- factor(kr$M15,levels=1:2,labels=c('Home','Hospital'))
kr$stunting <-ifelse(kr$HW70 < -200 ,'Yes','No')
kr$stunting<-factor(kr$stunting)
kr$wasting <- ifelse(kr$HW72 < -200,'Yes','No')
kr$wasting<-factor(kr$wasting)
kr$media <- ifelse(kr$V121=='Yes'| kr$V120=='Yes','Yes','No')
kr$media <- factor(kr$media)

kr$age <- ifelse(kr$HW1 <= 11,1,ifelse(kr$HW1 >= 12 & kr$HW1 <= 23,2,ifelse(kr$HW1 >= 24 & kr$HW1 <= 59,3,4)))
kr$age <- factor(kr$age,levels=c(3,2,1),labels=c('24-59','12-23','0-11'))
kr$V013 <- ifelse(kr$V013==2 | kr$V013==1,1,ifelse(kr$V013==3 | kr$V013==4,2,ifelse(kr$V013==5 | kr$V013==6,3,4)))
kr$V013 <- factor(kr$V013,levels = c(1,2,3,4),labels = c("15-24","25-34","35-44","45+"))
kr$V161 <- ifelse(kr$V161==1 |kr$V161==2|kr$V161==3|kr$V161==4|kr$V161==5|kr$V161==6,1,2)
kr$V161 <- factor(kr$V161,levels = c(1,2),labels = c("Fossil fuel","Biomass fuel"))
summary(kr$V161)
kr$V116 <- ifelse(kr$V116==10|kr$V116==11|kr$V116==12|kr$V116==13|kr$V116==14|kr$V116==15,1,2)
kr$V116 <- factor(kr$V116,levels = c(1,2),labels = c("Modern toilet","Other"))
kr$V113 <- ifelse(kr$V113==10|kr$V113==11|kr$V113==12|kr$V113==13,1,ifelse(kr$V113==20|kr$V113==21,2,3))
kr$V113 <- factor(kr$V113,levels = c(1,2,3),labels = c("Piped Water","Tube well","Other"))
kr$ari <- ifelse(kr$H31B=='Yes'& (kr$H31C=='Chest only'|kr$H31C=='Both'),1 ,0)
kr$ari <- ifelse(is.na(kr$ari)==1,0,kr$ari) # As DHS suggested
kr$ari <- factor(kr$ari,levels = c(0,1),labels = c('No','Yes'))

kr$V445 <- kr$V445/100 #mother's bmi
kr$bmi  <- ifelse(kr$V445<18.5,1,ifelse(kr$V445>=18.5 & kr$V445<=24.9,2,ifelse(kr$V445>=25 & kr$V445<=29.9,3,4)))
kr$bmi  <- factor(kr$bmi,levels=c(4,3,2,1),labels = c('obese','over weight','normal weight','under weight'))
kr$wgt <- kr$V005/1000000 #sampling weight


model <- glm( kr$ari ~ kr$V161  + kr$V106 + kr$B4 + kr$V101 +
                kr$M17 + kr$M15 + kr$stunting + kr$wasting + kr$age 
                ,
              family=binomial(link='logit'),data=kr)
summary(model)
exp(cbind(coef(model), confint(model)))



library(tidyverse)
library(ggplot2)
library(broom)

set.seed(1234)

# Using the builtin Titanic dataset as example data for a GLM
tdf = as.data.frame(Titanic)

m1 = glm(Survived == "Yes" ~ Class + Sex, data = tdf, family = "binomial", weights = Freq)
m1_preds = tidy(m1, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(Model = "m1")
# Create modified data by mixing up the frequencies - doesn't do anything meaningful,
#   just a way to get different coefficients
tdf$FreqScrambled = sample(tdf$Freq)
m2 = glm(Survived == "Yes" ~ Class + Sex, data = tdf, 
         family = "binomial", weights = FreqScrambled)
m2_preds = tidy(m2, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(Model = "m2")

# At this point we have a table of odds ratios and confidence intervals
#   for plotting
ors = bind_rows(m1_preds, m2_preds)
ors


dodger = position_dodge(width = 0.3)
# Elements like pointrange and position_dodge only work when the outcome
#   is mapped to y, need to go through with OR set as y then flip at the
#   end
ggplot(ors, aes(y = estimate, x = term, colour = Model)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = dodger,
                  size = 1.2) +
  geom_hline(yintercept = 1.0, linetype = "dotted", size = 1) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL) +
  labs(y = "Odds ratio", x = "Effect") +
  coord_flip(ylim = c(0.1, 10)) +
  theme_bw() 
  



