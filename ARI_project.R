
require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)

## importing dataset

setwd('E:\\Study\\ResearchProject\\ARI_BioMass\\BD_2017-18_DHS_12012021_346_170459\\BDKR7RDT')

kr <- as.data.frame(read.spss('BDKR70FL.sav',use.value.labels=F),stringsAsFactors = FALSE)
View(kr)

## subsetting datasets

f <- c('v005','v021','v022','h31b','h31c','h43','hw1','v106',
       'b4','b5','v013','h10','v190','v161','v025',
       'v113','v116','hw70','hw72','hw73','v101','v135',
       'v445','b3','bord','m17','m15','m43','m18',
       'v218','v120','v121','bidx','s107a', 'v136',
       'v119','v127','v128','v129','v705','v717',
       'v719','v701','m19','h34')

kr <- kr[,f]
kr <- kr[!is.na(kr$hw1),]

kr$m18 <- ifelse(kr$m18==9|kr$m18==8 ,na,kr$m18)
kr$m43 <- ifelse(kr$m43==9|kr$m43==8,na,kr$m43)
kr$m15 <- ifelse(kr$m15==99,na,kr$m15)
kr$m17 <- ifelse(kr$m17==9,na,kr$m17)


kr$hw70 <- ifelse(kr$hw70==9996|kr$hw70==9997|kr$hw70==9998|kr$hw70==9999,NA,kr$hw70)
kr$hw72 <- ifelse(kr$hw72==9996|kr$hw72==9997|kr$hw72==9998|kr$hw72==9999,NA,kr$hw72)
kr$h31b <- ifelse(kr$h31b==9|kr$h31b==8|is.na(kr$h31b)==1 ,0,kr$h31b)
kr$h31c <- ifelse(kr$h31c==9|kr$h31c==8|is.na(kr$h31c)==1 ,0,kr$h31c)
kr$h43  <- ifelse(kr$h43==9 | kr$h43==8 ,NA,kr$h43)
kr$v106 <- ifelse(kr$v106==9,NA,kr$v106)
kr$h10  <- ifelse(kr$h10==9|kr$h10==8,NA,kr$h10)
kr$v161 <- ifelse(kr$v161==99|kr$v161==97,NA,kr$v161)
kr$v113 <- ifelse(kr$v113==99|kr$v113==97,NA,kr$v113)
kr$v116 <- ifelse(kr$v116==99|kr$v116==97,NA,kr$v116)
kr$v135 <- ifelse(kr$v135==9,NA,kr$v135)
kr$v445 <- ifelse(kr$v445==9998|kr$v445==9999,NA,kr$v445)
kr$v121 <- ifelse(kr$v121==9,NA,kr$v121)
kr$v120 <- ifelse(kr$v120==9,NA,kr$v120)
kr$v119 <- ifelse(kr$v119==7,NA,kr$v119)

kr$v127 <- ifelse(kr$v127==97,NA,kr$v127)
kr$v128 <- ifelse(kr$v128==97,NA,kr$v128)
kr$v129 <- ifelse(kr$v129==97,NA,kr$v129)

kr$v705 <- ifelse(kr$v705==98,NA,kr$v705)
kr$v717 <- ifelse(kr$v717==98,NA,kr$v717)
kr$v701 <- ifelse(kr$v701==8,NA,kr$v701)
kr$m19 <- ifelse(kr$m19==9996|kr$m19==9998,NA,kr$m19)
kr$h34 <- ifelse(kr$h34==8,NA,kr$h34)


kr$V022  <- factor(kr$v022,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),labels = c("barisal - city corporation","barisal - other urban","barisal - rural",
                                                                                                         "chittagong - city corporation","chittagong - other urban",
                                                                                                         "chittagong - rural","dhaka - city corporation", "dhaka - other urban",
                                                                                                         "dhaka - rural","khulna - city corporation","khulna - other urban","khulna - rural",
                                                                                                         "mymensingh - other urban","mymensingh - rural","rajshahi - city corporation",
                                                                                                         "rajshahi - other urban", "rajshahi - rural","rangpur - other urban",
                                                                                                         "rangpur - rural","sylhet - city corporation","sylhet - other urban","sylhet - rural"))
kr$h31b <- factor(kr$h31b,levels=c(1,0),labels = c('yes','no'))
kr$h31c <- factor(kr$h31c,levels=c(1,2,3,6),labels = c('chest only','nose only','both','other'))
kr$h43  <- factor(kr$h43,levels=c(0,1),labels = c('no','yes'))

kr$v106 <- ifelse(kr$v106==0|kr$v106==1,0,kr$v106)
kr$v106 <- factor(kr$v106,levels=c(3,2,0),labels = c('higher','secondary','no education or primary'))

kr$v701 <- ifelse(kr$v701==0|kr$v701==1,0,kr$v701)
kr$hedu <- factor(kr$v701,levels=c(3,2,0),labels = c('higher','secondary','no education or primary'))

kr$b4   <- factor(kr$b4,levels = c(1,2),labels = c('male','female'))
kr$h10  <- factor(kr$h10,levels=c(1,0),labels = c('yes','no'))

#kr$v190 <- ifelse(kr$v190==1|kr$v190==2|kr$v190==3,0,1)
kr$v190 <- ifelse(kr$v190==1 | kr$v190==2,1,ifelse(kr$v190==3,2,3))
#kr$v190 <- factor(kr$v190,levels=c(1,0),labels = c('high economic class','low economic class'))
kr$v190 <- factor(kr$v190,levels=c(3,2,1),labels = c("rich","middle","poor"))

kr$v025 <- factor(kr$v025,levels = c(1,2),labels = c('urban','rural'))
kr$b5   <- factor(kr$b5,levels=c(1,0),labels = c('yes','no'))
kr$v135   <- factor(kr$v135,levels=c(1,2),labels = c('usual resident','visitior'))
kr$v101 <- factor(kr$v101,levels=c(1,2,3,4,5,6,7,8),labels = c("barisal","chittagong","dhaka","khulna","Mymensingh","rajshahi","rangpur","sylhet"))
kr$v121   <- factor(kr$v121,levels=c(1,0),labels = c('yes','no'))
kr$v120   <- factor(kr$v120,levels=c(1,0),labels = c('yes','no'))

kr$yy <- as.integer(((kr$b3-1)/12))+1900
kr$mm <- kr$b3-((kr$yy-1900)*12)

kr$season <- ifelse(kr$mm==12|kr$mm==1|kr$mm==2,1,ifelse(kr$mm==3|kr$mm==4|kr$mm==5,2,ifelse(kr$mm==6|kr$mm==7|kr$mm==8,3,4)))
kr$season <- factor(kr$season,levels=c(1,2,3,4),labels=c('summer','autumn',"winter","spring"))

kr$s107a <- factor(kr$s107a,levels=c(1,2),labels=c('school','madrasha'))


kr$m17 <- factor(kr$m17,levels=c(1,0),labels=c('caesarean','non-caesarean'))
kr$m43 <- factor(kr$m43,levels=c(1,0),labels=c('yes','no'))

kr$m18 <- ifelse(kr$m18==4|kr$m18==5,1,ifelse(kr$m18==3,2,3))
kr$m18 <- factor(kr$m18,levels=c(1,2,3),labels=c('below average','average','above average'))

kr$v218 <- ifelse(kr$v218 <=2 ,1,ifelse(kr$v218 >=5,3,2))
kr$v218 <- factor(kr$v218,levels=c(1,2,3),labels=c('less or equal  2','3-4','greater or equal  5'))

kr$bord <- ifelse(kr$bord==1|kr$bord==2|kr$bord==3,1,ifelse(kr$bord==4|kr$bord==5|kr$bord==6,2,3))
kr$bord <- factor(kr$bord,levels=c(1,2,3),labels=c('1-3','4-6','6+'))

kr$m15 <- ifelse(kr$m15==10|kr$m15==11,1,2)
kr$m15 <- factor(kr$m15,levels=1:2,labels=c('home','hospital'))

kr$stunting <-ifelse(kr$hw70 < -200 ,'yes','no')
kr$stunting<-factor(kr$stunting)

kr$wasting <- ifelse(kr$hw72 < -200,'yes','no')
kr$wasting<-factor(kr$wasting)

kr$uw <- ifelse(kr$hw73 < -200,'yes','no')
kr$uw<-factor(kr$uw)


kr$media <- ifelse(kr$v121=='yes'| kr$v120=='yes','yes','no')
kr$media <- factor(kr$media)


kr$age <- ifelse(kr$hw1 <= 11,1,ifelse(kr$hw1 >= 12 & kr$hw1 <= 23,2,ifelse(kr$hw1 >= 24 & kr$hw1 <= 59,3,4)))
kr$age <- factor(kr$age,levels=c(3,2,1),labels=c('24-59','12-23','0-11'))
summary(kr$age)


kr$v013 <- ifelse(kr$v013==2 | kr$v013==1,1,ifelse(kr$v013==3 | kr$v013==4,2,3))
kr$v013 <- factor(kr$v013,levels = c(1,2,3),labels = c("15-24","25-34","35+"))

kr$v161 <- ifelse(kr$v161==1 |kr$v161==2|kr$v161==3|kr$v161==4|kr$v161==5|kr$v161==6|kr$v161==7,1,2)
kr$v161 <- factor(kr$v161,levels = c(1,2),labels = c("fossil fuel","biomass fuel"))


kr$v116 <- ifelse(kr$v116==10|kr$v116==11|kr$v116==12|kr$v116==13|kr$v116==14|kr$v116==15,1,2)
kr$v116 <- factor(kr$v116,levels = c(1,2),labels = c("modern toilet","other"))

kr$v113 <- ifelse(kr$v113==10|kr$v113==11|kr$v113==12|kr$v113==13,1,ifelse(kr$v113==20|kr$v113==21,2,3))
kr$v113 <- factor(kr$v113,levels = c(1,2,3),labels = c("piped water","tube well","other"))


kr$ari <- ifelse(kr$h31b=='yes'& (kr$h31c=='chest only'|kr$h31c=='both'),1 ,0)

kr$ari <- ifelse(is.na(kr$ari)==1,0,kr$ari) # as dhs suggested

kr$ari <- factor(kr$ari,levels = c(0,1),labels = c('no','yes'))
summary(kr$ari)

kr$v445 <- kr$v445/100 #mother's bmi
kr$bmi  <- ifelse(kr$v445<18.5,1,ifelse(kr$v445>=18.5 & kr$v445<=24.9,2,ifelse(kr$v445>=25 & kr$v445<=29.9,3,4)))
kr$bmi  <- factor(kr$bmi,levels=c(4,3,2,1),labels = c('obese','over weight','normal weight','under weight'))

median(kr$v136)
kr$hhmem <- ifelse(kr$v136<5,1,2)
kr$hhmem  <- factor(kr$hhmem,levels=c(1,2),labels = c('belowmedian','abovemedian'))

kr$v119 <- factor(kr$v119,levels = c(0,1),labels = c('no','yes'))

kr$floor <- ifelse(kr$v127==11|kr$v127==12,1,ifelse(kr$v127==21|kr$v127==22,2,3))
kr$floor <- factor(kr$floor,levels = c(1,2,3),labels = c("natural","rudimentary","finished"))

kr$wall <- ifelse(kr$v128==11|kr$v128==12|kr$v128==13,1,ifelse(kr$v128==21|kr$v128==22|
                                                                 kr$v128==23|kr$v128==24|kr$v128==25|kr$v128==26,2,3))
kr$wall <- factor(kr$wall,levels = c(1,2,3),labels = c("natural","rudimentary","finished"))

kr$roof <- ifelse(kr$v129==11|kr$v129==12,1,ifelse(kr$v129==21|kr$v129==22,2,3))
kr$roof <- factor(kr$roof,levels = c(1,2,3),labels = c("natural","rudimentary","finished"))

kr$hocu <- ifelse(kr$v705==4|kr$v705==5,1,ifelse(kr$v705==0,2,3))
kr$hocu <- factor(kr$hocu,levels = c(1,2,3),labels = c("Agri","dontw","indus"))

kr$rocu <- ifelse(kr$v717==4|kr$v717==5,1,ifelse(kr$v717==0,2,3))
kr$rocu <- factor(kr$rocu,levels = c(1,2,3),labels = c("Agri","dontw","indus"))

kr$rworkfor <- factor(kr$v719,levels = c(1,2,3),labels = c("family","else","self"))

kr$bw <- ifelse(kr$m19 <=2500,1,2)
kr$bw <- factor(kr$bw,levels=c(1,2),labels=c('LBW','Normal'))
kr$va <- factor(kr$h34,levels = c(0,1),labels = c('no','yes'))

kr$wgt <- kr$v005/1000000 #sampling weight
summary(kr$rworkfor)

#keep only data for usual resident and surviving children of age under 5.

#kr <- kr[which(kr$v135== 'usual resident' & kr$hw1 < 60 & kr$b5=='yes'), ]


## saving finaldata set

save(kr,file='kr.rdata')


## load 'kr.rdata' 

#setwd('c:\\users\\rhafi sheikh\\desktop\\project01')
#load(file='kr.rdata')


#analysis
kr$bw
design1 <- svydesign(id=kr$v021, strata=kr$v022, weights=kr$wgt,data=kr)

round(svytable(~ari,design=design1))
prop.table(svytable(~ari,design=design1))*100

round(svytable(~v025+ari,design=design1)) #type of place of residence
round(svytable(~v025,design=design1))
prop.table(svytable(~v025,design=design1))*100
prop.table(svytable(~v025+ari,design=design1), margin=1)*100
svychisq(~v025+ari,design=design1,data=kr)  #sig

logit1 <- (svyglm(ari~v025, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~v101+ari,design=design1))  #division
round(svytable(~v101,design=design1))
prop.table(svytable(~v101,design=design1))*100
prop.table(svytable(~v101+ari,design=design1), margin=1)*100
svychisq(~v101+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~v101, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~media+ari,design=design1)) #media
round(svytable(~media,design=design1))
prop.table(svytable(~media,design=design1))*100
prop.table(svytable(~media+ari,design=design1), margin=1)*100
svychisq(~media+ari,design=design1,data=kr)  #sig

logit1 <- (svyglm(ari~media, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~v113+ari,design=design1)) #source of drinking water
round(svytable(~v113,design=design1))
prop.table(svytable(~v113,design=design1))*100
prop.table(svytable(~v113+ari,design=design1), margin=1)*100
svychisq(~v113+ari,design=design1,data=kr)  #sig

logit1 <- (svyglm(ari~v113, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~v116+ari,design=design1)) #Toilet Service
round(svytable(~v116,design=design1))
prop.table(svytable(~v116,design=design1))*100
prop.table(svytable(~v116+ari,design=design1), margin=1)*100
svychisq(~v116+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~v116, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~v161+ari,design=design1)) #fuel
round(svytable(~v161,design=design1))
prop.table(svytable(~v161,design=design1))*100
prop.table(svytable(~v161+ari,design=design1), margin=1)*100
svychisq(~v161+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~v161, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig
model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~v190+ari,design=design1))  #wealth index combined
round(svytable(~v190,design=design1))
prop.table(svytable(~v190,design=design1))*100
prop.table(svytable(~v190+ari,design=design1), margin=1)*100
svychisq(~v190+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~v190, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~v119+ari,design=design1)) #electricity
round(svytable(~v119,design=design1))
prop.table(svytable(~v119,design=design1))*100
prop.table(svytable(~v119+ari,design=design1), margin=1)*100
svychisq(~v119+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~v119, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~floor+ari,design=design1)) #floor
round(svytable(~floor,design=design1))
prop.table(svytable(~floor,design=design1))*100
prop.table(svytable(~floor+ari,design=design1), margin=1)*100
svychisq(~floor+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~floor, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~roof+ari,design=design1)) #roof
round(svytable(~roof,design=design1))
prop.table(svytable(~roof,design=design1))*100
prop.table(svytable(~roof+ari,design=design1), margin=1)*100
svychisq(~roof+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~roof, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~wall+ari,design=design1)) #wall
round(svytable(~wall,design=design1))
prop.table(svytable(~wall,design=design1))*100
prop.table(svytable(~wall+ari,design=design1), margin=1)*100
svychisq(~wall+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~wall, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~hhmem+ari,design=design1)) #hhmember
round(svytable(~hhmem,design=design1))
prop.table(svytable(~hhmem,design=design1))*100
prop.table(svytable(~hhmem+ari,design=design1), margin=1)*100
svychisq(~hhmem+ari,design=design1,data=kr) #insig


logit1 <- (svyglm(ari~hhmem, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci


round(svytable(~h10+ari,design=design1)) #ever had vaccination
round(svytable(~h10,design=design1))
prop.table(svytable(~h10,design=design1))*100
prop.table(svytable(~h10+ari,design=design1), margin=1)*100
svychisq(~h10+ari,design=design1,data=kr)  #sig

logit1 <- (svyglm(ari~h10, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~v013+ari,design=design1)) #age in 5-year groups
round(svytable(~v013,design=design1))
prop.table(svytable(~v013,design=design1))*100
prop.table(svytable(~v013+ari,design=design1), margin=1)*100
svychisq(~v013+ari,design=design1,data=kr)  #insig

logit1 <- (svyglm(ari~v013, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~v106+ari,design=design1)) # mother Education
round(svytable(~v106,design=design1))
prop.table(svytable(~v106,design=design1))*100
prop.table(svytable(~v106+ari,design=design1), margin=1)*100
svychisq(~v106+ari,design=design1,data=kr)  #insig

logit1 <- (svyglm(ari~v106, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~bmi+ari,design=design1))  #mothers bmi
round(svytable(~bmi,design=design1))
prop.table(svytable(~bmi,design=design1))*100
prop.table(svytable(~bmi+ari,design=design1), margin=1)*100
svychisq(~bmi+ari,design=design1,data=kr) #insig

logit1 <- (svyglm(ari~bmi, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~v218+ari,design=design1)) #number of living children
round(svytable(~v218,design=design1))
prop.table(svytable(~v218,design=design1))*100
prop.table(svytable(~v218+ari,design=design1), margin=1)*100
svychisq(~v218+ari,design=design1,data=kr)  #insig

logit1 <- (svyglm(ari~v218, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~rocu+ari,design=design1))
round(svytable(~rocu,design=design1))
prop.table(svytable(~rocu,design=design1))*100
prop.table(svytable(~rocu+ari,design=design1), margin=1)*100
svychisq(~rocu+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~rocu, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~rworkfor+ari,design=design1))
round(svytable(~rworkfor,design=design1))
prop.table(svytable(~rworkfor,design=design1))*100
prop.table(svytable(~rworkfor+ari,design=design1), margin=1)*100
svychisq(~rworkfor+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~rworkfor, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~hocu+ari,design=design1))
round(svytable(~hocu,design=design1))
prop.table(svytable(~hocu,design=design1))*100
prop.table(svytable(~hocu+ari,design=design1), margin=1)*100
svychisq(~hocu+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~hocu, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~hedu+ari,design=design1))
round(svytable(~hedu,design=design1))
prop.table(svytable(~hedu,design=design1))*100
prop.table(svytable(~hedu+ari,design=design1), margin=1)*100
svychisq(~hedu+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~hedu, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~s107a+ari,design=design1)) #HH education type
round(svytable(~s107a,design=design1))
prop.table(svytable(~s107a,design=design1))*100
prop.table(svytable(~s107a+ari,design=design1), margin=1)*100
svychisq(~s107a+ari,design=design1,data=kr) #insig 

logit1 <- (svyglm(ari~s107a, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~age+ari,design=design1)) #child age
round(svytable(~age,design=design1))
prop.table(svytable(~age,design=design1))*100
prop.table(svytable(~age+ari,design=design1), margin=1)*100
svychisq(~age+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~age, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~b4+ari,design=design1)) #sex of child
round(svytable(~b4,design=design1))
prop.table(svytable(~b4,design=design1))*100
prop.table(svytable(~b4+ari,design=design1), margin=1)*100
svychisq(~b4+ari,design=design1,data=kr)  #sig

logit1 <- (svyglm(ari~b4, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~bord+ari,design=design1)) #birth order
round(svytable(~bord,design=design1))
prop.table(svytable(~bord,design=design1))*100
prop.table(svytable(~bord+ari,design=design1), margin=1)*100
svychisq(~bord+ari,design=design1,data=kr)   #sig

logit1 <- (svyglm(ari~bord, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1)  #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~m15+ari,design=design1)) #place of delivery
round(svytable(~m15,design=design1))
prop.table(svytable(~m15,design=design1))*100
prop.table(svytable(~m15+ari,design=design1), margin=1)*100
svychisq(~m15+ari,design=design1,data=kr)  #insig

logit1 <- (svyglm(ari~m15, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1)  #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~bw+ari,design=design1)) #birth weight
round(svytable(~bw,design=design1))
prop.table(svytable(~bw,design=design1))*100
prop.table(svytable(~bw+ari,design=design1), margin=1)*100
svychisq(~bw+ari,design=design1,data=kr) #insig

logit1 <- (svyglm(ari~bw, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1)  #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~m17+ari,design=design1)) #C-section
round(svytable(~m17,design=design1))
prop.table(svytable(~m17,design=design1))*100
prop.table(svytable(~m17+ari,design=design1), margin=1)*100
svychisq(~m17+ari,design=design1,data=kr) #insig

logit1 <- (svyglm(ari~m17, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1)  #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~season+ari,design=design1)) #season
round(svytable(~season,design=design1))
prop.table(svytable(~season,design=design1))*100
prop.table(svytable(~season+ari,design=design1), margin=1)*100
svychisq(~season+ari,design=design1,data=kr)   #sig

logit1 <- (svyglm(ari~season, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~h43+ari,design=design1))  #drugs for intestinal parasites in last 6 months
round(svytable(~h43,design=design1))
prop.table(svytable(~h43,design=design1))*100
prop.table(svytable(~h43+ari,design=design1), margin=1)*100
svychisq(~h43+ari,design=design1,data=kr) #sig

logit1 <- (svyglm(ari~h43, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~va+ari,design=design1)) #vitamin a
round(svytable(~va,design=design1))
prop.table(svytable(~va,design=design1))*100
prop.table(svytable(~va+ari,design=design1), margin=1)*100
svychisq(~va+ari,design=design1,data=kr) #insig

logit1 <- (svyglm(ari~va, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1)  #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~stunting+ari,design=design1))
round(svytable(~stunting,design=design1))
prop.table(svytable(~stunting,design=design1))*100
prop.table(svytable(~stunting+ari,design=design1), margin=1)*100
svychisq(~stunting+ari,design=design1,data=kr) #insig

logit1 <- (svyglm(ari~stunting, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1)  #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

round(svytable(~wasting+ari,design=design1))
round(svytable(~wasting,design=design1))
prop.table(svytable(~wasting,design=design1))*100
prop.table(svytable(~wasting+ari,design=design1), margin=1)*100
svychisq(~wasting+ari,design=design1,data=kr) #insig

logit1 <- (svyglm(ari~wasting, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1)  #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci


round(svytable(~uw+ari,design=design1))
round(svytable(~uw,design=design1))
prop.table(svytable(~uw,design=design1))*100
prop.table(svytable(~uw+ari,design=design1), margin=1)*100
svychisq(~uw+ari,design=design1,data=kr) #insig

logit1 <- (svyglm(ari~uw, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1)  #insig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci


#Multi-variate

logit1 <- (svyglm(ari~v161, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

options(scipen=999)
logit1 <- (svyglm(ari~v101+ media + v116 + v161 + v190 + v119 + roof + wall + hhmem + age + b4 + h10 + rocu + hocu, family=quasibinomial(link=logit), data=kr,design=design1, na.action = na.omit))
summary(logit1) #sig
vif(logit1)

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci


#multivariable logistic


hoslem.test(logit1$y, fitted(logit1), g=10) #hosmer and lemeshow goodness of fit  test

#auc value

prob <- predict(logit1,type="response")
pred <- prediction(as.numeric(prob),as.numeric(logit1$y))
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc

#roc curve

plot(perf, main="ROC Curve ", xlab="specificity",  ylab="sensitivity")  
grid()
abline(0,1, col="blue", lty=2)

#graph

# #graph1
# 
# q_2 <- svytable(~kr$ari+kr$v106+kr$v025,design1)
# 
# q_2 <- as.data.frame(q_2)
# names(q_2) <- c('ari','edu','res','freq')
# q_2 <- q_2[which(q_2$ari=='yes'),]
# 
# q_2.1 <- as.data.frame(svytable(~kr$v106+kr$v025,design1))
# 
# q_2$freq <- (q_2$freq/q_2.1$Freq)*100
# 
# ggplot(q_2, aes(x=edu, y=freq,fill=res)) +geom_bar(stat='identity',position = 'dodge')+
#   geom_text(aes(label=paste(round(freq,2),'%',sep="")),
#             position=position_dodge(width=0.8),vjust=1,color='white',size=3.5)+
#   labs(x='educational level of mother',
#        y='percentage of children',fill="place of residence")+
#   scale_fill_manual(values=c("black", "#7f8c8d"))+
#   theme_light()


#graph2
require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)
library(nnet)
library(FSA)
library(caret)
require(mapproj)
require(rgdal)
require(car)

round(svytable(~v101+ari,design=design1))  #division
round(svytable(~v101,design=design1))
prop.table(svytable(~v101,design=design1))*100
prop.table(svytable(~v101+ari,design=design1), margin=1)*100
svychisq(~v101+ari,design=design1,data=kr) #sig

round(svytable(~v101+v161,design=design1))  #division
round(svytable(~v101,design=design1))
prop.table(svytable(~v101,design=design1))*100
prop.table(svytable(~v101+v161,design=design1), margin=1)*100
svychisq(~v101+v161,design=design1,data=kr) #sig

#graph2
#tiff("Map.tiff", units="in", width=6, height=5, res=300)
q <- readShapeSpatial('bgd_admbnda_adm1_bbs_20180410.shp')
q_1 <- fortify(q)


ARI <- read.csv("E:\\Study\\ResearchProject\\ARI_BioMass\\ARI.csv")

q_1$prev <- ifelse(q_1$id==0,ARI$ARI[1],
                   ifelse(q_1$id==1,ARI$ARI[2],
                          ifelse(q_1$id==2,ARI$ARI[3],
                                 ifelse(q_1$id==3,ARI$ARI[4],
                                        ifelse(q_1$id==4,ARI$ARI[5],
                                               ifelse(q_1$id==5,ARI$ARI[6],
                                                      ifelse(q_1$id==6,ARI$ARI[7],ARI$ARI[8])))))))


centroids.df <- as.data.frame(coordinates(q))
names(centroids.df) <- c("Longitude", "Latitude")
centroids.df$name <- c('    Barisal',
                       'Chittagong\n',
                       'Dhaka','Khulna\n',
                       'Mymensingh\n',
                       'Rajshahi',
                       'Rangpur\n','Sylhet')


x <- ggplot(q_1, aes(x=long, y=lat)) + geom_polygon(aes(group=group,fill=prev),colour= "lightgrey")+coord_map()+
  geom_text(data=centroids.df,aes(label = name, x = Longitude, y = Latitude),color='black',size=3.5)+
  scale_fill_distiller(name='ARI (%)',palette ="YlOrRd", direction=1)+
  theme(legend.text = element_text(size = 8))+
  theme_void()

x


#graph2
q <- readShapeSpatial('bgd_admbnda_adm1_bbs_20180410.shp')
q_1 <- fortify(q)


ARI <- read.csv("E:\\Study\\ResearchProject\\ARI_BioMass\\BF.csv")

q_1$prev <- ifelse(q_1$id==0,ARI$ARI[1],
                   ifelse(q_1$id==1,ARI$ARI[2],
                          ifelse(q_1$id==2,ARI$ARI[3],
                                 ifelse(q_1$id==3,ARI$ARI[4],
                                        ifelse(q_1$id==4,ARI$ARI[5],
                                               ifelse(q_1$id==5,ARI$ARI[6],
                                                      ifelse(q_1$id==6,ARI$ARI[7],ARI$ARI[8])))))))


centroids.df <- as.data.frame(coordinates(q))
names(centroids.df) <- c("Longitude", "Latitude")
centroids.df$name <- c('    Barisal',
                       'Chittagong\n',
                       'Dhaka','Khulna\n',
                       'Mymensingh\n',
                       'Rajshahi',
                       'Rangpur\n','Sylhet')


y <- ggplot(q_1, aes(x=long, y=lat)) +geom_polygon(aes(group=group,fill=prev),colour= "lightgrey")+coord_map()+
  geom_text(data=centroids.df,aes(label = name, x = Longitude, y = Latitude),color='black',size=3.5)+
  scale_fill_distiller(name='Solid Fuel (%)',palette ="YlOrRd", direction=1)+
  theme(legend.text = element_text(size = 8))+
  theme_void()
y
tiff("Map.tiff", units="in", width=6, height=6, res=300)
gridExtra::grid.arrange(x,y,ncol=2)
dev.off()

#tiff("FT.tiff", units="in", width=6, height=5, res=300)
df <- data.frame(value = c(0.4, 19.8, 0, 0.1, 43.8,0.6,27.9,7.1,0.1),
                 fuel_types = c("Electricity", "LPG", "Kerosene", 
                           "Charcoal", "Wood", "Straw", "crop", 
                           "dung", "Other"))

library(ggplot2)


x <- ggplot(df, aes(x = "", y = value, fill = fuel_types)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") + labs(fill = "Fuel Types")+ labs(y = "",
                                                               x = "",
                                                               colour = "")


x
df <- data.frame(value = c(86.3, 13.7),
                 fuel_types = c("Solid fuel", "Clean fuel"))

library(ggplot2)

y <- ggplot(df, aes(x = "", y = value,  fill = fuel_types)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") + labs(fill = "Fuel Types")+ labs(y = "",
                                                             x = "",
                                                             colour = "")  + theme( legend.title = element_text(color = "Black", size = 18),
                                                                                    legend.text = element_text(color = "Black", size = 18),
                                                                                    legend.position="bottom")
y
tiff("pie.tiff", units="in", width=6, height=6, res=300)
gridExtra::grid.arrange(x,y,ncol=2)
dev.off()



# df2 <- data.frame(ARI=rep(c("Yes", "No"), each=7),
#                   Types=rep(c("Electricity or gas", "Kerosene", "Charcoal",
#                               "Wood/straw", "Animal dung", "Other fuel","No food cooked in household"),2),
#                   Percentage=c(2.0,0.0,0.0,3.4,2.3,0,0,98.0,100.0,100.0,96.6,97.7,100,100))
# head(df2)
# 
# ggplot(data=df2, aes(x=, y=Percentage, fill=ARI)) +
#   geom_bar(stat="identity", position=position_dodge())+
#   geom_text(aes(label=Percentage), vjust=-1, color="black",
#             position = position_dodge(0.9), size=5)+
#   scale_fill_brewer(palette="Paired")+
#   theme_minimal()



df <- data.frame(dose=c("Electricity or gas", "Wood/straw", "Animal dung"),
                 len=c(2.0,3.4,2.3))
head(df)


x <-ggplot(data=df, aes(x=reorder(dose, -len), y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=-1, color="black", size=8)+
  theme_minimal()+
  xlab("Cooking fuel") + ylab("Symptoms of ARI (%)") + theme(legend.title = element_text(color = "Black", size = 18),
                                                                          legend.text = element_text(color = "Black", size = 18),
                                                                          text = element_text(size = 18))



x
tiff("Pie.tiff", units="in", width=20, height=15, res=300)
gridExtra::grid.arrange(x,y,ncol=2)
dev.off()

