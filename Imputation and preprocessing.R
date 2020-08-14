## Imputation and preprocessing

## Load necessary packages
library(DataCombine)
library(mice)
library(zoo)
library(dplyr)

## Load Data
pdat2<-read.csv("~/pdat2.csv")

pdat2=pdat2[c(-1,-2)]

## Specify data structure for imputation
MImodel = pdat2[c(-1,-2)]
MImodel[,1] = as.factor(MImodel[,1]) 
MImodel[,2] = as.factor(MImodel[,2])
MImodel[,3] = as.factor(MImodel[,3])
MImodel[,4] = as.factor(MImodel[,4])
MImodel[,5] = as.factor(MImodel[,5])
MImodel[,6] = as.factor(MImodel[,6])
MImodel[,7] = as.factor(MImodel[,7])
MImodel[,10] = as.factor(MImodel[,10])
MImodel[,14] = as.factor(MImodel[,14])
MImodel[,15] = as.factor(MImodel[,15])
MImodel[,16] = as.factor(MImodel[,16])
MImodel[,17] = as.factor(MImodel[,17])
MImodel[,18] = as.factor(MImodel[,18])
MImodel[,19] = as.factor(MImodel[,19])
MImodel[,20] = as.factor(MImodel[,20])
MImodel[,22] = as.factor(MImodel[,22])

## Run imputation, save imputed data to global environment
m = 5
imp = mice(MImodel,m = m)

k=24

qhat = matrix(NA, nrow = m,ncol = k)

u = array(NA,dim = c(k,k,m))

for (i in 1:m) {
  data.impute = mice::complete(imp,action = i)}

all_coping_talkfamily.imp = as.factor(as.factor(data.impute[,1]))
all_coping_talkMH.imp = as.factor(as.factor(data.impute[,2]))
all_coping_copingthought.imp = as.factor(as.factor(data.impute[,3]))
all_coping_thinking.imp = as.factor(as.factor(data.impute[,4]))
all_coping_talkfriend.imp = as.factor(as.factor(data.impute[,5]))
all_coping_dis_rel_combined.imp = as.factor(as.factor(data.impute[,6]))
all_coping_crisisline.imp  = as.factor(as.factor(data.impute[,7]))
coping_sum.imp = data.impute[,8]
coping_sum_3avg.imp  = data.impute[,9]
all_coping_any.imp = as.factor(as.factor(data.impute[,10]))
SIUrge.imp  = data.impute[,11]
Self_Efficacy.imp = data.impute[,12]

pdat2.imp<-data.frame(all_coping_talkfamily.imp = all_coping_talkfamily.imp, 
                      all_coping_talkMH.imp =all_coping_talkMH.imp, 
                      all_coping_copingthought.imp = all_coping_copingthought.imp,
                      all_coping_thinking.imp = all_coping_thinking.imp, 
                      all_coping_talkfriend.imp = all_coping_talkfriend.imp,
                      all_coping_dis_rel_combined.imp = all_coping_dis_rel_combined.imp,
                      all_coping_any.imp = all_coping_any.imp, 
                      SIUrge.imp = SIUrge.imp, 
                      Self_Efficacy.imp = Self_Efficacy.imp, 
                      coping_sum_3avg.imp = coping_sum_3avg.imp, 
                      coping_sum.imp = coping_sum.imp)

## Splines: Restructure Data file to appropriately model variables 
na.df <- data.frame(SIUrge.imp = NA)
vars<-c("SIUrge.imp")
pdat2.1<-pdat2.imp[vars]
pdat2.2<- do.call(rbind, apply(pdat2.1, 1, function(x) {rbind(x, na.df)}))

na.df <- data.frame(all_coping_talkfamily.imp = NA, 
                    all_coping_talkMH.imp = NA, 
                    all_coping_copingthought.imp = NA,
                    all_coping_thinking.imp = NA,
                    all_coping_talkfriend.imp = NA,
                    all_coping_dis_rel_combined.imp = NA,
                    all_coping_any.imp = NA, 
                    Self_Efficacy.imp = NA,
                    coping_sum.imp = NA,
                    coping_sum_3avg.imp = NA)

vars<-c("all_coping_talkfamily.imp", 
        "all_coping_talkMH.imp", 
        "all_coping_copingthought.imp",
        "all_coping_thinking.imp",
        "all_coping_talkfriend.imp",
        "all_coping_dis_rel_combined.imp",
        "all_coping_any.imp",
        "Self_Efficacy.imp",
        "coping_sum.imp",
        "coping_sum_3avg.imp")
pdat2.3<-pdat2.imp[vars]
pdat2.4 <- do.call(rbind, apply(pdat2.3, 1, function(x) {rbind(na.df, x)}))

pdat2.imp<-cbind(pdat2.4, pdat2.2)
pdat2.imp$Day<-c(1:56)

## Splines: Create Splines
pdat2.imp<-data.frame(na.spline(pdat2.imp))
pdat2.imp<-round(pdat2.imp)
pdat2.imp$SIUrge.imp[pdat2.imp$SIUrge.imp>7]<-7
pdat2.imp$SIUrge.imp[pdat2.imp$SIUrge.imp<0]<-0
pdat2.imp$Self_Efficacy.imp[pdat2.imp$Self_Efficacy.imp>10]<-10
pdat2.imp$Self_Efficacy.imp[pdat2.imp$Self_Efficacy.imp<0]<-0
pdat2.imp$all_coping_talkMH.imp[pdat2.imp$all_coping_talkMH.imp==-2]<-0

cols <- c("all_coping_talkfamily.imp", 
          "all_coping_talkMH.imp", 
          "all_coping_copingthought.imp",
          "all_coping_thinking.imp",
          "all_coping_talkfriend.imp",
          "all_coping_dis_rel_combined.imp",
          "all_coping_any.imp")

pdat2.imp <- pdat2.imp %>% 
  mutate_at(c(cols),list(~recode(., `-3`=0,`-2`=0,`-1`=0,`2`= 1,`3`=1)))

pdat2.imp[cols] <- lapply(pdat2.imp[cols], factor) 

## Extract Standardized Residuals. Save to global environment

mod1<-lm(coping_sum_3avg.imp~Day, data=pdat2.imp)
Coping_sum_3avg_resid = as.numeric(rstandard(mod1))

mod2<-glm(all_coping_talkfamily.imp~Day, data=pdat2.imp, family=binomial(link="logit"))
Coping_TalkFamily_resid<-as.numeric(rstandard(mod2))

mod3<-glm(all_coping_talkMH.imp~Day, data=pdat2.imp, family=binomial(link="logit"))
Coping_TalkMH_resid<-as.numeric(rstandard(mod3))

mod4<-glm(all_coping_any.imp~Day, data=pdat2.imp, family=binomial(link="logit"))
Coping_any_resid<-as.numeric(rstandard(mod4))

mod5<-glm(all_coping_dis_rel_combined.imp~Day, data=pdat2.imp, family=binomial(link="logit"))
Coping_dis_rel_resid<-as.numeric(rstandard(mod5))

mod6<-glm(all_coping_copingthought.imp~Day, data=pdat2.imp, family=binomial(link="logit"))
Coping_Thought_resid<-as.numeric(rstandard(mod6))

mod7<-glm(all_coping_thinking.imp~Day, data=pdat2.imp, family=binomial(link="logit"))
Coping_Thinking_resid<-as.numeric(rstandard(mod7))

mod8<-lm(coping_sum.imp~Day, data=pdat2.imp, na.action=na.exclude)
Coping_sum_resid<-as.numeric(rstandard(mod8))

mod9<-glm(all_coping_talkfriend.imp~Day, data=pdat2.imp, family=binomial(link="logit"))
Coping_TalkFriend_resid<-as.numeric(rstandard(mod9))

mod10<-lm(SIUrge.imp~Day, data=pdat2.imp)
SIUrge_resid<-as.numeric(rstandard(mod10))

mod11<-lm(Self_Efficacy.imp~Day, data=pdat2.imp)
Efficacy_resid<-as.numeric(rstandard(mod11))

pdat2<-as.data.frame(cbind(Coping_TalkFamily_resid, Coping_TalkMH_resid, Coping_dis_rel_resid, 
                           Coping_Thought_resid, Coping_Thinking_resid, Coping_TalkFriend_resid, 
                           Coping_sum_resid, Efficacy_resid, SIUrge_resid, Coping_any_resid, 
                           Coping_sum_3avg_resid))

## Calculate Difference Scores
attach(pdat2)
Coping_TalkFamily_resid=diff(Coping_TalkFamily_resid)
Coping_TalkMH_resid = diff(Coping_TalkMH_resid)
Coping_dis_rel_resid = diff(Coping_dis_rel_resid)
Coping_Thought_resid = diff(Coping_Thought_resid)
Coping_Thinking_resid = diff(Coping_Thinking_resid)
Coping_TalkFriend_resid = diff(Coping_TalkFriend_resid)
Coping_any_resid = diff(Coping_any_resid)
Coping_sum_resid = diff(Coping_sum_resid)
SIUrge_resid =  diff(SIUrge_resid)
Efficacy_resid=diff(Efficacy_resid)
Coping_sum_3avg_resid = diff(Coping_sum_3avg_resid)

pdat2<-as.data.frame(cbind(Coping_TalkFamily_resid, 
                           Coping_TalkMH_resid, Coping_dis_rel_resid, 
                           Coping_Thought_resid, Coping_Thinking_resid, 
                           Coping_TalkFriend_resid, Coping_sum_resid, 
                           Efficacy_resid, SIUrge_resid, Coping_sum_3avg_resid))

pdat2$day<-c(1:111)