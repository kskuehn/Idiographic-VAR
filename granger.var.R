##Load packages
library(vars)

## Load data
pdat2<-read.csv("~/pdat2.var.csv")

## Run model
pdat2.2<-c("SIUrge", "Coping_TalkFamily")
pdat2.2<-pdat2[pdat2.2]
coping_var=VAR(pdat2.2,p=1,  type="const")
causality(coping_var, cause="SIUrge")$Granger
summary(coping_var)