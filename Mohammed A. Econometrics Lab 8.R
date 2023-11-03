#1.Group Members: Mohammed A. Al Muhaymin, Zakaria Sule, Md Muhibul Islam

#2.
library(plyr)
library(dplyr)
library(tidyverse)
library(haven)
attach(acs2021)

getwd()
setwd("/Users/mohammed.almuhaymin/Documents/AAAAA ECO B2000")
levels_n <- read.csv("IND_levels.csv")
names(levels_n) <- c("New_Level","levels_orig")
acs2021$IND <- as.factor(acs2021$IND)
levels_orig <- levels(acs2021$IND) 
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))

acs2021$public_work <- acs2021$IND 
levels_public <- read.csv("publicwork_recode.csv")
names(levels_public) <- c("levels_orig","New_Level")
levels_new_pub <- join(data.frame(levels_orig),data.frame(levels_public))


levels(acs2021$IND) <- levels_new$New_Level
levels(acs2021$public_work) <- levels_new_pub$New_Level
summary(levels(acs2021$IND))
summary(levels_n)
summary(names(levels))
acs2021$public_work_num <- as.numeric(acs2021$public_work == "work for public, stable")
table(acs2021$public_work,acs2021$public_work_num)
print
#("The subgroup we have chosen to look at is people 25-35, in the labor force, working year round, fulltime.")
#Here we will consider people 25-35, in labor force, working year round, full time, spanish, female,and married.
use_varb <- (acs2021$AGE>=25) & (acs2021$AGE<=35) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 >4) & (acs2021$MARST == 1)
dat_use <- subset(acs2021,use_varb)
summary(dat_use)
#The dat_use has  amount 5944 obs

ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data=dat_use)
summary(ols_out1)
#The P value is very indicating there is some significance, All of the parameters Pr(>|t|) < 0.05

#LAB 7

# from last time:
ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use)
summary(ols_out1)



pred_vals_ols1 <- predict(ols_out1, dat_use)
pred_model_ols1 <- (pred_vals_ols1 > mean(pred_vals_ols1))
table(pred = pred_model_ols1, true = dat_use$public_work_num)

# logit 
model_logit1 <- glm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use, family = binomial)
summary(model_logit1)
pred_vals <- predict(model_logit1, dat_use, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_use$public_work_num)
