#1.Group Members: Mohammed A. Al Muhaymin, Zakaria Sule, Md Muhibul Islam

#2.
library(plyr)
library(dplyr)
library(tidyverse)
library(haven)

load("~/Documents/AAAAA ECO B2000/publicwork_recode.csv")
getwd()
setwd("/Users/mohammed.almuhaymin/Downloads")
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
use_varb <- (acs2021$AGE>=25) & (acs2021$AGE <= 35) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 >4) & (acs2021$UHRSWORK >= 40) & (acs2021$SEX == 2) & (acs2021$RACED == 110) (acs2021$MARST == 1)
dat_use <- subset(acs2021,use_varb)
summary(dat_use)
#The data has 13496 amount

ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use)
summary(ols_out1)
#The P value is very indicating there is some significance, All of the parameters Pr(>|t|) < 0.05


#3.
#We decided to analyze labor market. We are still looking for data and articles. But initial research have led us to find some information.
#Reading a CBS news article we found that there has been a decrease of people in finance. Recent graduates have
#seen how labor market is impacted by pandemic. In addition, the graduates are learning to be versatile to have 
#various career opportunities. The graduates are optimistic that they will find a job.
#https://www.cbsnews.com/news/job-labor-market-college-graduation-season-class-of-2023-in-transition/


#We also read little research article by going to Econ lit about wage differentials in the labor market.Based 
#on the reading, different jobs pay different. The degree is not the only factor that determines the wage in 
#a job. There are other variables such as risk associated with the job. We will continue to find more article
#analyze them for the project. 
#https://pubs.aeaweb.org/doi/pdfplus/10.1257/jep.37.3.189

