#HW 3: Mohammed A.Al Muhaymin, Luis Melo, Mishal Nawaz

load("~/Downloads/111111111 ECO B2000/Household_Pulse_data_w57.RData")
# note your directory structures might be different so "setwd" would be different on your machine
require(tidyverse)
require(class)
require(caret)
summary(Household_Pulse_data$KIDGETVAC_5_11Y)
# We are looking to create a hypothesis based on data for kids age 5-11 years old and their vaccination status
#Here we are removing NAs
dat_kidvaxx_nonmissing <- subset(Household_Pulse_data, (Household_Pulse_data$KIDGETVAC_5_11Y != "NA") )
# Looks to have gone right, data for subset is generated
# Below will summarize the data subset of kids 5-11 yo, Feel Free to skip
summary(dat_kidvaxx_nonmissing)
# We will now assign a rank value to the data set responses of 5 valuing the Vaccine most and 1 valuing the vaccine least with the unsures set to the middle at 3 
temp1 <- fct_recode(dat_kidvaxx_nonmissing$KIDGETVAC_5_11Y, '5' = 'kids 5-11yo definitely get vaxx',
                    '4'='kids 5-11yo probably get vaxx', '3'='unsure kids 5-11 get vaxx',
                    '2'='kids 5-11yo probably NOT get vaxx', '1'='kids 5-11yo definitely NOT get vaxx',
                    '3'='do not know plans for vaxx for kids 5-11yo')
summary(temp1)
# Data shows a skew to left with majority of data points scoring 1 for not getting vvaxed
# Numeric Factor Conversion, with a scary note NA about the NAs influencing data, however below shows there were no NA data points with 5-11 yo
kidsvax511 <- as.numeric(levels(temp1))[temp1]
summary(kidsvax511)
# Data leans towards most kids 5-11 not having recieved vaccines shown by mean of 1.794 or even with rounding up to nearest category was probably not getting Vaxxed
norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE)  )
}

#Lazy Way aka scaredy cat way aka safe way taught by professor
# We are expecting to find there will be closeness the relationship between persons who are 
# We are classifying the data based on people who receive vaccination and region

data_use_prelim <- data.frame(norm_varb(as.numeric(dat_kidvaxx_nonmissing$RECVDVACC)),norm_varb(as.numeric(dat_kidvaxx_nonmissing$REGION)))

good_obs_data_use <- complete.cases(data_use_prelim,dat_kidvaxx_nonmissing$KIDGETVAC_5_11Y)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(kidsvax511,good_obs_data_use)

# Algorithm training starts
#We kept the same seed and divided 80/20 ratio

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
summary(cl_data)
summary(train_data)

for (indx in seq(1, 9, by= 2)) {
  pred_y <- knn3Train(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_y == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

# regression modeling start

cl_data_n <- as.numeric(cl_data)
summary(as.factor(cl_data_n))
names(train_data) <- c("norm_recvdvax","norm_region")

model_ols_v1 <- lm(cl_data_n ~ train_data$norm_recvdvax + train_data$norm_region)

y_hat <- fitted.values(model_olsv1)

# This model shows that with increasing neighbors, there is an increase in their closeness despite the large coefficient value
mean(y_hat[cl_data_n == 2])
mean(y_hat[cl_data_n == 3])
mean(y_hat[cl_data_n == 4])
mean(y_hat[cl_data_n == 5])

model_ols_v1

cl_data_n2 <- as.numeric(cl_data_n == 2) 

model_ols_v2 <- lm(cl_data_n2 ~ train_data$norm_recvdvax + train_data$norm_region)
y_hat_v2 <- fitted.values(model_ols_v2)

# This model gives different results with lower coefficient values suggesting closer neighbors
mean(y_hat_v2[cl_data_n2 == 1])
mean(y_hat_v2[cl_data_n2 == 0])

model_ols_v2

summary(model_ols_v1)
summary(model_ols_v2)


# all in all perhaps it is easier to choose data without interfering values such as NA where the need to turn qualitative data into quantitative data is removed
#The coefficients show there is no significant correlation between people receiving vaccine and their regions compared to their kids 5-11 years old
#This is proven by having both models showing T values < 1 and P values > 0.05

