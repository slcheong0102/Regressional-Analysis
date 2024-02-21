# ----- Setup -----
rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)

# ----- Question 1: (Same Week) Sample Correlation Coefficient/Coefficient of Determination -----

cases1 <- c(36,531,4233,8682,7164,2229,600,164,57,722,1517,1828,1539,2416,3148,3465,1440)
deaths1 <- c(0,0,130,552,738,414,198,90,56,50,71,137,178,194,290,310,149)
ols1 <- lm(deaths1 ~ cases1)
summary(ols1)
var_cases1 <- var(cases1)
var_deaths1 <- var(deaths1)
sxx1 <- var_cases1*(length(cases1)-1)
syy1 <- var_deaths1*(length(deaths1)-1)
b1_q1 <- summary(ols1)$coef[2,1]
r1_q1 <- b1_q1*sqrt(sxx1/syy1)
r2_q1 <- r1_q1^2

# ----- Question 2: (One Week Earlier) Sample Correlation Coefficient/Coefficient of Determination -----

cases2 <- c(36,531,4233,8682,7164,2229,600,164,57,722,1517,1828,1539,2416,3148,3465)
deaths2 <- c(0,130,552,738,414,198,90,56,50,71,137,178,194,290,310,149)
ols2 <- lm(deaths2 ~ cases2)
summary(ols2)
var_cases2 <- var(cases2)
var_deaths2 <- var(deaths2)
sxx2 <- var_cases2*(length(cases2)-1)
syy2 <- var_deaths2*(length(deaths2)-1)
b1_q2 <- summary(ols2)$coef[2,1]
r1_q2 <- b1_q2*sqrt(sxx2/syy2)
r2_q2 <- r1_q2^2

# ----- Question 3: (Two Weeks Earlier) Sample Correlation Coefficient/Coefficient of Determination -----

cases3 <- c(36,531,4233,8682,7164,2229,600,164,57,722,1517,1828,1539,2416,3148)
deaths3 <- c(130,552,738,414,198,90,56,50,71,137,178,194,290,310,149)
ols3 <- lm(deaths3 ~ cases3)
summary(ols3)
var_cases3 <- var(cases3)
var_deaths3 <- var(deaths3)
sxx3 <- var_cases3*(length(cases3)-1)
syy3 <- var_deaths3*(length(deaths3)-1)
b1_q3 <- summary(ols3)$coef[2,1]
r1_q3 <- b1_q3*sqrt(sxx3/syy3)
r2_q3 <- r1_q3^2

# ----- Question 4: Beta 0 and 1 hat for the best prediction model -----
summary(ols2)$coef[1,1]
summary(ols2)$coef[2,1]

# ----- Question 5: P Value of the T Test -----
n_q5 <- 30
r1_q5 <- -0.45
t_stats_q5 <- sqrt(n_q5-2)*(r1_q5/sqrt(1-r1_q5^2))
df_q5 <- n_q5-2
p_value_left <- pt(t_stats_q5, df_q5, lower.tail = TRUE)
p_value_left

# ----- Question 6: Use ANOVA Table to determine coefficient of determination -----
SSE <- 500
SST <- 1500
r2_q6 <- 1- SSE/SST
