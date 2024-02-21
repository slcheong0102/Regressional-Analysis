# ----- Setup -----
rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
fires <- read.table("C:/Users/soulu/OneDrive - University of Toronto/University of Toronto/UTM 2021-2023/7_2023 Fall/STA302H5F/Quizzes and Exams/fire-damage.txt")




# ----- Question 1: Provide the numerical value of the ls estimate pf Beta 1 -----
fires <- slice(fires, -1)
colnames(fires)[colnames(fires) == "V1"] <- "Distance"
colnames(fires)[colnames(fires) == "V2"] <- "Percent"
Distance <- c(
  7.5, 8.3, 6.2, 1.6, 5.6, 6, 4.3, 8.1, 5.7, 0.3, 1.6, 2.5, 5.8, 5.3, 6.3, 3.4, 6.2, 3.2, 6.3, 6.1,
  4.6, 6.7, 0.5, 3.2, 5.3, 5, 4.8, 6.4, 0.2, 4.9, 6.2, 7.6, 6.6, 2.9, 2.1, 4.8, 8.1, 1.2, 4.6, 4,
  6.1, 0.8, 5.9, 6.5, 6.5, 7.5, 7.2, 6.7, 4.1, 4, 4.8, 4, 6.2, 5.5, 7, 7.5, 6.2, 5.7, 7.3, 1.9, 2.8,
  6.3, 9.4, 3.7, 4.9, 1.8, 3.6, 2.9, 2.6, 3.1, 4.7, 5.9, 2.5, 4.6, 5.2, 7.6, 3.7, 7.4, 3.3, 5.1, 5.1,
  7.5, 4.7, 2.7, 2.7
)
Percent <- c(
  68, 66, 34, 30, 70, 62, 47, 72, 40, 53, 18, 48, 53, 48, 64, 52, 61, 34, 65, 66,
  33, 76, 34, 46, 55, 33, 46, 49, 17, 47, 63, 69, 54, 52, 43, 35, 58, 5, 46, 57, 40, 39, 42, 62, 52,
  76, 67, 45, 23, 33, 59, 50, 49, 44, 52, 68, 48, 55, 77, 9, 35, 51, 84, 30, 61, 40, 24, 37, 51, 30,
  47, 54, 47, 52, 52, 52, 33, 74, 56, 53, 54, 76, 37, 45, 50
)
ols1 <- lm(Percent ~ Distance)
summary(ols1)
ols1$coef

# ----- Question 3: Is there sufficient evidence that Distance increases as Percent increases?-----
residuals <- resid(ols1)
n <- length(residuals)
s <- sqrt(sum(residuals^2)/(n-2))
summary(ols1)$coef
b <- summary(ols1)$coef[2,1]
SEb <- summary(ols1)$coef[2,2]
t.statistic <- b/SEb
t.statistic

# ----- Question 4: Estimate the lower confidence limit -----
b-qt(0.975,df=n-2)*SEb

# ----- Question 5: Estimate the upper confidence limit -----
b+qt(0.975,df=n-2)*SEb

# ----- Question 6: Estimate the ucl if Distance = 2 -----
x0 = data.frame(Distance=2)
predict(ols1,x0,interval="confidence")
