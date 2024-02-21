# ----- Setup -----
rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)

# ----- Question 1: What is the slope (Beta 1) of the SLR -----
city1 <- c(17,20,22,17,19,20,13,18,18,11,17,
          57,20,9,22,16,13,23,20,15,19,25)
highway1 <- c(24,28,31,25,27,27,21,25,23,16,23,
             56,26,13,28,23,19,30,26,22,27,30)
ols1 <- lm(highway1~city1)
summary(ols1)

# ----- Question 2: What is the slope of the SLR without the Insight?
city2 <- c(17,20,22,17,19,20,13,18,18,11,17,
          20,9,22,16,13,23,20,15,19,25)
highway2 <- c(24,28,31,25,27,27,21,25,23,16,23,
             26,13,28,23,19,30,26,22,27,30)
ols2 <- lm(highway2~city2)
summary(ols2)

# ----- Question 3: Which car has the highest leverage value? -----
col1 = c(1,1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,1);
col2 = city1 - mean(city1)*c(1,1,1,1,1,1,1,1,1,1,1,
                             1,1,1,1,1,1,1,1,1,1,1);
X=cbind(col1,col2);
X=matrix(X,nrow=22,ncol=2)
H = X%*%solve(t(X)%*%X)%*%t(X)
H.rounded = round(H,4)
y.hat = H%*%highway1
max.leverage <- max(diag(H))
round(max.leverage,4)

# ----- Question 4: Find the corresponding ordinary residual for the car with the highest hat value -----
# Method 1
e4 = matrix(highway1,ncol=1) - y.hat
e4

# Method 2
I = diag(22)
(I-H)%*%highway1

# Method 3
residuals(ols1)
min(residuals(ols1))

# ----- Question 5: Find the studentized residual for this car -----
studentized = rstandard(ols1)
round(studentized,4)

# ----- Question 6: Deleted studentized residual -----
deleted = rstudent(ols1)
round(deleted,4)

# ----- Question 7: Would you consider obs 12 as an outlier with the deleted studentized residual? -----
residuals(ols1)

x=c(2,4,5,7,2)
y=c(3,4,5,6,7)
mod1 = lm(y~x)
residuals(mod1)
e = residuals(mod1)
MSE = sum(e^2)/mod1$df.residual
