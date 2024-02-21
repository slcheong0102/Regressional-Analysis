# ----- Setup -----
rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
url="https://mcs.utm.utoronto.ca/~nosedal/data/tax-forms.txt"

taxes=read.table(file=url, header=TRUE);

# ----- Question 1: Compute Sum of Squares between Treatments -----
combined_data <- unlist(taxes)
SSE_R <- 119*var(combined_data)
var_1 <- var(taxes$Form1)
var_2 <- var(taxes$Form2)
var_3 <- var(taxes$Form3)
var_4 <- var(taxes$Form4)
SSE_C <- 29*(var_1 + var_2 + var_3 + var_4)
round(SSE_R - SSE_C, 4)

# ----- Question 2: Compute the Mean Square Between Treatments -----
MSE_NUM <- (SSE_R - SSE_C)/3

# ----- Question 3: Compute the Sum of Squares due to Error -----
round(SSE_C, 4)

# ----- Question 4: Compute the Mean Square due to Error -----
MSE_DENOM <- 111479.8667/(120-4-1)

# ----- Question 5: Compute the F Statistics -----
MSE_NUM/MSE_DENOM
2821.3667/969.3901
