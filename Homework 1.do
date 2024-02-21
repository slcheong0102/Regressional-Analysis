********************************************************************************
**************** ECO375 Homework 1 (Cheong Siu Lun, Chris) *********************
********************************************************************************


// Setting up Working Directory
cd "C:\Users\soulu\OneDrive - University of Toronto\University of Toronto\UTM 2021-2023\7_2023 Fall\ECO375H5F\Homework\Homework 1"
use "eco375hw1data2023", clear


*****************************
// Question 4: Google Trends
*****************************
// (a) Present a table showing the mean and standard deviation of normalized search interest in each city name in the data set.

describe
summarize montreal vancouver toronto

/* 
Since with the code "describe", we notice that there are 3 city names displayed, therefore we can use the code "summarize montreal vancouver toronto" to display a table summarizing the mean of standard deviation (sd) of each city name. Hence, the results are as follow:
1. montreal
	mean = 13.53846
	sd = 26.13623
2. vancouver
	mean = 12.46154
	sd = 26.96793
3. toronto
	mean = 19.53846
	sd = 24.46976
*/


// (b) Use OLS to estimate β1 in: rain = β0 + β1umbrella + u. Report the estimate β_1 hat in words a non-economist could understand. Do not use causal language. Should we think of this as an estimate of the causal effect of searches for umbrellas on searches for rain? Why or why not?

regress rain umbrella
matrix list e(b)

/* 
The code "regress rain umbrella" constructs a simple linear regression analysis with dependent variable (y) as rain and explanatory variable (x) as unbrella. We can display the matrix of coefficient estimates with the code "matrix list e(b)". Here, we can see that beta 1 hat is 0.62796504. This implies that for every 1 unit increase in the normalized search interest of "umbrella", there is a 0.62796504 unit increase in the normalized search interest of "rain". 

No, we should not think this as an estimate of the causal effect between searches of unbrellas and rain. The reason is that beta 1 hat in this case only implies a positive correlation and correlation is not equivalent to causality. There may be omitted variables, reverse causality, or even endogeneity that disagrees with the hypothesis of causality of rain and umbrella. In other words, we do not have enough information to conclude causality with only beta 1 hat.
*/


// (c) Create a new variable, aboveavgcold, that is equal to 1 if the location has normalized search interest above that variable's average, and 0 otherwise. Use a regression to estimate the difference between normalized search interest for pizza when normalized search interest for cold is above or below average. Is this difference significantly different from 0 at the 5% level?

egen cold_mean = mean(cold)
gen aboveavgcold = 0
replace aboveavgcold = 1 if cold > cold_mean
reg pizza aboveavgcold

/* 
Step 1: egen cold_mean = mean(cold)
This code generates a new column with the average value of the normalized search interest of "cold" in every entry.

Step 2: gen aboveavgcold = 0
This code creates a new variable "aboveavgcold" and input all entries with 0 initially.

Step 3: replace aboveavgcold = 1 if cold > cold_mean
This code replaces the entries of aboveavgcold if it satisfies the condition the value of cold is greater than that of cold_mean.

Step 4: reg pizza aboveavgcold
This code performs an ols regression on a binary explanatory variable. The estimate of beta 1 hat in the result is 5.6. This value is the estimate of the treatment effect, i.e. te_i = y_i(1)-y_i(0). Since the p-value is 0.634 > 0.05 (significant level), we fail to reject the null hypothesis (H0), i.e. the difference between y(1) and y(0) is not significantly different from 0 (beta 1 = te = 0).
*/


// (d) Use OLS to estimate β1 in each of the following equations. Report your estimate for each using words a non-economist could understand. Use causal language for this part (even though the results may not actually be causal).
/* 
poutine = β0 + β1 ln(montreal) + u
ln(poutine) = β0 + β1montreal + u
ln(poutine) = β0 + β1 ln(montreal) + u
*/

gen montreal_ln = ln(montreal)
gen poutine_ln = ln(poutine)
reg poutine montreal_ln
reg poutine_ln montreal
reg poutine_ln montreal_ln

/* 
Step 1: gen montreal_ln = ln(montreal) & gen poutine_ln = ln(poutine)
These 2 codes generate a new variable by taking the natural logrithm of the variable inside the ln() function.

Step 2: reg y x; where y = poutine or poutine_ln and x = montreal or montreal_ln
This code runs an ols regression on the specified dependent and explanatory variables subject to the question.

1. For poutine = β0 + β1 ln(montreal) + u
code: reg poutine montreal_ln
Here, we can see that the beta 1 hat is 20.5043. This implies that for every 1% increase in the normalized search interest in "montreal", there is a 0.01*20.5043 = 0.205043 unit increase in the normalized search interest of "poutine".

2. For ln(poutine) = β0 + β1montreal + u
code: reg poutine_ln montreal
Here, we can see that the beta 1 hat is 0.015788. This implies that for every 0.01 unit increase in the normalized search interest in "montreal", there is a 0.015788% increase in the normalized search interest of "poutine".

3. For reg poutine_ln montreal_ln
code: reg poutine_ln montreal_ln
Here, we can see that the beta 1 hat is 0.412179. This implies that for every 1% increase in the normalized search interest in "montreal", there is a 0.412179% increase in the normalized search interest of "poutine".
*/


// (e) Using the data, briefly present and describe one fact that you think is interesting. Your answer to this question must actually use Stata code and the data to find a fact that was not discussed in a previous question.

twoway (scatter poutine montreal_ln) (lowess poutine montreal_ln)
graph export "Q4e_poutine montreal_ln.pdf", replace
twoway (scatter poutine_ln montreal) (lowess poutine_ln montreal)
graph export "Q4e_poutine_ln montreal.pdf", replace
twoway (scatter poutine_ln montreal_ln) (lowess poutine_ln montreal_ln)
graph export "Q4e_poutine_ln montreal_ln.pdf", replace

/*
The above codes create a layered graph of scatter plot and lowess plot. We can see that from the graphs generated, "poutine_ln montreal_ln.pdf" seems to have a better overall linear relationship than the other 2 graphs. In this case, using poutine_ln and montreal_ln seems to be the best variables (logrithm transformation) to run an ols regression on.
*/



*****************************
// Question 5: Monte Carlo Simulation
*****************************
// (a) Simulate the model once in Stata. Generate a data set {yi, xi: i = 1, ..., n} with n = 400 observations. Show a scatterplot of X and Y . Use OLS to estimate β0 and β1; what are your estimates?

// Step 1: Clear Working Directory and Set Seed
clear all
set seed 123

// Step 2: Create Simulation Program "rg_5a"
cap program drop rg_5a
program rg_5a, rclass
drop _all
set obs 400
gen x = rnormal(-1,1)
gen u = rnormal(0,1)
gen y = 2 + 3*x + u
qui regress y x
return scalar b0_5a = _b[_cons]
return scalar b1_5a = _b[x]
end

// Step 3: Simulate rg_5a with 1 Repitition to Estimate β0 and β1
simulate "rg_5a" b0_5a = r(b0_5a) b1_5a = r(b1_5a), reps(1)
di b0_5a 
di b1_5a

// Step 4: Construct Scatterplot of x and y with the Same Seed 123
set seed 123
set obs 400
gen x = rnormal(-1,1) //generate variable x
gen u = rnormal(0,1)  //generate variable u
gen y = 2 + 3*x + u   //generate variable y
scatter y x, title("Scatterplot of X and Y")
graph export "Q5a_Scatterplot of X and Y.pdf", replace

/*
1. Generate a data set {yi, xi: i = 1, ..., n} with n = 400 observations
Since we will be using the same seed "seed 123", this psuedo-random number generator starting point will ensure us to be obtaining the same results in both the simulation program "rg_5a" and the one run in Step 4 above.

- code: set seed 123
This code set the same seed for both rg_5a and Step 4

- code: set obs 400
This code set the number of observations to be taken from the random number generator to be 400.

- codes: gen x = rnormal(-1,1) ; gen u = rnormal(0,1) ; gen y = 2 + 3*x + u 
These codes generate the required data frame containing {yi, xi}. We can see that x follows a normal distrubution with mean=-1 and standard deviation=1, while u follows a normal distribution with mean=0 and standard deviation=1.

2. Show scatterplot of X and Y
- code: scatter y x, title("Scatterplot of X and Y")
This code creates a scatterplot with y on the Y-axis and x on the X-axis.

-code: graph export "Q5a_Scatterplot of X and Y.pdf", replace
This code exports a pdf version of the graph and will replace the graph every time the code is run.

3. Simulate the model once and use ols to estimate β0 and β1
In step 2, we created a program named "rg_5a" to run the simulations n times. Since the question is asking to simulate once only, therefore n=1 (i.e. reps(1)). In the program, we would follow the same intuition as the above parts by setting seed and creating the variables x, y and u with 400 observations. Then, we will regress y against x, returning the beta 0 hat and beta 1 hat in scalar form before ending the program.

- code: simulate "rg_5a" b0_5a = r(b0_5a) b1_5a = r(b1_5a), reps(1)
This code simulates the program "rg_5a" and store b0_5a as b0_5a and b1_5a as b1_5a.

- code: di b0_5a ; di b1_5a
These 2 codes return the beta 0 hat and beta 1 hat of the program with 1 repetition. The beta 0 hat is 2.1037087, and the beta 1 hat is 3.0248418
*/


// (b) Now, simulate this model 200 times in Stata. For each simulation, generate adata set {yi, xi: i = 1, ..., n} with n = 200 observations. Then, for each sample, estimate β0 and β1 using OLS and save the results βˆ0 and βˆ1. What are the averages and the standard deviations of βˆ0 and βˆ1 across the simulations? Are βˆ0 and βˆ1 close to the true β0 and β1? Why should we expect that result? Plot the histograms of βˆ0 and βˆ1.

// Step 1: Clear Working Directory and Set Seed
clear all
set seed 123

// Step 2: Create Simuation Program "rg_5b"
cap program drop rg_5b
program rg_5b, rclass
drop _all
set obs 200
gen x = rnormal(-1,1)
gen u = rnormal(0,1)
gen y = 2 + 3*x + u
qui regress y x
return scalar b0_5b = _b[_cons]
return scalar b1_5b = _b[x]
end

// Step 3: Simulate rg_5b 200 times
simulate "rg_5b" b0_5b = r(b0_5b) b1_5b = r(b1_5b), reps(200)

// Step 4: Find the Averages and Standard Deviations of Beta 0 Hat and Beta 1 Hat
summarize b0_5b, detail
summarize b1_5b, detail

// Step 5: Plot the Hitograms of Both Beta 0 Hat and Beta 1 Hat
hist b0_5b, normal
graph export "Q5b_b0_5b.pdf", replace
hist b1_5b, normal
graph export "Q5b_b1_5b.pdf", replace

/*
From Step 4, we can see that results as follow:
1. Beta 0 Hat (b0_5b)
- Average/Mean: 1.994573
- Standard Deviation: 0.0992975

2. Beta 1 Hat (b_1_5b)
- Average/Mean: 2.99587
- Standard Deviation: 0.0758896

Yes, the beta 0 hat and beta 1 hat this time are closer to the true beta 0 and beta 1 than that estimated in 5a. The reason is that even though we reduced the number of observations for each regression from 400 to 200, however, 5a only performed 1 regression whereas 5b performed 200 regressions and obtained 200 estimates of beta 0 hat and beta 1 hat. Therefore, by law of large numbers, the distribution of both beta 0 hat and beta 1 hat will coverge normally to the true beta 0 and beta 1 where the averages obtained as n increases will converge in probability to the true parameters; this is supported by the diminishing variance/standard deviation of both beta 0 hat and beta 1 hat.
*/


// (c) With a bit of algebra, we can see that X = −2/3 + 1/3*Y − 1/3*U. Now, again simulate the same model as above (Y = β0+β1X+U), 200 times, and for each simulation, generate a data set {yi, xi: i = 1, ..., n} with n = 400 observations. This time, though, you should use OLS to estimate γ0 and γ1 in the regression X = γ0 + γ1Y + V . (That is, regression X against Y rather than the other way around.) What are the averages and standard deviations of ˆγ0 and ˆγ1, the OLS estimates? Are ˆγ0 and ˆγ1 usually close to −2/3 and 1/3? Why should we expect that result?

// Step 1: Clear Working Directory and Set Seed
clear all
set seed 123

// Step 2: Create Simuation Program "rg_5c"
cap program drop rg_5b
program rg_5c, rclass
drop _all
set obs 400
gen x = rnormal(-1,1)
gen u = rnormal(0,1)
gen y = 2 + 3*x + u
qui regress x y
return scalar b0_5c = _b[_cons]
return scalar b1_5c = _b[y]
end

// Step 3: Simulate rg_5c 200 times
simulate "rg_5c" b0_5c = r(b0_5c) b1_5c = r(b1_5c), reps(200)

// Step 4: Find the Averages and Standard Deviations of Gamma 0 Hat and Gamma 1 Hat
summarize b0_5c, detail
summarize b1_5c, detail
mean b0_5c
mean b1_5c

/*
From Step 4, we can see that results as follow:
1. Gamma 0 Hat (b0_5c)
- Average/Mean: -0.7008598
- Standard Deviation: 0.0176378

2. Gamma 1 Hat (b_1_5c)
- Average/Mean: 0.2993803
- Standard Deviation: 0.0050753

From codes: mean b0_5c and mean b1_5c, we can see that the 95% confidence interval for gamma 0 hat is [-0.7033192, -0.6984004], while the 95% confidence interval for gamma 1 hat is [0.2986726, 0.300088]. We can see that both the true value of gamma 0 (-0.666667) and gamma 1 (0.3333333) fall beyond their respective confidence intervals. This implies that we have to reject the null hypothesis given the model, i.e. the estimators for gamma 0 and gamma 1 are biased. The reason for this is because the ols regression consists of an error term u in which u captures the unexplained variation which are independent of x but not y. Reversing the dependent and independent variable in the regression does not reverse the dependency of u and E[u|x]=0 does not imply E[u|y]=0 as y originally is a function of x and u. Therefore, running a regression of x against y will likely result in a biased estimation on gamma 0 and gamma 1, as corr(y,u)=corr(2+3*x+u,u)=corr(u,u)=1, which is not 0. This violates SLR.4: expectation of u conditioned on the independent variable (in this case y) is 0.
*/

