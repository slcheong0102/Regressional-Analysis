********************************************************************************
**************** ECO375 Homework 2 (Cheong Siu Lun, Chris) *********************
********************************************************************************

// Setting up Working Directory

cd "C:\Users\soulu\OneDrive - University of Toronto\University of Toronto\UTM 2021-2023\7_2023 Fall\ECO375H5F\Homework\Homework 2"
use "eco375hw2data2023", clear


*****************************
// Question 3: Data from the US
*****************************
// (a) Estimate the following model using OLS: ln(wage) = β0 + β1*age + u. State the estimated β1 hat using words a non-economist could understand; do not use causal language. Answer twice: once using an approximation, and once using an exact answer.

gen lwage = ln(incwage)
reg lwage age


// (b) What is the t-statistic when testing whether β1=.02 in Equation 3?

gen tstat3b = (_b[age]-0.02)/_se[age]
gen df = e(N)-1-1
di invttail(df, 0.05/2)
di tstat3b


// (c) Now, estimate the following model using OLS: ln(wage) = β0 + β1age + β2age2 + β3age3 + u. Using an approximation, based on these results, how much does income increase or decrease with age when someone is 25? What about when they are 65?

// Step 1: Generate new variables and run regression
gen age2 = age^2
gen age3 = age^3
reg lwage age age2 age3

// Step 2: Calculate the approximate income increase when i) age=25 and ii) age=65 
di (_b[age] + 2*_b[age2]*25 + 3*_b[age3]*(25^2))*100
di (_b[age] + 2*_b[age2]*65 + 3*_b[age3]*(65^2))*100


// (d) Test whether log income varies non-linearly with age in Equation 4. Can you reject the null of linearity at the 5% level?

// H0: β2=0 and β3=0 (Testing 2 Linear Restrictions)
// Step 1: Find the Sum of Squared Residuals (SSR) of the restricted model
reg lwage age
gen SSRr =  e(rss)

// Step 2: Find the SSR of unrestricted model
reg lwage age age2 age3
gen SSRur = e(rss)

// Step 3: Find the F-statistic with q=2 and n-k-1=n-3-1=n-4
gen df1 = 2
gen df2 = e(N)-3-1
gen fstat3d = ((SSRr-SSRur)/df1)/(SSRur/df2)

// Step 4: Compare the F-statistic with the critical value at 5% level
di fstat3d
di invF(df1, df2, 0.05)

// Step 5: Verify the answer from step 1-4 with the following F Test
reg lwage age age2 age3
di fstat3d
test age2 age3


// (e) Now, estimate the following model using OLS: ln(wage) = β0 + β1age + β2own + β3age × own + u. Should estimated β's be interpreted causally? Why or why not? Refer to the assumption(s) we discussed in class. Then, state the estimated beta 3 hat using words a non-economist could understand; use an approximation, and use causal language if and only if the estimates should be interpreted causally.

reg lwage age own c.age#c.own


// (f) How much faster or slower do wages rise with age for people in the construction industry versus those in all other industries? (Use an approximation here.)

gen construction = 0
replace construction = 1 if indly == 770
reg lwage age c.construction#c.age


// (g) By how many standard deviations is log income higher for men than for women in this data?
gen male = 0
replace male = 1 if sex == 1
egen lwage_mean = mean(lwage)
egen lwage_sd = sd(lwage)
gen lwage_standardized = (lwage - lwage_mean)/(lwage_sd)
reg lwage_standardized male


// (h) Using the data, briefly present and describe one fact that you think is interesting. Your answer to this question must actually use Stata code and the data to find a fact that was not discussed in a previous question.
reg lwage age male c.age#c.male


*****************************
// Question 4: Monte Carlo Simulation
*****************************
// (a) Define a new variable γ ≡ β1 × β2. Estimate γ using ˆγ = βˆ1 × βˆ2 within each simulation, for each set of estimates. Create a table like the following with average estimates of βˆ1, βˆ2, and ˆγ based on 5 observations or 100 observations.

// For 5 observations
// Step 1: Clear working directory
clear all
set more off

// Step 2: Define a simulation program "rg_4"
drop _all
program define rg_4, rclass
	version 14.2
	syntax [, obs(integer 100)]
	clear
	quietly set obs `obs'
	gen x1 = rnormal(0,1)
	gen v = rnormal(0,2)
	gen x2 = x1 + v
	gen u = runiform(-6,6)
	gen y = 3 + 4*x1 + 2*x2 + u
	qui regress y x1 x2
	return scalar b1 = _b[x1]
	return scalar b2 = _b[x2]
	return scalar se1 = _se[x1]
	return scalar se2 = _se[x2]
end

// Step 3: Simulate the program 10,000 times with 5 observations
set seed 123
simulate b1_4a_obs5 = r(b1) b2_4a_obs5 = r(b2) gamma_4a_obs5 = (r(b1)*r(b2)), reps(10000): rg_4, obs(5)
sum b1_4a_obs5
sum b2_4a_obs5
sum gamma_4a_obs5

// Step 4: Simulate the program 10,000 times with 100 observations
set seed 123
simulate b1_4a_obs100 = r(b1) b2_4a_obs100 = r(b2) gamma_4a_obs100 = (r(b1)*r(b2)), reps(10000): rg_4, obs(100)
sum b1_4a_obs100
sum b2_4a_obs100
sum gamma_4a_obs100


// (c) Now, for each regression, test H0 : β1 = 4 against H1 : β1 ̸= 4. Conduct this test in two ways: first with a small-sample t-test, and second with the asymptotic t-test. For each test, how often do we reject the null at a 5% significance level? Create tables like the following with the critical values for the tests and the fraction of observations in which we reject the null hypothesis.
// Step 1: Display the Small Sample and Asymptotic Critical Values
di invttail(5-2-1, 0.05/2)
di invttail(100-2-1, 0.05/2)
di invnorm(0.05/2)

// Step 2: Simulate the program for 5 observations
set seed 123
simulate b1_4c_obs5 = r(b1) se1_4c_obs5 = r(se1), reps(10000): rg_4, obs(5)
gen tstat_obs5 = (b1_4c_obs5 - 4)/se1_4c_obs5
gen t_obs5 = 0
gen z_obs5 = 0
replace t_obs5 = 1 if abs(tstat_obs5) >= abs(invttail(2,0.05/2))
replace z_obs5 = 1 if abs(tstat_obs5) >= abs(invnorm(0.05/2))

tab t_obs5 //tab counts the frequencies of 0 and 1
tab z_obs5

// Step 3: Simulate the program for 100 observations
set seed 123
simulate b1_4c_obs100 = r(b1) se1_4c_obs100 = r(se1), reps(10000): rg_4, obs(100)
gen tstat_obs100 = (b1_4c_obs100 - 4)/se1_4c_obs100
gen t_obs100 = 0
gen z_obs100 = 0
replace t_obs100 = 1 if abs(tstat_obs100) >= abs(invttail(97,0.05/2))
replace z_obs100 = 1 if abs(tstat_obs100) >= abs(invnorm(0.05/2))

tab t_obs100
tab z_obs100


























