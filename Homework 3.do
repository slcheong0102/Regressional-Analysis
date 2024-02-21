********************************************************************************
**************** ECO375 Homework 3 (Cheong Siu Lun, Chris) *********************
********************************************************************************

*　Setting up Working Directory

cd "C:\Users\soulu\OneDrive - University of Toronto\University of Toronto\UTM 2021-2023\7_2023 Fall\ECO375H5F\Homework\Homework 3"


*****************************
// Question 4: Monte Carlo Simulation
*****************************
* Define the program hwprog
clear all
set seed 123
cap program drop hwprog
program hwprog, rclass
drop _all
set obs 100
gen u = rnormal(0,1)
gen v = rnormal(0,1)
gen p = rnormal(0,1)
gen z = rnormal(0,1)
gen x = 3 - z + p + v
gen y1 = 1 + 2*x + 0.2*p + (2.2 - 0.2)*u   
gen y2 = 1 + 2*x + 2*p + (2.2 - 2)*u       
foreach samplesize in 10 30 100 {
	reg y1 x if _n <= `samplesize'
	return scalar b11_`samplesize' = _b[x] 
	reg y2 x if _n <= `samplesize'
	return scalar b12_`samplesize' = _b[x] 
	ivregress 2sls y1 (x = z) if _n <= `samplesize'
	return scalar i11_`samplesize' = _b[x] 
	ivregress 2sls y2 (x = z) if _n <= `samplesize'
	return scalar i12_`samplesize' = _b[x] 
}
end

simulate "hwprog" b11_10=r(b11_10) b11_30=r(b11_30) b11_100=r(b11_100) b12_10=r(b12_10) b12_30=r(b12_30) b12_100=r(b12_100) i11_10=r(i11_10) i11_30=r(i11_30) i11_100=r(i11_100) i12_10=r(i12_10) i12_30=r(i12_30) i12_100=r(i12_100), reps(1000)


* (c) Using the results of your simulation, complete the table below four times: showing the average and standard deviation of the estimators of β1, from using OLS and from using IV.

** For reg y1 x (δ = 0.2)
sum(b11_10)
sum(b11_30)
sum(b11_100)

** For reg y2 x (δ = 2)
sum(b12_10)
sum(b12_30)
sum(b12_100)

** For ivregress y1 x (δ = 0.2)
sum(i11_10)
sum(i11_30)
sum(i11_100)

** For ivregress y2 x (δ = 2)
sum(i12_10)
sum(i12_30)
sum(i12_100)


* (d) Using the results above, create one more table with the difference, MSE(β_hat OLS) - MSE(β_hat IV). That is, find the MSE of each estimator, and fill the table with the difference. Note that positive values shows situations where IV is preferred, while negative values shows when OLS is preferred.

** Find the MSE for OLS and IV Estimators in Each Sample Size {10, 30, 100}

** (1) samplesize = 10
** y = y1 (δ = 0.2)
egen OLSMSE1_10 = mean((b11_10 - 2)^2)
egen IVMSE1_10 = mean((i11_10 - 2)^2)
gen diff1_10 = OLSMSE1_10 - IVMSE1_10
di diff1_10

** y = y2 (δ = 2)
egen OLSMSE2_10 = mean((b12_10 - 2)^2)
egen IVMSE2_10 = mean((i12_10 - 2)^2)
gen diff2_10 = OLSMSE2_10 - IVMSE2_10
di diff2_10

** (2) samplesize = 30
** y = y1 (δ = 0.2)
egen OLSMSE1_30 = mean((b11_30 - 2)^2)
egen IVMSE1_30 = mean((i11_30 - 2)^2)
gen diff1_30 = OLSMSE1_30 - IVMSE1_30
di diff1_30

** y = y2 (δ = 2) 
egen OLSMSE2_30 = mean((b12_30 - 2)^2)
egen IVMSE2_30 = mean((i12_30 - 2)^2)
gen diff2_30 = OLSMSE2_30 - IVMSE2_30
di diff2_30

** (3) samplesize = 100
** y = y1 (δ = 0.2)
egen OLSMSE1_100 = mean((b11_100 - 2)^2)
egen IVMSE1_100 = mean((i11_100 - 2)^2)
gen diff1_100 = OLSMSE1_100 - IVMSE1_100
di diff1_100

** y = y2 (δ = 2)
egen OLSMSE2_100 = mean((b12_100 - 2)^2)
egen IVMSE2_100 = mean((i12_100 - 2)^2)
gen diff2_100 = OLSMSE2_100 - IVMSE2_100
di diff2_100





