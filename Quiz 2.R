----- # Attempt 1 ------
# Question 1 (What is the MLE estimte for Beta 1)
x = c(77,79,62,65,73,91,68,73,89,74);
y = c(11,9,3,6,7,12,8,7,10,8)
linear.reg = lm(y~x);
coef(linear.reg);

# Question 2 (What is the MLE estimate for Beta 0)
x = c(77,79,62,65,73,91,68,73,89,74);
y = c(11,9,3,6,7,12,8,7,10,8)
linear.reg = lm(y~x);
coef(linear.reg);

# Question 3 (What is the MLE estimate for Sigma^2)
x = c(1,1.1,2.4,0.3,0.5,1,0.6,1.4,1.6,1.4,0.3,0.5);
y = c(36,39,55,26,39,40,46,39,60,49,37,46)
linear.reg = lm(y~x);
coef(linear.reg);

# Question 4 (What is the distribution of Beta)
----- # Attempt 2 -----
# Question 1 (What is the MLE estimate for Beta 1)
x = c(6.5,7.1,7.4,6.4,7.4,8.3,7.4,6.1,5.2,6.2);
y = c(50,63,38,50,50,81,50,44,13,25)
linear.reg = lm(y~x);
coef(linear.reg);

# Question 2 (What is the MLE estimate for Beta 0)
x = c(3.4,3.3,3.4,3.6,3.2,3.8,3.8,2.6,2.7,4,2.5);
y = c(3.5,3.4,3.9,3.7,2.9,2.8,3.6,2.6,2.3,4,2.5)
linear.reg = lm(y~x);
coef(linear.reg);

# Question 3 (What is the MLE estimate for Sigma^2)
x = c(8,9,6,5,3,6,3,2);
y = c(10.9,8.6,11.4,13.6,10.3,11.7,10.7,14.8)
linear.reg = lm(y~x);
coef(linear.reg);

# Question 4 (What is the distribution of Beta)
x = c(36,48,45,29,49,35,65,32,33,9,17,15,8);
y = c(84.5,67.1,188,204.2,213.1,260.5,723.5,135.6,45.3,1.7,387.8,24.2,15)
linear.reg = lm(y~x);
coef(linear.reg);