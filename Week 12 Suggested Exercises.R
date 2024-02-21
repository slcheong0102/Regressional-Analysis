rm(list=ls())
# Q2a. Fit the Following Model: Y = β0 + β1*x1 + β2*x2 + β3*x2^2
yield <- c(21,23,26,22,23,28)
pressure <- c(50,50,50,80,80,80)
temperature <- c(100,200,300,100,200,300)
temperature2 <- c(10000,40000,90000,10000,40000,90000)
rg2_c <- lm(yield ~ pressure + temperature + temperature2)
rg2_c

# Q2b. Test to see whether β3 differs significantly from zero, with alpha = 0.05
rg2_r <- lm(yield ~ pressure + temperature)
rg2_r
anova(rg2_c)
anova(rg2_r)
anova(rg2_c,rg2_r)

# Q2c
rg2_rnew<-lm(yield ~ pressure)
rg2_rnew
anova(rg2_c,rg2_rnew)

# Q3a. Fit the Model
y <- c(1,0,0,1,2,3,3)
x1 <- c(-3,-2,-1,0,1,2,3)
x2 <- c(5,0,-3,-4,-3,0,5)
x3 <- c(-1,1,1,0,-1,-1,1)
rg3_c <- lm(y ~ x1 + x2 + x3)
rg3_c
rg3_r <- lm(y ~ x1 + x2)
anova(rg3_c,rg3_r)
x0=data.frame(x1=1,x2=-3,x3=-1)
predict(rg3_c,x0,interval="confidence")
predict(rg3_c,x0,interval="prediction")

y=c(2.48,2.26,2.47,2.77,2.99,3.05,3.18,
    3.46,3.03,3.26,2.67,2.53);
x1=c(4.51,3.58,4.31,5.06,5.64,4.99,5.29,
     5.83,4.70,5.61,4.90,4.20);
x2=c(0,0,13,56,117,306,358,330,187,94,23,0);

mod = lm(y ~ x1 + x2)
mod

y=c(19.3,2.8,30.5,6.59,11.7,6.1,7.6,13.4,50.4,14.9,35.4,6.3);
x1=c(63,21,94,50,42,21,36,48,104,49,89,25);
x2=c(40,7,87,10,23,6,11,8,88,43,71,8);

SSY=sum((y-mean(y))^2);
n=length(y);
k =2;
mod=lm(y~x1+x2);
SSE=sum(mod$res^2);
numerator=(SSY-SSE)/k;
denominator=SSE/(n-k-1);
F.star = numerator/denominator;
F.star;

SSY
SSE

rm(list=ls())

x1 <- c(-3,-2,-1,0,1,2,3)
y <- c(1,0,0,-1,-1,0,0)
x2 <- c(9,4,1,0,1,4,9)
mod = lm(y ~ x1 + x2)
mod

x1=c(rep(80,9),rep(90,9),rep(100,9));
x2= rep(c(rep(50,3),rep(55,3),rep(60,3)),3);
y = c(50.8,50.7,49.4,93.7,90.9,90.9,74.5,73.0,71.2,
      63.4,61.6,63.4,93.8,92.1,97.4,70.9,68.8,71.3,
      46.6,49.1,46.4,69.8,72.5,73.2,38.7,42.5,41.4);

mod.C=lm(y~x1+x2+I(x1*x2)+I(x1^2)+I(x2^2));
coef(mod.C);

SSE.C=sum((residuals(mod.C))^2);
SSE.C;

