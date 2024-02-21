x = c(27,39,73,66,33,43,47,55,60,68,70,75,82);
y = c(2,3,10,9,4,6,5,8,7,9,10,13,12);
x.bar = mean(x);
y.bar = mean(y);
S.xx = (x-x.bar)%*%(x-x.bar);
S.yy = (y-y.bar)%*%(y-y.bar);
S.xy = (x-x.bar)%*%(y-y.bar);
b1 = S.xy/S.xx;
b0 = y.bar - (S.xy/S.xx)*x.bar;
MSR <- (b1^2)*S.xx;
SSE <- S.yy - MSR;
MSE <- SSE/11;
F_Statistics <- MSR/MSE

