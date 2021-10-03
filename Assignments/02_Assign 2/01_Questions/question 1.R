x=c(1.5,4,3,3,4.5,2)
y=c(2.5,3.5,4,2.5,5,1.5)
x^2
y^2
x*y
relation=lm(y~x)
plot(x,y)
abline(relation)
y_hat=0.5128+0.8846*x

SST=sum((y-mean(y))^2)
SSR=sum((y_hat-mean(y))^2)
SSRes=sum((y-y_hat)^2)
sigma2_hat=SSRes/(6-2)
sigma_hat=sqrt(sigma2_hat)
t=qt(0.05,4,lower.tail = FALSE)

#beta1 ci
c_11=sqrt(1/(sum((x-mean(x))^2)))
beta_0_lower=0.5128-t*sigma_hat*c_11
beta_0_upper=0.5128+t*sigma_hat*c_11

#beta0 ci
c_00=sqrt((1/6)+(mean(x))^2/(sum((x-mean(x))^2)))
beta_1_lower=0.8846-t*sigma_hat*c_00
beta_1_upper=0.8846+t*sigma_hat*c_00

#y_0_hat ci
h_00=sqrt((1/6)+(3-mean(x))^2/(sum((x-mean(x))^2)))
y_0_hat=0.5128+0.8846*3
y_0_hat_lower=y_0_hat-t*sigma_hat*h_00
y_0_hat_upper=y_0_hat+t*sigma_hat*h_00


