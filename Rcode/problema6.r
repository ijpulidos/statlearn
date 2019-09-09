# Defining seed for reproducibility
set.seed(42)

x1 = rnorm(150,0,4)
x2 = -1.5*x1+rnorm(150, 0, 3)
y = 10+2*x1+1.5*x2+rnorm(150,0,3)
par(mfrow=c(1,3))
plot(x1,y); plot(x2,y); plot(x1,x2)

cor(x1, y)
cor(x2, y)
cor(x1, x2)

x = cbind(x1, x2)
fit = lm(y~x); summary(fit)

summary(fit)

# Linear model for each variable
fit1 = lm(y~x1); summary(fit1)
fit2 = lm(y~x2); summary(fit2)

pairs(cbind(y,x))
