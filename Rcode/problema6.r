# needed modules/libs
library(leaps)

# Defining seed for reproducibility
set.seed(42)

# Size of data
N = 150

x1 = rnorm(N,0,4)
x2 = -1.5*x1+rnorm(N, 0, 3)
y = 10+2*x1+1.5*x2+rnorm(N,0,3)
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

pairs(cbind(x,y))

x3 = 1*y + rnorm(N, 0, 4)
x4 = -6*x2 + rnorm(N, 0, 3)
x5 = 5*x1 + rnorm(N, 0, 5)
x6 = 0.3*x3 + rnorm(N, 0, 4)
x = cbind(x1, x2, x3, x4, x5, x6)
fit=lm(y~x); summary(fit)

pairs(cbind(x,y))

# Dividing data sets in train and test of same size
indices = sample(c(1:N), size=0.5*N)  # getting indices randmonly
x1_train = x1[indices]
x2_train = x2[indices]
x3_train = x3[indices]
x4_train = x4[indices]
x5_train = x5[indices]
x6_train = x6[indices]
y_train = y[indices]
x1_test = x1[-indices]
x2_test = x2[-indices]
x3_test = x3[-indices]
x4_test = x4[-indices]
x5_test = x5[-indices]
x6_test = x6[-indices]
y_test = y[-indices]

# Creating dataframe

data_train <- data.frame(x1_train, x2_train, x3_train, x4_train, x5_train, x6_train, y_train)
data_test <- data.frame(x1_test, x2_test, x3_test, x4_test, x5_test, x6_test, y_test)

### SELECCIÓN SECUENCIAL FORWARD ###

reg_subset_seq=regsubsets(y_train~.,data_train,nvmax=6,method="forward")

#Los resultados quedan en "reg_sub_summary"
reg_sub_summary_seq=summary(reg_subset_seq)

(reg_sub_summary_seq)

#Se pueden ver varios argumentos del resultado
reg_sub_summary_seq$cp  #(los Cp mallows para cada mejor modelo con cada k)
reg_sub_summary_seq$bic
reg_sub_summary_seq$adjr2

#Se pueden graficar los resultados:
plot(reg_sub_summary_seq$cp,type="b") #El cp de mallows cambiando k
plot(reg_sub_summary_seq$bic,type="b",col="red")  #El BIC
plot(reg_sub_summary_seq$adjr2,type="b",col="blue")  #El R2ajustado

# El cp para el mejor modelo (mínimo de la curva)
min(reg_sub_summary_seq$cp)

# Sanity check
# The idea is to check coefficients are the same as before
lm_best_seq = lm(y_train~x1_train+x3_train+x4_train, data=data_train)
summary(lm_best_seq)

# Prediction - MSE
pred_seq = predict(lm_best_seq, data_test)
mse_seq = mean((data_test$y_test - pred_seq)^2)


### SELECCIÓN EXHAUSTIVA ###

reg_subset_exh=regsubsets(y_train~.,data_train,nvmax=6,method="exhaustive")

#Los resultados quedan en "reg_sub_summary"
reg_sub_summary_exh=summary(reg_subset_exh)

(reg_sub_summary_exh)

#Se pueden ver varios argumentos del resultado
reg_sub_summary_exh$cp  #(los Cp mallows para cada mejor modelo con cada k)
reg_sub_summary_exh$bic
reg_sub_summary_exh$adjr2


### PCA ###

### PLS ###

### Ridge ###

### Lasso ###