# Selecting seed to be reproducible
set.seed(42)

# "best" predictor/algorithm
# One can try to modify the denominator inside the cosine to change the complexity of f*
f = function(x){
  y=2+x^(.2)*cos(x/.15)/x^-.45
  return(y)
}
plot(f,0,5)

# Simulating data points: change n and sigma
N=400
sigma=1.2
x=runif(N,0,5); x=sort(x)  # For convenience, the input x is sorted
y=rep(0,times=N)
for (i in 1:N) {
  y[i] = f(x[i]) + rnorm(1,0,sigma)  # rnorm(size, mean, std)
}
plot(x,y)
points(x,f(x), type="l", col=2, lwd=2)

# k-neighbors estimator
# k is number of neighbors and test=TRUE/FALSE tells if you estimate
# over any grid (FALSE) or over the x sampled in test data set (TRUE)
kn = function(k, test){
  if (test=="FALSE") {
    z = seq(0, 5, by=0.01)
    ll = length(z)
  }
  if (test=="TRUE"){
    z = x_test
    ll = length(z)
  }
  nk = rep(0, times=ll)
  for (j in 1:ll) {
    veci = which(abs(z[j]-x_train) %in% sort(abs(z[j]-x_train))[1:k])
    nk[j] = sum(y_train[veci])/k
  }
  return(nk)
}

# Sampling data between train and test
indices = sample(c(1:N), size=400)  # getting indices randmonly
x_train = x[indices]
y_train = y[indices]
x_test = x[-indices]
y_test = y[-indices]

# Plotting train and test data in same figure
plot(x_train, y_train)
points(x_test, y_test, col=2, pch=4)

# MSE computation (test data)
klist = 1:200  # List of k to test
MSE = rep(0, times=length(klist))  # I make a zeroes array with length of ksize to store MSEs
for (ki in klist) {
  y_pred = kn(ki, "TRUE")
  MSEi = mean(sum((y_test - y_pred)^2))
  MSE[ki] = MSEi
}

# Getting best k from MSE
plot(klist, MSE)
kbest = which.min(MSE)

# Plotting best estimator
plot(x,y)
points(x, f(x), type="l", col=2, lwd=2)
points(x_test, kn(kbest,"TRUE"), type="l", col=3, lwd=2)


####### CROSS VALIDATION (4.3) #######

# k-fold cross validation
folds = 10  # ex. folds=10 means 10-fold CV
fold_size = N/10

klist = 1:200  # List of k to test
MSE_complete = list()  # List to store the MSE list of each k-fold iteration
for (iteration in 1:folds){
  metaindices_test = ((iteration-1)*fold_size+1):(iteration*fold_size)  # Parenthesis are really important to separate :
  ss1 = indices[-metaindices_test] # Choosing indices for train (ss1) and test (ss2)
  ss2 = indices[metaindices_test]
  # choosing partition train/test data
  y_train=y[ss1]
  x_train=x[ss1]
  y_test=y[ss2]
  x_test=x[ss2]
  # Plotting train and test data in same figure
  #plot(x_train, y_train)
  #points(x_test, y_test, col=2, pch=4)
  #browser()  # breakpoint
  # MSE computation (test data)
  MSE = rep(0, times=length(klist))  # I make a zeroes array with length of ksize to store MSEs
  for (ki in klist) {
    y_pred = kn(ki, "TRUE")
    MSEi = mean(sum((y_test - y_pred)^2))
    MSE[ki] = MSEi
  }
  MSE_complete[[iteration]] <- MSE  # Store the MSE for this k in the complete MSE list.
}

# Plotting all MSE curves
for (i in 1:folds) {
  if (i==1){
    plot(klist, MSE_complete[[i]])
  }
  else{
    points(klist, MSE_complete[[i]]) 
  }
}

# Computing mean MSE curve
MSE_mean = list()
for (i in klist) {
  sum = 0
  for (j in 1:folds) {
    sum = sum + MSE_complete[[j]][i]
  }
  MSE_mean[i] <- sum/folds
}
# Plotting mean MSE curve
#points(klist, MSE_mean, col=2)
plot(klist, MSE_mean, col=2)
# Taking the best k from mean MSE curve
k_best = which.min(MSE_mean)

# Plotting best estimator
plot(x,y)
points(x, f(x), type="l", col=2, lwd=2)
# Sorting x_test for plotting
x_test = sort(x_test)
points(x_test, kn(k_best,"TRUE"), type="l", col=3, lwd=2)

# Effect of irreducible error sigma^2
# The error is related to the sigma somehow...

mean(sum((y_test - f(x_test))^2))  # Irreducible error (?)

y_theoretical = list()
for (i in 1:length(x_test)) {
  y_theoretical[i] <- f(x_test[i])
}

points(x_test, y_theoretical)


length(y_theoretical)
length(x_test)

####### END OF CROSS-VALIDATION (4.3) #######
