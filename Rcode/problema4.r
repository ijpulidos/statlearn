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
indices = sample(c(1:N), size=300)  # getting indices randmonly
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
points(x_test, kn(25,"TRUE"), type="l", col=5, lwd=2)

# Test
k = 1
y_pred = kn(k, "TRUE")
MSEi = mean(sum((y_test - y_pred)^2))