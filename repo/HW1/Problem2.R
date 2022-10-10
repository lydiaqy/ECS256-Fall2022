# Pick a value for lambda and c
lambda <- 3
c <- 1
# T=min(X,c)
T_func <- function(n){
  # Generate n samples in exponential distribution with rate as lambda
  X <- rexp(n, rate = lambda)
  l <- length(X)
  T <- rep(0, l)
  for (i in 1:l){
    T[i] <- if(X[i] < c) X[i] else c
  }
  print(T)
}
# An example for 10000 samples
T <- T_func(10000)
VarT <- var(T)
print(VarT)

# Our calculation results
# E(T|X<=c)
Cal_ET1 <- 1/lambda - (c+1/lambda)*exp(-lambda*c)
# E(T^2|X<=c)
Cal_ET1_2 <- 2/(lambda^2) - (c^2+2*c/lambda+2/(lambda^2))*exp(-lambda*c)
# E(T|X>c)
Cal_ET2 <- c
# E(T^2|X>c)
Cal_ET2_2 <- c^2
# E(T)
Cal_ET <- Cal_ET1*(1-exp(-lambda*c)) + Cal_ET2*exp(-lambda*c)
# E(T^2)
Cal_ET_2 <- Cal_ET1_2*(1-exp(-lambda*c)) + Cal_ET2_2*exp(-lambda*c)
# Var(T)
Cal_VarT <- ET_2 - ET^2
# Calculation result
print(Cal_VarT)
# Absolute difference between our calulation result and the random 10000 sample variance
print(abs(Cal_VarT - VarT))

# Run multiple n
N <- c(10, 100, 1000, 10000, 100000)
length_n <- length(N)
sum_diff_n <- rep(0, length_n)
# Do multiple trials
n_trials <- 100
for (j in 1:n_trials){
  VarT_n <- rep(0, length_n)
  for (i in 1:length_n){
    VarT_n[i] <- var(T_func(N[i]))
  }
  print(VarT_n)
  diff_n <- rep(0, length_n)
  for (i in 1:length_n){
    diff_n[i] <- abs(Cal_VarT - VarT_n[i])
    sum_diff_n[i] <- sum_diff_n[i] + diff_n[i]
  }
}
# Get the average of absolute difference
avg_diff_n <- sum_diff_n/n_trials
print(avg_diff_n)