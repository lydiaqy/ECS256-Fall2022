# Target function
MCautocor <- function(k, P){
  m <- nrow(P)
  pivec <- findpi(P)
  P_k <- getP_k(P, k)
  rho <- 0
  for (s in 1:m){
    for (t in 1:m){
      rho <- rho + s*t*pivec[s]*P_k[s,t]
    }
  }
  mu <- wtdmean(pivec, c(1:m))
  sigmasqr <- wtdvar(pivec, c(1:m))
  return((rho - mu^2)/sigmasqr)
}
# Function to get P^k
getP_k <- function(P, k){
  prd <- P
  if (k > 1){
    for (i in 1:(k-1)){
      prd <- prd %*% P
    }
  }
  return(prd)
}
# Function to find pi
findpi <- function(P){
  n <- nrow(P)
  pivec <- eigen(t(P))$vectors[,1]
  if (pivec[1] < 0) pivec <- -pivec
  pivec <- pivec / sum(pivec)
  return(pivec)
}
# Generate standarized transition matrix
test <- function(m){
  P <- matrix(0, nrow=m, ncol=m)
  P[1, 1] <- 0.5
  P[m, m] <- 0.5
  for (i in 1:m){
    if (i < m) P[i, i+1] <- 0.5
    if (i > 1) P[i, i-1] <- 0.5
  }
  P
}
# Functions to caclulate mean and variance of the states
wtdmean <- function(p, x){sum(p*x)}
wtdvar <- function(p, x){wtdmean(p, x^2) - (wtdmean(p, x))^2}

# Tests to verify
P20 <- test(20)
MCautocor(5, P20)
MCautocor(9, P20)
MCautocor(15, P20)

##=====simulation code=======
#sim <- function(k, P, nReps){
#  m <- nrow(P)
#  pivec <- findpi(P)
#  oneSamplePath <- function(){
#    x_arr <- sample(c(1:m), size = k+1, prob = pivec)
#    return(x_arr)
#  }
#  paths <- replicate(nReps, oneSamplePath())
#  corr <- cor(t(paths))
#  return(corr)
#}
#sim(5, P20, 10000)  