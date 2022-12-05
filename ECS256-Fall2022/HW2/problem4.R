#=======a.eTij(P)========
# Function to calculate E(T_{ij}), j=n
findeta <- function(P, n, d){
  p_nn <- P[-n, -n]
  q <- diag(d-1) - p_nn
  ones <- rep(1, d-1)
  solve(q, ones)
}
# Target function
eTij <- function(P){
  d <- nrow(P)
  # When i=j, E(T_{ij}|U=j)=1
  Tij <- matrix(1, d, d)
  rownames(Tij) <- 1:d
  colnames(Tij) <- 1:d
  # When j=n, calculate E(T_{in}|U=k/=j)
  for (j in 1:d){
    eTin <- findeta(P, j, d)
    for (i in 1:d){
      if (i < j){
        Tij[i, j] <- eTin[i]
      }
      if (i > j){
        Tij[i, j] <- eTin[i-1]
      }
    }
  }
  return(Tij)
}

# Test with an example
test <- matrix(c(0.25, 0.75, 0, 0.25, 0, 0.75, 0, 0.25, 0.75), ncol=3, byrow=TRUE)
eTij_test <- eTij(test)
print(eTij_test)

#=======b.varTij(P)========
# Function to calculate E(T_{ij}^2), j=n
findbeta <- function(P, n, d){
  p_nn <- P[-n, -n]
  q <- diag(d-1) - p_nn
  eta <- findeta(P, n, d)
  rhs <- 2*eta - rep(1, d-1)
  solve(q, rhs)
}
# Function to get E(T_{ij}^2)
e_Tij2 <- function(P){
  d <- nrow(P)
  # When i=j, E(T_{ij}^2|U=j)=1
  eTij2 <- matrix(1, d, d)
  rownames(eTij2) <- 1:d
  colnames(eTij2) <- 1:d
  # When j=n, calculate E(T_{in}^2|U=k/=j)
  for (j in 1:d){
    eTin2 <- findbeta(P, j, d)
    for (i in 1:d){
      if (i < j){
        eTij2[i, j] <- eTin2[i]
      }
      if (i > j){
        eTij2[i, j] <- eTin2[i-1]
      }
    }
  }
  return(eTij2)
}

# Function to get E(T_{ij})^2
eTij_2 <- function(P){
  etij <- eTij(P)
  return(etij * etij)
}

# Target function
# Var(T_{ij}) = E(T_{in}^2) - E(T_{ij})^2
varTij <- function(P){
  e_tij2 <- e_Tij2(P)
  etij_2 <- eTij_2(P)
  vartij <- e_tij2 - etij_2
  return(vartij)
}

# Test with an example
test <- matrix(c(0.25, 0.75, 0, 0.25, 0, 0.75, 0, 0.25, 0.75), ncol=3, byrow=TRUE)
varTij_test <- varTij(test)
print(varTij_test)