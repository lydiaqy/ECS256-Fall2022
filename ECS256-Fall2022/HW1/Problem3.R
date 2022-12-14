# HW1 Problem3
# Functions to generate independent discrete random variable A,B,C
A <- function(){
  if (runif(1, min = 0, max = 1) < 0.75) 0 else 1
}
B <- function(){
  if (runif(1, min = 0, max = 1) < 1/3) 0 else 10
}
C <- function(){
  if (runif(1, min = 0, max = 1) < 0.5) 0 else 100
}
# U = A + C, V = B + C, Y = U + V + W
# Function to generate W with mean = 0
W <- function(){
  if (runif(1, min = 0, max = 1) < 0.5) -1 else 1
}
# Simulation of E(Y|U,V) = U + V
# Define the number of trials n
simu_1 <- function(n){
  p_uv <- rep(0, 16)
  y_uv <- rep(0, 16)
  count <- rep(0, 16)
  sum_u <- 0
  sum_v <- 0
  for (i in 1:n) {
    a <- A()
    b <- B()
    c <- C()
    w <- W()
    u <- a + c
    v <- b + c
    y <- u + v + w
    sum_u <- sum_u + u
    sum_v <- sum_v + v
    # U may be 0, 1, 100, 101
    # V may be 0, 10, 100, 110
    u_pred <- c(0, 1, 100, 101)
    v_pred <- c(0, 10, 100, 110)
    for (u_idx in 1:4){
      if (u == u_pred[u_idx]){
        for (v_idx in 1:4){
          if (v == v_pred[v_idx]){
            p_uv[4*(u_idx - 1) + v_idx] <- p_uv[4*(u_idx - 1) + v_idx] + 1
            y_uv[4*(u_idx - 1) + v_idx] <- y_uv[4*(u_idx - 1) + v_idx] + y
            count[4*(u_idx - 1) + v_idx] <- count[4*(u_idx - 1) + v_idx] + 1
          }
        }
      }
    }}
    # Probability for all conditions
    p_uv <- p_uv/n
    ey_uv <- 0
    # E(Y|U,V) = \sum_{E(Y|U=u, V=v)*p_uv}
    for (i in 1:16){
      if (count[i] != 0){
        ey_uv <- ey_uv + y_uv[i]/count[i]*p_uv[i]}
    }
    # The value of E(Y|U,V)
    print(ey_uv)
    # The value of averaged U and V
    avg_u <- sum_u/n
    avg_v <- sum_v/n
    # The sum of U and V
    print(avg_u + avg_v)
    sprintf("E(Y|U,V) - (U+V) = %f", abs(ey_uv - avg_u - avg_v))
}
n <- 10000
simu_1(n)

# Simulation of E(Y|U) = U + E(V|U)
# Define the number of trials n
simu_2 <- function(n){
  p_u <- rep(0, 4)
  y_u <- rep(0, 4)
  v_u <- rep(0, 4)
  count_yu <- rep(0, 4)
  count_vu <- rep(0, 4)
  sum_u <- 0
  for (i in 1:n) {
    a <- A()
    b <- B()
    c <- C()
    w <- W()
    u <- a + c
    v <- b + c
    y <- u + v + w
    sum_u <- sum_u + u
    # U may be 0, 1, 100, 101
    u_pred <- c(0, 1, 100, 101)
    for (u_idx in 1:4){
      if (u == u_pred[u_idx]){
        p_u[u_idx] <- p_u[u_idx] + 1
        y_u[u_idx] <- y_u[u_idx] + y
        v_u[u_idx] <- v_u[u_idx] + v
        count_yu[u_idx] <- count_yu[u_idx] + 1
        count_vu[u_idx] <- count_vu[u_idx] + 1
        }
      }
  }
  # Probability for all conditions for U
  p_u <- p_u/n
  ey_u <- 0
  ev_u <- 0
  # E(Y|U) = \sum_{E(Y|U=u, V=v)*p_uv}
  # E(V|U) = \sum_{E(Y|U=u, V=v)*p_uv}
  for (i in 1:4){
    if (count_yu[i] != 0){
      ey_u <- ey_u + y_u[i]/count_yu[i]*p_u[i]}
    if (count_vu[i] != 0){
      ev_u <- ev_u + v_u[i]/count_vu[i]*p_u[i]}
  }
  # The value of E(Y|U)
  print(ey_u)
  # The value of averaged U
  avg_u <- sum_u/n
  # The sum of U and E(V|U)
  print(avg_u + ev_u)
  sprintf("E(Y|U) - (U+E(V|U)) = %f", abs(ey_u - avg_u - ev_u))
}
n <- 10000
simu_2(n)