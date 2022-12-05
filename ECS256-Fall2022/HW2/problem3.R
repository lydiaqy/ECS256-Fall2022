# Generate transition matrix
transmat <- matrix(rep(0, 16), ncol=4)
p <- 0.4
q <- 0.3
p1 <- 1 - p
q1 <- 1 - q
transmat[1,1] <- q1^3 + 3*q*(q1^2)*p
transmat[1,2] <- 3*q*(q1^2)*p1 + 3*(q^2)*q1*2*p*p1
transmat[1,3] <- 3*(q^2)*q1*((p1^2) + (p^2)) + 3*(q^3)*p*(p1^2)
transmat[1,4] <- (q^3)*((p1^3) + (p^3)) + 3*(q^3)*p1*(p^2)
transmat[2,1] <- (q1^2)*p
transmat[2,2] <- 2*q*q1*2*p*p1 + (q1^2)*p1
transmat[2,3] <- 2*q*q1*(p1^2+p^2) + 3*(q^2)*p*(p1^2)
transmat[2,4] <- (q^2)*(p^3+p1^3+3*(p^2)*p1)
transmat[3,1] <- 0
transmat[3,2] <- 2*p*p1*q1
transmat[3,3] <- q1*(p1^2+p^2) + 3*q*p*(p1^2)
transmat[3,4] <- q*(p^3+p1^3+3*(p^2)*p1)
transmat[4,1] <- 0
transmat[4,2] <- 0
transmat[4,3] <- 3*p*(p1^2)
transmat[4,4] <- p^3 + p1^3 + 3*(p^2)*p1
# Check sum of each row
apply(transmat,1,sum)
# Calculate pi
# (I-P')pi = 0
findpi <- function(p){
  n <- nrow(p)
  pi_vec <- eigen(t(p))$vectors[,1]
  if (pi_vec[1] < 0) pi_vec <- -pi_vec
  pi_vec <- pi_vec / sum(pi_vec)
  return(pi_vec)
}
pi_vec <- findpi(transmat)
print(pi_vec)
# P(collision per time slot | state i), i = 0, 1, 2, 3
Pc_s0 <- 3*(q^2)*q1*(p^2) + (q^3)*(p^3) + 3*(q^3)*p1*(p^2)
Pc_s1 <- 2*q*q1*(p^2) + (q^2)*(p^3) + 3*(q^2)*(p^2)*p1
Pc_s2 <- q1*(p^2) + q*(p^3) + 3*q*(p^2)*p1
Pc_s3 <- p^3 + 3*(p^2)*p1
Pc_s <- c(Pc_s0, Pc_s1, Pc_s2, Pc_s3)
# P(collision per time slot)
Pc <- sum(Pc_s * pi_vec)
# Long-run average time between collisions, unit is time slot
print(1/Pc)
