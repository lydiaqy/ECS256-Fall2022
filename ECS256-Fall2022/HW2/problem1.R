# Function to calculate p^m
findpi <- function(p, k){
  prd <- p
  if (k > 1){
    for (i in 1:(k - 1)){
      prd <- prd %*% p
    }
  }
  return(prd)
}

# Function to calculate l1 distance
l1_distance <- function(u, v){
  l <- length(u)
  sum <- 0
  for (i in 1:l){
    sum <- sum + abs(u[i] - v[i])
  }
  return(sum)
}

# Function to get L1 norm distance
get_l1 <- function(m, P){
  l1_norm <- rep(0, m)
  for (i in 1:m){
    prd <- findpi(P, i)
    # Use row 1
    pi_row1 <- prd[1,]
    # Average all rows
    pi_avg <- colMeans(prd)
    # Calculate l1 distance
    l1_norm[i] <- l1_distance(pi_row1, pi_avg)
  }
  return(l1_norm)
}
#========= Experiment for small P===========
# Define P
P_small <- matrix(c(0.658, 0.280, 0.000, 0.2952, 0.5640, 0.4800, 0.0468, 0.1560, 0.5200), ncol=3)
# Check sum of each row
apply(P_small, 1, sum)
# Pick m
m <- 40
l1_norm_small <- get_l1(m, P_small)
# Plot (m, l1_norm) for small P
#install.packages("ggplot2")
#install.packages("ggpubr")
library(ggplot2)
library(ggpubr)
psmall_data <- data.frame(k = 1:m, l1_distance = l1_norm_small)
plot_sp <- ggplot(data = psmall_data, aes(x = k, y = l1_distance)) + geom_point()
plot_sp + ggtitle("Small P") + xlab("Values of k") + ylab("L1 Distance")
#========= Experiment for large P===========
# Function to generate a large P
gen_large_transmat <- function(dim){
  transmat <- matrix(0, dim, dim)
  for (i in 1:dim){
    for (j in 1:dim){
      transmat[i, j] <- runif(1, min=1, max=20)
    }
    transmat[i, ] <- transmat[i, ]/sum(transmat[i, ]) 
  }
  transmat
}
# Define the dimension of P large
dim <- 20
P_large <- gen_large_transmat(dim)
apply(P_large, 1, sum)
l1_norm_large <- get_l1(m, P_large)
# Plot (m, l1_norm) for large P
plarge_data <- data.frame(k = 1:m, l1_distance = l1_norm_large)
plot_lp <- ggplot(data = plarge_data, aes(x = k, y = l1_distance)) + geom_point()
plot_lp + ggtitle("Large P") + xlab("Values of k") + ylab("L1 Distance")
ggarrange(plot_sp, plot_lp, labels = c("Small P", "Large P"), ncol = 2, nrow = 1)%>%
  ggexport(filename = "/Users/pmh/Yun/ECS256_probability/HW2/problem1.png")