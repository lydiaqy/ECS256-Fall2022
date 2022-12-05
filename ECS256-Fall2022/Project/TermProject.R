#------Problem A code-------
#-------this is the function that computes the pi vector------
# f(i,j)=1+j+i*(n+1)
findQ=function(m,n,u,v,p)
{
  convert=function(i,j)
  {
    return(1+j+i*(n+1))
  }
  # matrix storing lamb_ij_kl
  M=matrix(0,(m+n+1)*(n+1),(m+n+1)*(n+1))
  # vector storing lamb_i
  lamb_vec=rep(0,(m+n+1)*(n+1))
  
  
  for (i in 0:(m+n))
  {
    for (j in 0:n)
    {
      if (i-j<=m)
      {
        up=0
        down=0
        #each case we can go up or down
        #we treat each case separately
        queue=max(i-j,0)
        meet=min(i,j)
        
        # more people or nurses
        # regular mu_ij=1/v
        if (queue==m)
        {
          if (j==n)
          {
            # can't go up
          }
          else
          {
            # can go up to i+1,j+1
            M[1+j+i*(n+1),1+j+1+(i+1)*(n+1)]=p/v
            up=p/v
          }
          
        }
        # the queue is not full, can add one more person
        else
        {
          M[1+j+i*(n+1),1+j+(i+1)*(n+1)]=1/v
          up=1/v
        }
        # less people or nurses
        # regular d_ij=min(i,j)/u
        
        # if there are people it's possible to go down
        if (i>0)
        {
          # if no one in queue
          
          
          # if there are nurses
          if(j>0)
          {
            if (queue==0)
            {
              # reduce one nurse and patient
              M[1+j+i*(n+1),j+(i-1)*(n+1)]=meet/u
              down=meet/u
            }
            # if there are people in queue, reduce patient by 1
            else
            {
              M[1+j+i*(n+1),1+j+(i-1)*(n+1)]=meet/u
              down=meet/u
            }
          }
          # if no nurses, can't go down
          else
          {
            
          }
        }
        # no person, can't go down
        else
        {
          
        }
      }
      # illegal states, ignore them
      else
      {
        
      }
      
      # update lamb_ij
      lamb_vec[1+j+i*(n+1)]=up+down
    }
  }
  
  # now we have the matrix containing lamb_ij_kl
  # and the vector containing lamb_ij
  # now try to find Q using M and lamb_vec
  Q=matrix(0,(m+n+1)*(n+1),(m+n+1)*(n+1))
  for (k in 1:((m+n+1)*(n+1)))
  {
    for (l in 1:((m+n+1)*(n+1)))
    {
      if (k==l)
      {
        
        Q[k,l]=-lamb_vec[k]
      }
      else
      {
        
        Q[k,l]=M[l,k]
      }
    }
  }
  
  #print(lamb_vec)
  # deletion of impossible states
  # deletion of transient states
  count=0
  for (i in 0:(m+n))
  {
    for (j in 0:n)
    {
      if (i-j>m )
      {
        Q=Q[-(convert(i,j)-count),-(convert(i,j)-count)]
        
        count=count+1
      }
      else if(j>i)
      {
        Q=Q[-(convert(i,j)-count),-(convert(i,j)-count)]
        
        count=count+1
      }
    }
  }
  #print(t(M))
  
  return(Q)
}
find_pi=function(Q)
{
  num=nrow(Q)
  Q[num,]=rep(1,num)
  b=c(rep(0,num-1),1)
  #b=rbind(b)
  #b=t(b)
  #pi_vec=solve(Q,b)
  #pi_vec=inv(Q)%*%b
  pi_vec=solve(Q,b)
  return(pi_vec)
}

call_system=function(m,n,u,v,p)
{
  Q=findQ(m,n,u,v,p)
  pi_vec=find_pi(Q)
  for (i in 0:(m+n))
  {
    for (j in 0:n)
    {
      if (i-j>m)
      {
        pi_vec=append(pi_vec,0,j+i*(n+1))
      }
      else if(j>i)
      {
        pi_vec=append(pi_vec,0,j+i*(n+1))
      }
    }
  }
  return(pi_vec)
}
queue_vec=function(m,n,pi_vec)
{
  q_vec=rep(0,m+1)
  for (i in 0:(m+n))
  {
    for (j in 0:n)
    {
      queue=max(i-j,0)
      if (i-j<=m)
      {
        q_vec[queue+1]=q_vec[queue+1]+pi_vec[j+1+i*(n+1)]
      }
    }
  }
  return(q_vec)
}
meeting_vec=function(m,n,pi_vec)
{
  m_vec=rep(0,n+1)
  for (i in 0:(m+n))
  {
    for (j in 0:n)
    {
      meet=min(i,j)
      if (i-j<=m)
      {
        m_vec[meet+1]=m_vec[meet+1]+pi_vec[j+1+i*(n+1)]
      }
    }
  }
  return(m_vec)
}
people_vec=function(m,n,pi_vec)
{
  p_vec=rep(0,m+n+1)
  for (i in 0:(m+n))
  {
    for (j in 0:n)
    {
      if (i-j<=m)
      {
        p_vec[i+1]=p_vec[i+1]+pi_vec[j+1+i*(n+1)]
      }
    }
  }
  return(p_vec)
}
nurse_vec=function(m,n,pi_vec)
{
  n_vec=rep(0,n+1)
  for (i in 0:(m+n))
  {
    for (j in 0:n)
    {
      if (i-j<=m)
      {
        n_vec[j+1]=n_vec[j+1]+pi_vec[j+1+i*(n+1)]
      }
    }
  }
  return(n_vec)
}
queue_length=function(m,n,pi_vec)
{
  l=0
  queue_vec=queue_vec(m,n,pi_vec)
  for (i in 0:m)
  {
    l=l+i*queue_vec[i+1]
  }
  return(l)
}
num_people=function(m,n,pi_vec)
{
  l=0
  people_vec=people_vec(m,n,pi_vec)
  for (i in 0:(m+n))
  {
    l=l+i*people_vec[i+1]
  }
  return(l)
}
num_nurse=function(m,n,pi_vec)
{
  l=0
  nurse_vec=nurse_vec(m,n,pi_vec)
  for (i in 0:n)
  {
    l=l+i*nurse_vec[i+1]
  }
  return(l)
}
num_meeting=function(m,n,pi_vec)
{
  l=0
  meeting_vec=meeting_vec(m,n,pi_vec)
  for (i in 0:n)
  {
    l=l+i*meeting_vec[i+1]
  }
  return(l)
}
utility_rate=function(m,n,pi_vec)
{
  rate=num_meeting(m,n,pi_vec)/num_nurse(m,n,pi_vec)
  return(rate)
}
mean_waititng=function(m,n,u,v,pi_vec)
{
  e_queue=queue_length(m,n,pi_vec)
  e_meeting=num_meeting(m,n,pi_vec)
  handling_t=u/e_meeting
  e_wait=e_queue*handling_t
  return(e_wait)
}
mean_residence=function(m,n,u,v,pi_vec)
{
  wt=mean_waititng(m,n,u,v,pi_vec)
  e_res=wt+u
  return(e_res)
}
rejection_prob=function(m,n,p,pi_vec)
{
  prob=0
  for (i in (0:m+n))
  {
    for (j in 0:n)
    {
      if (i-j==m)
      {
        if (j==n)
        {
          prob=prob+pi_vec[1+j+i*(n+1)]
        }
        else
        {
          prob=prob+pi_vec[1+j+i*(n+1)]*(1-p)
        }
      }
    }
  }
  return(prob)
}
# conditional waiting time
wt_notice=function()
{
  
}
m=2
n=3
u=3
v=2
p=1
pi_vec=call_system(m,n,u,v,p)
#print(queue_length(m,n,pi_vec))
#print(mean_residence(m,n,u,v,pi_vec))
print(mean_waititng(m,n,u,v,pi_vec))
print(rejection_prob(m,n,p,pi_vec))

#-------this is the simulation code for problem 1-------
q_simu=function(m,n,u,v,p,duration,k,l)
{
  state_vec=rep(0,(1+m+n)*(1+n))
  # random starting nurses and people
  #j=sample.int(n+1,1)-1
  #i=sample.int(j+m+1,1)-1
  wt=0
  wij=0
  wt_vec=numeric(0)
  if_waiting=FALSE
  i=0
  j=0
  ct=0
  q=max(k-l,0)
  progress=0
  while (ct<duration)
  {
    if (i==k&&j==l)
    {
      if_waiting=TRUE
    }
    # find the queue length
    # find the number of meetings
    if (i<=j)
    {
      queue=0
      meet=i
    }
    else
    {
      queue=i-j
      meet=j
    }
    arrival=rexp(1,1/v)
    if (meet!=0)
    {
      cf=rexp(1,meet/u)
    }
    else
    {
      cf=Inf
    }
    ht=min(cf,arrival)
    wt=wt+queue*ht
    state_vec[1+j+i*(n+1)]=state_vec[1+j+i*(n+1)]+ht
    # if it is arrival
    if (arrival<=cf)
    {
      if (queue==m)
      {
        if (j<n)
        {
          num=runif(1)
          # add patient and nurse
          if (num<p)
          {
            
            i=i+1
            j=j+1
            
          }
          # reject the patient
          else
          {
            
          }
        }
        # reject the patient since there is no room
        else if(j==n)
        {
          
        }
      }
      else
      {
        i=i+1
        
      }
      
    }
    
    # if it is cf
    else
    {
      # reduce 1 nurse if queue is empty
      if (queue==0)
      {
        
        j=j-1
      }
      else
      {
        
      }
      i=i-1
      if (if_waiting==TRUE)
      {
        progress=progress+1
      }
      
    }
    ct=ct+ht
    if (if_waiting==TRUE)
    {
      wij=wij+ht
    }
    if (progress==q)
    {
      wt_vec=c(wt_vec,wij)
      wij=0
      progress=0
      if_waiting=FALSE
    }
  }
  return(sum(wt_vec)/length(wt_vec))
  #return(state_vec)
  
}
m=3
n=3
u=2
v=1
p=1
duration=80000
print(q_simu(m,n,u,v,p,duration,3,1))
#jpeg("plt1.jpg",width=600,height=600)
#plot(x,err_vec,main="",xlab="m",ylab="difference between simulation and waiting time function")
#lines(x,r2,type="l",col="green")
#legend("topleft",legend=c("rvs=5","rvs=30"),col=c("red","green"),lwd=2)
#dev.off()

#-------this is the code that generates the optimal choice of m,n table------

#mean_waititng(m,n,u,v,pi_vec)
#rejection_prob(m,n,p,pi_vec)
# find out the best combinations of m and n for each raito of u,v
n_vec=numeric(0)
m_vec=numeric(0)
mw_vec=numeric(0)
rj_vec=numeric(0)
for (u in seq(0.2,5,0.4))
{
  best_m=-5
  least_nurse=20
  good_mw=-1
  good_rj=-1
  for (n in ceiling(u):10)
  {
    for (m in 0:5)
    {
      pi_vec=call_system(m,n,u,1,1)
      mw=mean_waititng(m,n,u,1,pi_vec)
      rj=rejection_prob(m,n,1,pi_vec)
      if (mw<(u/2)&&rj<0.01)
      {
        if (n<least_nurse)
        {
          best_m=m
          least_nurse=n
          good_mw=mw
          good_rj=rj
        }
      }
    }
  }
  n_vec=c(n_vec,least_nurse)
  m_vec=c(m_vec,best_m)
  mw_vec=c(mw_vec,good_mw)
  rj_vec=c(rj_vec,good_rj)
}
x=seq(0.2,5,0.4)
plot(x,n_vec)
plot(x,m_vec)
# find how scaling the parameters affects the metrics
m=2
n=3
p=1
u=2
v=1
lst1=rep(0,10)
lst2=rep(0,10)

x1=c(1:10)
for (i in 1:10)
{
  pi_vec=call_system(m,n,i*u,i*v,p)
  mw=mean_waititng(m,n,i*u,i*v,pi_vec)
  rj=rejection_prob(m,n,p,pi_vec)
  lst1[i]=mw
  lst2[i]=rj
  mw_vec[i]=mw
}
plot(x1,lst1,main="the effect to mean waiting time when we scale u and v, m=2,n=3, u/v=2",xlab="v",ylab="mean waiting time")
plot(x1,lst2,main="the effect to rejection probability when we scale u and v,m=2, n=3, u/v=2",xlab="v",ylab="rejection probability")


#------Problem B code-------
#------Code for Chapter 2.2 Uniform-------
library(ggplot2)
library(ggpubr)
# Define the number of trials
trials <- 10000
# approximate U(1,2) using MOS
a <- 1
b <- 2
# Denote rv as the number of exponential rvs,rv>=1
rv <- 100
# Denote q as the number of quantiles, q>=1
q <- 100
# Mean and Variance of this uniform dist
target_mean <- 1.5
target_var <- 1/12

# Simulation on states with equal means
approx_equal <- function(){
  list1 <- numeric(0)
  for (i in 1:trials)
  {
    prob <- runif(1)
    prob_arr <- seq(0, 1, by=(1/q))
    idx <- findInterval(prob, prob_arr)
    lambda <- a + idx*((b-a)/(q+1))
    approx <- sum(rexp(rv, rv/lambda))
    list1 <- c(list1, approx)
  }
  return(list1)
}
# Plot
start_time1 <- Sys.time()
list1 <- approx_equal()
end_time1 <- Sys.time()
list1_data <- data.frame(t = 1:trials, points = list1)
plot1 <- ggplot(data = list1_data, aes(x = t, y = points)) + geom_point()
plot1 + ggtitle("Equal lambda") + xlab("Trials") + ylab("Points")
# Mean
mean1 <- mean(list1)
# Variance
var1 <- var(list1)

# Simulation on states with random means (sum is the quantile)
approx_random <- function(){
  list2=numeric(0)
  for (i in 1:trials)
  {
    prob <- runif(1)
    quantile_arr <- seq(0, 1, by=(1/q))
    idx <- findInterval(prob, quantile_arr)
    lambda <- a + idx*((b-a)/(q+1))
    # generate m random means whose sum is lambda
    mean_arr <- runif(rv, min=1, max=rv)
    mean_arr <- lambda*mean_arr/sum(mean_arr)
    approx_arr <- numeric(0)
    for(j in 1:rv){
      approx_arr <- c(approx_arr, rexp(1, 1/mean_arr[j]))
    }
    approx <- sum(approx_arr)
    list2 <- c(list2, approx)
  }
  return(list2)
}
# Plot
start_time2 <- Sys.time()
list2 <- approx_random()
end_time2 <- Sys.time()
list2_data <- data.frame(t = 1:trials, points = list2)
plot2 <- ggplot(data = list2_data, aes(x = t, y = points)) + geom_point()
plot2 + ggtitle("Random lambda") + xlab("Trials") + ylab("Points")
# Mean
mean2 <- mean(list2)
# Variance
var2 <- var(list2)

# Compare two methods
print(end_time1 - start_time1)
print(end_time2 - start_time2)
print(mean1)
print(mean2)
print(abs(mean1-target_mean))
print(abs(mean2-target_mean))
print(var1)
print(var2)
print(abs(var1-target_var))
print(abs(var2-target_var))

#------Code for Chapter 2.3 Truncated Normal-------
# PLEASE UNCOMMENT THE LINE BELOW IF FIRST RUN THIS SCRIPT
# install.packages('truncnorm') 
library('truncnorm') 
library(ggplot2)
library(ggpubr)
# Define the number of trials
trials <- 10000
# approximate truncated Norm(0, 1) using MOS
mu <- 0
sigma <- 1
# Denote rv as the number of exponential rvs, rv>=1
rv <- 100
# Denote q as the number of quantiles, q>=1
q <- 100
# Mean and Variance of this uniform dist
target_mean <- etruncnorm(a=0, b=Inf, mean=mu, sd=sigma)
target_var <- vtruncnorm(a=0, b=Inf, mean=mu, sd=sigma)

# Simulation on states with equal means
approx_equal <- function(){
  list <- numeric(0)
  for (i in 1:trials)
  {
    prob <- runif(1)
    prob_arr <- seq(0, 1, by=(1/q))
    idx <- findInterval(prob, prob_arr)
    lambda <- qtruncnorm(idx*(1/(q+1)), a=0, b=Inf, mean=mu, sd=sigma)
    approx <- sum(rexp(rv, rv/lambda))
    list <- c(list, approx)
  }
  return(list)
}
# Plot
start_time <- Sys.time()
list <- approx_equal()
end_time <- Sys.time()
list_data <- data.frame(value=list)
plot1 <- ggplot(data = list_data, aes(x = value)) + geom_histogram(fill="#69b3a2", color="#e9ecef")
plot1 + ggtitle("10000 Trials for Norm(0,1) Truncated at 0") + xlab("Values") + ylab("Trials")

# Results
mean <- mean(list)
var <- var(list)
print(end_time - start_time)
print(mean)
print(abs(mean-target_mean))
print(var)
print(abs(var-target_var))

# density plot
plot2 <- plot(density(list,from=0), main="Truncated Norm(0, 1) at 0, q=100, rv=100")
curve(dtruncnorm(x, a=0,b=Inf,mean=0,sd=1), 0, add = TRUE, col = "red")

#------this code is for pi vector computation for problem 2-------

library(truncnorm)
mos_Q=function(P,quant,e_vars)
{
  
  
  #number of states
  n=nrow(P)
  
  #number of branches
  branches=ncol(quant)
  # calculate the possible num of transitions from each state
  adj_l=list()
  count=0
  # the state number of each ori state
  state_num=numeric(0)
  
  for (i in 1:n)
  {
    count=count+1
    possible_vec=numeric(0)
    state_num=c(state_num,count)
    for (j in 1:n)
    {
      # construct the list of lists containing the states that can be reached
      # from state i
      if (P[i,j]>0)
      {
        possible_vec=c(possible_vec,j)
      }
    }
    adj_l=append(adj_l,list(possible_vec))
    count=count+e_vars*branches
  }
  #print(adj_l)
  q_size=count
  Q=matrix(0,nrow=q_size,ncol=q_size)
  #print(q_size)
  #print(state_num)
  # state conversion to one dimension function
  convert=function(i,j,k)
  {
    return(state_num[i]+(j-1)*e_vars+k)
  }
  
  # fill in the transitional probabilities
  for (i in 1:n)
  {
    substates=branches*e_vars
    lamb_list=numeric(0)
    # compute lambda convert(i)
    for (j in 1:branches)
    {
      lambda=(e_vars+1)/quant[i,j]
      lamb_list=c(lamb_list,lambda)
    }
    aver=sum(lamb_list)/branches
    # -lamb i,i
    Q[state_num[i],state_num[i]]=-aver
    for (j in 1:branches)
    {
      Q[state_num[i],convert(i,j,1)]=aver/branches
      for (k in 1:e_vars)
      {
        Q[convert(i,j,k),convert(i,j,k)]=-lamb_list[j]
        if(k!=e_vars)
        {
          Q[convert(i,j,k),convert(i,j,k+1)]=lamb_list[j]
          
        }
        else
        {
          for (l in adj_l[[i]])
          {
            
            Q[convert(i,j,k),state_num[l]]=P[i,l]*lamb_list[j]
          }
        }
      }
    }
  }
  # transpose Q
  Q=t(Q)
  return(Q)
  
}
# compute pi vector
find_pi=function(Q)
{
  num=nrow(Q)
  Q[num,]=rep(1,num)
  b=c(rep(0,num-1),1)
  #b=rbind(b)
  #b=t(b)
  #pi_vec=inv(Q)%*%b
  pi_vec=solve(Q,b)
  return(pi_vec)
}
# to get back the pi vector with original states

mos_pi=function(P,quant,e_vars)
{
  Q=mos_Q(P,quant,e_vars)
  pi_1=find_pi(Q)
  states=nrow(P)
  pi_vec=rep(0,states)
  branches=ncol(quant)
  for (i in 1:states)
  {
    for (j in (i+(i-1)*e_vars*branches):(i+i*e_vars*branches))
    {
      pi_vec[i]=pi_vec[i]+pi_1[j]
    }
  }
  return(pi_vec)
}
unif_quant=function(a_vec,b_vec,divisions)
{
  delta=1/(divisions+1)
  states=length(a_vec)
  quant=matrix(0,states,divisions)
  # generate percentage_vec
  percentage_vec=numeric(0)
  for (i in 1:divisions)
  {
    percentage_vec=c(percentage_vec,i*delta)
  }
  # generate quant matrix
  for (i in 1:states)
  {
    # transform interval
    quant[i,]=(percentage_vec*(b_vec[i]-a_vec[i]))+a_vec[i]
  }
  return(quant)
}

truncnorm_quant=function(mean_vec,sd_vec,divisions)
{
  delta=1/(divisions+1)
  states=length(mean_vec)
  quant=matrix(0,states,divisions)
  # generate percentage_vec
  percentage_vec=numeric(0)
  for (i in 1:divisions)
  {
    percentage_vec=c(percentage_vec,i*delta)
  }
  # generate quant matrix
  for (i in 1:states)
  {
    quant[i,]=qtruncnorm(percentage_vec,a=0,b=Inf,mean=mean_vec[i],sd=sd_vec[i])
  }
  return(quant)
}

# input parameters of distribution for ht of each state,
# the transitional probability matrix P
# the number of exp rvs to use, the number of percentages to use
# output pi_vec
mos_uniform=function(P,a_vec,b_vec,quant_num,rv_num)
{
  quant=unif_quant(a_vec,b_vec,quant_num)
  pi_vec=mos_pi(P,quant,rv_num)
  return(pi_vec)
}
mos_tnormal=function(P,mean_vec,sd_vec,quant_num,rv_num)
{
  quant=truncnorm_quant(mean_vec,sd_vec,quant_num)
  pi_vec=mos_pi(P,quant,rv_num)
  return(pi_vec)
}
r1=c(0.0,0.4,0.6)
r2=c(0.2,0.0,0.8)
r3=c(0.5,0.5,0.0)
P3=rbind(r1,r2,r3)
a3=c(1.2,1.5,1.8)
b3=c(8,9,8)
#for (i in 1:10)
#{
# change quauntiles number first then rv numbers
#pi_vec1=mos_tnormal(P,a_vec,b_vec,i,12)
#pi_vec2=mos_tnormal(P,a_vec,b_vec,i,30)
#err1_vec=abs(pi_vec1-simu_vec)
#err2_vec=abs(pi_vec2-simu_vec)
#err1=sum(err1_vec)
#err2=sum(err2_vec)
#r1=c(r1,err1)
#r2=c(r2,err2)
#}
#x=c(1:10)
#plot(x,r1,main="d wrt quantile",xlab="quantiles",ylab="error",col="red",type="l")
quants=15
#rvs=1

simu_u3=c(0.2521248, 0.3359102, 0.4119650)
e_vec=numeric(0)
x=numeric(0)
for (i in 1:15)
{
  err=sum(abs(mos_uniform(P3,a3,b3,quants,i)-simu_u3))
  e_vec=c(e_vec,err)
  x=c(x,i)
}
plot(x,e_vec,xlab="number of random variables",ylab="d",type="l")

#------this is the simulation code for problem 2-------

#input P, a_vec, b_vec, max_time
#output simulated pi_vec
#simu
library(truncnorm)
uniform_simu=function(P,a_vec,b_vec,max_time)
{
  states=nrow(P)
  #initialize state i, simu_pi, and time
  t=0
  i=sample.int(states,1)
  simu_pi=rep(0,states)
  #generate cumulative probability matrix
  c_matrix=matrix(0,states,states)
  for (i in 1:states)
  {
    for (j in 1:states)
    {
      if (j==1)
      {
        c_matrix[i,j]=P[i,j]
      }
      else
      {
        c_matrix[i,j]=c_matrix[i,j-1]+P[i,j]
      }
    }
  }
  
  while(t<max_time)
  {
    # generate random percentage
    num=runif(1)
    # generate random hitting time
    ht=runif(1,a_vec[i],b_vec[i])
    simu_pi[i]=simu_pi[i]+ht
    #see which state it will jump to
    for (j in 1:states)
    {
      if (j==1)
      {
        if((0<num)&&(num<=c_matrix[i,j]))
        {
          tempr=1
        }
      }
      else
      {
        if((c_matrix[i,j-1]<num)&&(num<=c_matrix[i,j]))
        {
          tempr=j
        }
      }
    }
    i=tempr
    t=t+ht
  }
  return(simu_pi/(sum(simu_pi)))
}
tnormal_simu=function(P,mean_vec,sd_vec,max_time)
{
  states=nrow(P)
  #initialize state i, simu_pi, and time
  t=0
  i=sample.int(states,1)
  simu_pi=rep(0,states)
  #generate cumulative probability matrix
  c_matrix=matrix(0,states,states)
  for (i in 1:states)
  {
    for (j in 1:states)
    {
      if (j==1)
      {
        c_matrix[i,j]=P[i,j]
      }
      else
      {
        c_matrix[i,j]=c_matrix[i,j-1]+P[i,j]
      }
    }
  }
  
  while(t<max_time)
  {
    # generate random percentage
    num=runif(1)
    # generate random hitting time
    ht=rtruncnorm(1,a=0,b=Inf,mean=mean_vec[i],sd=sd_vec[i])
    simu_pi[i]=simu_pi[i]+ht
    #see which state it will jump to
    for (j in 1:states)
    {
      if (j==1)
      {
        if((0<num)&&(num<=c_matrix[i,j]))
        {
          tempr=1
        }
      }
      else
      {
        if((c_matrix[i,j-1]<num)&&(num<=c_matrix[i,j]))
        {
          tempr=j
        }
      }
    }
    i=tempr
    t=t+ht
  }
  return(simu_pi/(sum(simu_pi)))
}

r1=c(0.0,0.4,0.6)
r2=c(0.2,0.0,0.8)
r3=c(0.5,0.5,0.0)
P3=rbind(r1,r2,r3)
a3=c(1.2,1.5,1.8)
b3=c(8,9,8)
#print(uniform_simu(P3,a3,b3,max_time = 120000000))
print(uniform_simu(P3,a3,b3,max_time = 1200000))
#0.2197650 0.1171213 0.2686937 0.2006243 0.1937956
#0.2347546, 0.1363880, 0.2609917, 0.1985844, 0.1692813


