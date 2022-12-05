# Problem Set 2: Markov chains

## 1.
Consider a finite-state space, aperiodic Markov chain with transition matrix P. Our textbook shows a way to find the stationary probability vector π that is alternative to the usual method that solves a system of linear equations. Say the matrix is of size r x r. The thrust of the argument is as follows:

The power Pk is the k-step transition matrix. So, row 1 of the matrix is P(Xk = j | X0 = 1), j = 1,...,r. Row 2 is P(Xk = j | X0 = 2), j = 1,...,r, and so on.
We know that for an aperiodic chain, P(Xk = j | X0 = i) → πj as k → ∞.
Thus for large k, each row of Pk will be approximately the vector π'.
So, to find π, we could raise P to a large power, then use row 1 as π.
But even better, we could average all the rows, which presumably will give us a better estimate.
Investigate the accuracy of this approach, as follows:

Do your experiments for one small P and one large one, of your choice.
For values of k = 1,2,3,...,m (your choice of m): Find the estimates of π, both using row 1 only and averaging all the rows. Take as your accuracy criterion the l1 distance (sum of absolute differences) of the estimated π to the actual value.
Show your results graphically, with commentary in a .tex file.

## 2.
Consider the states of a Markov chain over time, X0, X1, X2, ..., with the state space being some subset of the set of all integers, which we will take to be 1,...,m. Suppose the chain has a stationary distribution π, and that X0 has this distribution.

There is a term from time series analysis, autocorrelation: ρ(k), defined to be the correlation beteen Xi and Xi+k. Note that this quantity depends only on k, not i.

Explain why there is no dependence on i (.tex file).
Write an R function with call form
MCautocor(k,P)
(Note that an argument m is not needed. Explain why.)

## 3.
Consider the ALOHA model with 3 stations. Derive the long- run average time between collisions. Expression your answer in terms of p, q and π, and evaluate for the case p = 0.4, q = 0.3. Show your reasoning in a .tex file.

## 4.
Consider a Markov chain with finite state space.

Write an R function with call form
eTij(P)  # P is the transition matrix
giving the values of ETij.

Do the same for variance, writing a function
varTij(P)  # P is the transition matrix
that finds all Var(Tij), where Tij is the same it takes to go from state i to state j. Show your derivation in a .tex file.
