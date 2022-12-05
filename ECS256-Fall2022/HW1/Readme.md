Problem Set 1: Review of undergraduate probability.

Due Wednesday, October 5, 11:59 pm

The goal of this assignment is, as noted, review, but it is required and will count in your course grade, just like any other assignment. Note that the material on iterated expectation is technically undergraduate level, but is more sophisticted than what is usually covered at that level.
Please see our course syllabus for details on writing up and submitting your solutions. Note in particular the requirement that LaTeX and R must be used.


1.
Let X, X1,...,Xn be independent, expponentially distributed random variables with parameter λ.

a. Find the median of X, i.e. a number m such that P(X < m) = 0.5, in terms of λ.

b. Let the random variable M = median(X1,...,Xn). Usually computation of medians must take into account the possibility of tied values; e.g. see this site. But not here. Why not?

c. Find P(X1 < 1.2 X2).

d. Say n = 2, and that Xi is exponentially distributed with parameter λi, i = 1,2. We choose one of the Xi at random, with probability pi. Let T be the result. Find ET and Var(T) in terms of the λi and the pi.


2.
Say the random variable X has an exponential distribution with parameter λ. However, our observation of it is truncated at c, so that our observed value T is min(X,c). Derive a formula for Var(T) as a function of λ and c, using the "Pythagorean Theorem for variance" (Law of Total Variance, SRC p.54). Write simulation code to verify your formula.

3.
The Tower Property says,

E { [E(Y | U,V)] | U} = E(Y | U)
Write simulation code that demonstrates this for a context in which E(Y | U,V) = U + V, so E(Y | U) = U + E(V|U).

A few tips:

- Though this problem simply asks for a mere simulation, you probably will find it rather challenging, and it may generate quite a bit of discussion in your group. As noted in the blog, I'm available for help if you need it.

- If U and/or V are continuous random variables, you'd have to evaluate the conditional quantities with, e.g., U ≈ u instead of U = u. To avoid such complication, I recommend making U and V discrete.

- Make sure you understand the intuitive content here, with probability and expected value being thought of as long-run frequency of occcurrence (PSB, Sec. 2.2) and long-run average, respectively.


Link for PS1: https://web.cs.ucdavis.edu/~matloff/matloff/public_html/256/Assignments/PS1.html
