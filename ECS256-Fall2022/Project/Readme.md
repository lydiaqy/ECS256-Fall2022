# ECS 256 Term Project

## Problem A:
Here we will model a health maintenance organization (HMO) advice system. Nurses field calls from patients, who describe their symptoms. Advice might be to, say monitor the situation for another day, make an appointment with a physician or whatever. Here are the variables (note: probability convention is capital letters for random variables) and parameters (lower-case).

There is a virtual waiting room, i.e. the queue, accommodating a maximum of m patients. The number currently waiting is K.
The number of nurses N will vary in time from 0 to n, as follows:
If upon completion of a phone call, there are 0 patients waiting in the queue, the number of nurses will be reduced by 1 (with a minimum of 0). If a patient arrival occurs at the time the queue is full, either 1 new nurse will be added (with a maximum of n), with probability p, or the call will be terminated, with an apology to the patient.

Call duration is exponentially distributed with mean μ.
Intercall arrival time is exponentially distributed with mean ν.
Using parameter values, and especially success criteria, of your choice, investigate the effects of varying the parameters.

## Problem B:
Here you will explore the value of using the Method of Stages (MoS) to circumvent the restriction that event times in continuous-time Markov chains must have exponential distributions. The method itself is described in the blog post of Oct. 28, 1625.

The question you will address is simple:

Is the MoS an accurate and feasible approach to the problem of nonexponential distributions in Markov chains?
This problem is highly open-ended. Make sure to stick to this central issue, though: How well does the MoS work for Markov chains? Do not simply investigate the question of whether the MoS is a good approximation to a nonexponential distribution in its own right.
