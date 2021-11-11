# Design and sample size decisions ---------------------------------------------

## Simple comparisons of proportions: unequal sample sizes ---------------------

prop_int <- 0.25 # 1/4
prop_cont <- 0.75 # 3/4

# Standard error of the difference
sqrt(p_int*(1 - p_int)/(prop_int*n) + p_cont*(1 - p_cont)/(prop_cont*n))

# upper bound for proportions = 0.5
# 0.5*sqrt(1/(prop_int*n) + 1/(prop_cont*n))
# 0.5*sqrt(1/prop_int + 1/prop_cont) / sqrt(n)
# 1.154 / sqrt(n)

# n = (1.154/se)^2

# n to achieve 80% power to detect a difference of 10%, with 25% of the sample
# size in one group and 75% in the other: p_int - p_cont > 10%

# for 10% to be 2.8 se from zero: n > (2.8*1.15/0.10)^2
n <- (2.8*1.15/0.10)^2

n*.25 # in the int group
n*.75 # in the cont group



## The 2.8 ---------------------------------------------------------------------
# To find the value of n such that exactly 80% of the estimates will be at 
# least 1.96 standard errors from 0.5, we need:
# 
# 0.5 + 1.96*s.e. = 0.6 - 0.84*s.e.
# 
# Some algebra then yields (1.96 + 0.84) * s.e. = 0.1.
# 
# We can then substitute s.e. = 0.5/sqrt(n) and solve for n.
# 
# In summary, to have 80% power, the true value of the parameter must be 2.8 
# standard errors away from the comparison point: the value 2.8 is 1.96 from 
# the 95% interval, plus 0.84 to reach the 80th percentile of the normal 
# distribution.



# Estimates of means -----------------------------------------------------------
# The quick estimate of Theta is the sample mean, y_hat, which has a standard
# error of Sigma/sqrt(n), where Sigma is the standard deviation of y in the 
# population. 

# If the goal is to achieve a specific s.e. for y_hat, then the sample size must
# be at least n = (Sigma/s.r.)^2.

# If the goal is 89% power to distinguish Theta from a specified value Theta_0,
# then a conservative required sample size is n = (2.8Sigma/(Theta - Theta_0))^2.

# *2.8 standard errors from zero.




