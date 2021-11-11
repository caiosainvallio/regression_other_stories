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

