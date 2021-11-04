# Transformations and regression -----------------------------------------------

# libraries
library(rstanarm)
library(ggplot2)

# data set
data("kidiq")


# Linear transformations -------------------------------------------------------
fit_4 <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=kidiq)
fit_4

# não da para comparar os betas

kidiq$z_mom_hs <- (kidiq$mom_hs - mean(kidiq$mom_hs))/(2*sd(kidiq$mom_hs))
kidiq$z_mom_iq <- (kidiq$mom_iq - mean(kidiq$mom_iq))/(2*sd(kidiq$mom_iq))

fit_4_z <- stan_glm(kid_score ~ z_mom_hs + z_mom_iq + z_mom_hs:z_mom_iq, 
                    data=kidiq)
fit_4_z




# Logarithmic transformations --------------------------------------------------
# When additivity and linearity are not reasonable assumptions

# data set
URL <- "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv"
earnings <- read.csv(URL)


logmodel_1 <- stan_glm(log(earn) ~ height, data=earnings, subset=earn>0)
print(logmodel_1, digits = 3)


# The estimate beta_hat = 0.06 implies that a difference of 1 inch in height 
# corresponds to an expected difference of 0.06 in log(earnings), so that 
# earnings are multiplied by exp(0.06). exp(0.06) ≈ 1.06

fit_1 <- stan_glm(earn ~ height, data=earnings)
yrep_1 <- posterior_predict(fit_1)
n_sims <- nrow(yrep_1)
subset <- sample(n_sims, 100)
library("bayesplot")
ppc_dens_overlay(earnings$earn, yrep_1[subset,])
# the fit on the untransformed scale is poor: observed earnings in these data 
# are always positive, while the predictive replications contain many
# negative values.

yrep_log_1 <- posterior_predict(logmodel_1)
n_sims <- nrow(yrep_log_1)
subset <- sample(n_sims, 100)
ppc_dens_overlay(log(earnings$earn[earnings$earn>0]), yrep_log_1[subset,])


# Adding another predictor
# A difference of an inch of height corresponds to a difference 
# of 6% in earnings:
print(logmodel_1, digits = 3)

# But men are mostly taller than women and also tend to have higher
# earnings. Perhaps the 6% predictive difference can be understood by 
# differences between the sexes. Do taller people earn more, on average,
# than shorter people of the same sex?

logmodel_2 <- stan_glm(log(earn) ~ height + male, data=earnings, subset=earn>0)
print(logmodel_2, digits = 3)

# After adjusting for sex, each inch of height corresponds to an estimated 
# predictive difference of 2%: under this model, two people of the same sex but 
# differing by 1 inch in height will differ, on average, by 2% in earnings.

# The predictive comparison of sex, however, is huge: comparing a man and a
# woman of the same height, the man’s earnings are exp(0.37) = 1.45 times the 
# woman’s, that is, 45% more. (We cannot simply convert the 0.37 to 45% because 
# this coefficient is not so close to zero).






