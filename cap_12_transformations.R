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
# data set
URL <- "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv"
earnings <- read.csv(URL)



