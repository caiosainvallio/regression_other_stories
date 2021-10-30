

# Predicting presidential vote share from the economy

# libraries 
library(rstanarm)

# dataset
URL = "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat"
hibbs <- read.table(URL, header = TRUE)

# plot relation
plot(hibbs$growth, hibbs$vote, xlab="Economic growth", ylab="Incumbent party's vote share")

# fit the model
M1 <- stan_glm(vote ~ growth, data=hibbs)
print(M1)
abline(coef(M1))



1 - pnorm(50, 52.3, 3.9)



