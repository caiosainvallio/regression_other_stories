# Logistic Regression ----------------------------------------------------------

# libraries
library(rstanarm)
library(ggplot2)

# data
URL <- "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/NES/data/nes.txt"
nes92 <- read.table(URL, header = TRUE)

fit_1 <- stan_glm(rvote ~ income, family=binomial(link="logit"), data=nes92)
print(fit_1)

plot(nes92$income, nes92$rvote)
curve(invlogit(coef(fit_1)[1] + coef(fit_1)[2]*x), add=TRUE)


