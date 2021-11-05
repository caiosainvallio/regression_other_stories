# Logistic Regression ----------------------------------------------------------

# libraries
library(rstanarm)
library(ggplot2)

# data
URL <- "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/NES/data/nes.txt"
nes92 <- read.table(URL, header = TRUE)

fit_1 <- stan_glm(rvote ~ income, family=binomial(link="logit"), data=nes92)
print(fit_1, digits = 3)

plot(nes92$income, nes92$rvote)
curve(invlogit(coef(fit_1)[1] + coef(fit_1)[2]*x), add=TRUE)


# probabilidade usando a média de income
invlogit(-0.687 + 0.232*mean(nes92$income))
plogis(-0.687 + 0.232*mean(nes92$income))

invlogit(coef(fit_1)[1] + coef(fit_1)[2]*mean(nes92$income))
plogis(coef(fit_1)[1] + coef(fit_1)[2]*mean(nes92$income))

# A difference of 1 in income (on this 1–5 scale) corresponds to a positive 
# difference of 0.33 in the logit probability of supporting Bush.

# log(odds) interpretation
exp(0.232)


