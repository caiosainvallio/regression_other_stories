# fitting a simple regression to fake data

# library
library(rstanarm)

# fake dataset
x <- 1:20
n <- length(x)
a <- 0.2
b <- 0.3
sigma <- 0.5
y <- a + b*x + sigma*rnorm(n)
fake <- data.frame(x, y)

# fit the model
fit_1 <- stan_glm(y ~ x, data = fake)

# display the results
print(fit_1, digits = 2)

# plot the data and the fitted line
plot(fake$x, fake$y, main = "Data and fitted regression line")
a_hat <- coef(fit_1)[1]
b_hat <- coef(fit_1)[2]
abline(a_hat, b_hat)
x_bar <- mean(fake$x)
text(x_bar, a_hat + b_hat*x_bar, 
     paste("y =", round(a_hat, 2), "+", round(b_hat, 2), "* x"), adj = 0)









# intrerpret coefficients as comparison, not effects
earnings <- read.csv("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv")

earnings$earnk = earnings$earn / 1e3

fit_2 <- stan_glm(earnk ~ height + male, data = earnings)
print(fit_2)

R2 <- 1 - sigma(fit_2)^2 / sd(earnings$earnk)^2
R2

# it would be inappropriate to say that the estimated “effect of sex” is $10 600. 
# Better to say that, when comparing two people with the same height but different sex, 
# the man’s earnings will be, on average, $10 600 more than the woman’s in the fitted model.




