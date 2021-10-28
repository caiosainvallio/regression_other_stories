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








