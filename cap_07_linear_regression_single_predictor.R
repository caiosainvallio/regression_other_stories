

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


# Checking the model-fitting procedure ----------------------------------------
# Step 1 - Creating the pretend world
a <- 46.3
b <- 3.0
sigma <- 3.9
x <- hibbs$growth
n <- length(x)


# Step 2 - Simulating fake data
y <- a + b*x + rnorm(n, 0, sigma)
fake <- data.frame(x, y)


# Step 3 - Fitting the model and comparing fitted to assumed values
fit <- stan_glm(y ~ x, data=fake)
print(fit)

b_hat <- coef(fit)["x"]
b_se <- se(fit)["x"]

cover_68 <- abs(b - b_hat) < b_se
cover_95 <- abs(b - b_hat) < 2*b_se
cat(paste("68% coverage: ", cover_68, "\n"))
cat(paste("95% coverage: ", cover_95, "\n"))


# Step 4 - Embedding the simulation in a loop
n_fake <- 1e3
cover_68 <- rep(NA, n_fake)
cover_95 <- rep(NA, n_fake)
for (s in 1:n_fake){
  y <- a + b*x + rnorm(n, 0, sigma)
  fake <- data.frame(x, y)
  fit <- stan_glm(y ~ x, data=fake, refresh=0) # supress output on console
  b_hat <- coef(fit)["x"]
  b_se <- se(fit)["x"]
  cover_68[s] <- abs(b - b_hat) < b_se
  cover_95[s] <- abs(b - b_hat) < 2*b_se
}
cat(paste("68% coverage: ", mean(cover_68), "\n")) # 68% coverage:  0.685 
cat(paste("95% coverage: ", mean(cover_95), "\n")) # 95% coverage:  0.946


# using t distribution
n_fake <- 1e3
cover_68 <- rep(NA, n_fake)
cover_95 <- rep(NA, n_fake)
t_68 <- qt(0.84, n-2)
t_95 <- qt(0.975, n-2)
for (s in 1:n_fake){
  y <- a + b*x + rnorm(n, 0, sigma)
  fake <- data.frame(x, y)
  fit <- stan_glm(y ~ x, data=fake, refresh=0) # supress output on console
  b_hat <- coef(fit)["x"]
  b_se <- se(fit)["x"]
  cover_68[s] <- abs(b - b_hat) < t_68 * b_se
  cover_95[s] <- abs(b - b_hat) < t_95 * b_se
}
cat(paste("68% coverage: ", mean(cover_68), "\n")) # 68% coverage:  0.681 !!!
cat(paste("95% coverage: ", mean(cover_95), "\n")) # 95% coverage:  0.969 !!!




# Formulating comparisons as regression models
# estimating the means is the same as regressing on a constant term
n_0 <- 20
y_0 <- rnorm(n_0, 2.0, 5.0)
fake_0 <-  data.frame(y_0)
print(y_0)

fit_0 <- stan_glm(y_0 ~ 1, data=fake_0, prior_intercept=NULL, prior=NULL, prior_aux=NULL)
print(fit_0)

# estimating a difference is the same as regressing on a indicator variable
n_1 <- 30
y_1 <- rnorm(n_1, 8.0, 5.0)

diff <- mean(y_1) - mean(y_0)
se_0 <- sd(y_0) / sqrt(n_0)
se_1 <- sd(y_1) / sqrt(n_1)
se <- sqrt(se_0^2 + se_1^2)

n <- n_0 + n_1
y <- c(y_0, y_1)
x <- c(rep(0, n_0), rep(1, n_1))
fake <- data.frame(x, y)
fit <- stan_glm(y ~ x, data=fake, prior_intercept=NULL, prior=NULL, prior_aux=NULL)
print(fit)


