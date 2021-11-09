# glm models -------------------------------------------------------------------
library("rstanarm")
options(mc.cores = parallel::detectCores())
logit <- qlogis
invlogit <- plogis


# Poisson ----------------------------------------------------------------------
n <- 50
x <- runif(n, -2, 2)
a <- 1
b <- 2
linpred <- a + b*x
y <- rpois(n, exp(linpred))
fake <- data.frame(x=x, y=y)

fit_fake <- stan_glm(y ~ x, family=poisson(link="log"), data=fake)
print(fit_fake, digits=3)

plot(x, y)
curve(exp(coef(fit_fake)[1] + coef(fit_fake)[2]*x), add=TRUE)




# Negative biniomial -----------------------------------------------------------
phi_grid <- c(0.1, 1, 10)
K <- length(phi_grid)
y_nb <- as.list(rep(NA, K))
fake_nb <- as.list(rep(NA, K))
fit_nb <- as.list(rep(NA, K))

library("MASS")
for (k in 1:K){
  y_nb[[k]] <- rnegbin(n, exp(linpred), phi_grid[k])
  fake_nb[[k]] <- data.frame(x=x, y=y_nb[[k]])
  fit_nb[[k]] <- stan_glm(y ~ x, family=neg_binomial_2(link="log"), data=fake)
  print(fit_nb[[k]])
}


for (k in 1:K) {
  plot(x, y_nb[[k]])
  curve(exp(coef(fit_nb[[k]])[1] + coef(fit_nb[[k]])[2]*x), add=TRUE)
}











