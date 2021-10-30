
# computing the sum of squares directly
rss <- function(x, y, a, b){        # x and y are vectors, a and b are scalars
  resid <- y - (a + b*x)
  return(sum(resid^2))
}


# Bayesian model with weak priors
stan_glm(y ~ x, data=data)

# Bayesian with no prior (maximum penalized estimate)
stan_glm(y ~ x, data=data, prior_intercept=NULL, prior=NULL, prior_aux=NULL)

# Bayesian with no prior (maximum likelihood estimate)
stan_glm(y ~ x, data=data, prior_intercept=NULL, prior=NULL, prior_aux=NULL, algotithm="optimizing")


# Confidence intervals, uncertainty intervals, compatibility intervals
library(rstanarm)
x <- 1:10
y <- c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
fake <- data.frame(x, y)
fit <- stan_glm(y ~ x, data=fake)
print(fit)

sims <- as.matrix(fit)
quantile(sims[,"x"], c(0.025, 0.975))

