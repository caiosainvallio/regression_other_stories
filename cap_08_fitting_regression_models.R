
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


