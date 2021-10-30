

theta_hat_prior <- 0.524
se_prior <- 0.041
n <- 400
y <- 190
theta_hat_data <- y/n
se_data <- sqrt((y/n)*(1-y/n)/n)
theta_hat_bayes <- (theta_hat_prior/se_prior^2 + theta_hat_data/se_data^2) /
                   (1/se_prior^2 + 1/se_data^2)
se_bayes <- sqrt(1/(1/se_prior^2 + 1/se_data^2))



vignette("priors", package="rstanarm")
