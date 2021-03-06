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



# Diff Poisson and Neg-Binomial ------------------------------------------------
library("loo")
library("ggplot2")
library("bayesplot")
SEED <- 3579

data(roaches)
roaches$roach100 <- roaches$roach1 / 100
head(roaches)



## Binomial negative -----------------------------------------------------------
fit_1 <- stan_glm(y ~ roach100 + treatment + senior, 
                  family=neg_binomial_2,
                  offset=log(exposure2), 
                  data=roaches, seed=SEED, refresh=0)
prior_summary(fit_1)
print(fit_1, digits=2)
loo_1 <- loo(fit_1)


## Graphical posterior predictive checking -------------------------------------
yrep_1 <- posterior_predict(fit_1)
n_sims <- nrow(yrep_1)
sims_display <- sample(n_sims, 100)
ppc_1 <- ppc_dens_overlay(log10(roaches$y+1), log10(yrep_1[sims_display,]+1))+
  xlab('log10(y+1)') +
  theme(axis.line.y = element_blank())
ppc_1
ppc_stat(y=roaches$y, yrep=yrep_1, stat=function(y) mean(y==0))


## Poisson ---------------------------------------------------------------------
fit_2 <- stan_glm(y ~ roach100 + treatment + senior, family=poisson,
                  offset=log(exposure2), data=roaches, seed=SEED, refresh=0)
prior_summary(fit_2)
print(fit_2, digits=2)
loo_2 <- loo(fit_2)

loo_compare(loo_1, loo_2)


## Graphical posterior predictive checking -------------------------------------
yrep_2 <- posterior_predict(fit_2)
n_sims <- nrow(yrep_2)
sims_display <- sample(n_sims, 100)
ppc_2 <- ppc_dens_overlay(log10(roaches$y+1), log10(yrep_2[sims_display,]+1)) +
  xlim(0,3) +
  xlab('log10(y+1)') +
  theme(axis.line.y = element_blank())
ppc_2


## Compare plot ----------------------------------------------------------------
pbg <- bayesplot_grid(ppc_2, ppc_1,
                      grid_args = list(ncol = 2),
                      titles = c("Poisson", "negative-binomial"))
pbg







# Logistic-binomial model ------------------------------------------------------
# We first simulate N = 100 players each shooting n = 20 shots, where the 
# probability of a successful shot is a linear function of height (30% for a
# 5'9" player, 40% for a 6' tall player, and so forth):

N <- 100
height <- rnorm(N, 72, 3)
p <- 0.4 + 0.1*(height - 72)/3
n <- rep(20, N)
y <- rbinom(N, n, p)
data <- data.frame(n=n, y=y, height=height)


fit_1a <- stan_glm(cbind(y, n-y) ~ height, 
                   family=binomial(link="logit"),
                   data=data)
print(fit_1a)






# Zero-inflated negative-binomial model ----------------------------------------
library("brms")
roaches$logp1_roach1 <- log(roaches$roach1+1)
roaches$log_exposure2 <- log(roaches$exposure2)

fit_3 <- brm(bf(y ~ logp1_roach1 + treatment + senior + offset(log_exposure2),
                zi ~ logp1_roach1 + treatment + senior + offset(log_exposure2)),
             family=zero_inflated_negbinomial(), data=roaches,
             prior=set_prior("normal(0,1)"), seed=SEED, refresh=500)

print(fit_3)
loo_3 <- loo(fit_3)
loo_compare(loo_1, loo_3)

yrep_3 <- posterior_predict(fit_3)
ppc_3 <- ppc_dens_overlay(log10(roaches$y+1), log10(yrep_3[sims_display,]+1))+
  xlab('log10(y+1)') +
  theme(axis.line.y = element_blank())
ppc_3

ppc_stat(y=roaches$y, yrep=yrep_3, stat=function(y) mean(y==0))
ppc_stat(y=roaches$y, yrep=yrep_3, stat=function(y) quantile(y, probs=0.95))
