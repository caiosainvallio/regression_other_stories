# Plotting the data and fitted model -------------------------------------------

# libraries
library(rstanarm)
library(ggplot2)

# data set
data("kidiq")


# Displaying a regression line as a function of one input variable -------------
fit_2 <- stan_glm(kid_score ~ mom_iq, data=kidiq)
fit_2
ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point() +
  geom_abline(intercept = coef(fit_2)[1], slope = coef(fit_2)[2]) +
  labs(x = "Mother IQ score", y = "Child test score") +
  jtools::theme_apa()




# Displaying two fitted regression lines ---------------------------------------
## Model with no interaction ---------------------------------------------------

fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq)
fit_3
ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point(aes(color = factor(mom_hs)), show.legend = FALSE) +
  geom_abline(
    intercept = c(coef(fit_3)[1], coef(fit_3)[1] + coef(fit_3)[2]),
    slope = coef(fit_3)[3],
    color = c("gray", "black")) +
  scale_color_manual(values = c("gray", "black")) +
  labs(x = "Mother IQ score", y = "Child test score") +
  jtools::theme_apa()




## Model with interaction ------------------------------------------------------
fit_4 <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=kidiq)
fit_4
ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point(aes(color = factor(mom_hs)), show.legend = FALSE) +
  geom_abline(
    intercept = c(coef(fit_4)[1], sum(coef(fit_4)[1:2])),
    slope = c(coef(fit_4)[3], sum(coef(fit_4)[3:4])),
    color = c("gray", "black")) +
  scale_color_manual(values = c("gray", "black")) +
  labs(x = "Mother IQ score", y = "Child test score") +
  jtools::theme_apa()





# Displaying uncertainty in the fitted regression ------------------------------
fit_2
sims_2 <- as.matrix(fit_2)
n_sims_2 <- nrow(sims_2)
subset <- sample(n_sims_2, 10)

ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point() +
  geom_abline(
    intercept = sims_2[subset, 1],
    slope = sims_2[subset, 2],
    color = "gray",
    size = 0.25) +
  geom_abline(
    intercept = coef(fit_2)[1],
    slope = coef(fit_2)[2],
    size = 0.75) +
  labs(x = "Mother IQ score", y = "Child test score") +
  jtools::theme_apa()




# Displaying using one plot for each input variable ----------------------------
fit_3
sims_3 <- as.matrix(fit_3)
n_sims_3 <- nrow(sims_3)

par(mar=c(3,3,1,3), mgp=c(1.7, .5, 0), tck=-.01)
par(mfrow=c(1,2))
plot(kidiq$mom_iq, kidiq$kid_score, xlab="Mother IQ score", ylab="Child test score", bty="l", pch=20, xaxt="n", yaxt="n")
axis(1, seq(80, 140, 20))
axis(2, seq(20, 140, 40))
mom_hs_bar <- mean(kidiq$mom_hs)
subset <- sample(n_sims_3, 10)
for (i in subset){
  curve(cbind(1, mom_hs_bar, x) %*% sims_3[i,1:3], lwd=.5,
        col="gray", add=TRUE)
}
curve(cbind(1, mom_hs_bar, x) %*% coef(fit_3), col="black", add=TRUE)
jitt <- runif(nrow(kidiq), -.03, .03)
plot(kidiq$mom_hs + jitt, kidiq$kid_score, xlab="Mother completed high school", ylab="Child test score", bty="l", pch=20, xaxt="n", yaxt="n")
axis(1, c(0,1))
axis(2, seq(20, 140, 40))
mom_iq_bar <- mean(kidiq$mom_iq)
for (i in subset){
  curve(cbind(1, x, mom_iq_bar) %*% sims_3[i,1:3], lwd=.5,
        col="gray", add=TRUE)
}
curve(cbind(1, x, mom_iq_bar) %*% coef(fit_3), col="black", add=TRUE)







# Plotting the outcome vs. a continuous predictor ------------------------------
# a continuous outcome y is modeled given a treatment
# indicator z and a continuous pre-treatment predictor x:
# y = a + b*x + theta*z + error

N <- 100
x <- runif(N, 0, 1)
z <- sample(c(0, 1), N, replace=TRUE)
a <- 1
b <- 2
theta <- 5
sigma <- 2
y <- a + b*x + theta*z +  rnorm(N, 0, sigma)
fake <- data.frame(x=x, y=y, z=z)
fit <- stan_glm(y ~ x + z, data=fake)

par(mfrow=c(1,2), mar=c(3,3,2,2), mgp=c(1.7,.5,0), tck=-.01)
for (i in 0:1){
  plot(range(x), range(y), type="n", xlab="Pre-treatment predictor, x", ylab="Outcome, y", main=paste("z =", i), bty="l")
  points(x[z==i], y[z==i], pch=20+i)
  abline(coef(fit)["(Intercept)"] + coef(fit)["z"]*i, coef(fit)["x"])
}






# Forming a linear predictor from a multiple regression ------------------------
# extend the previous example so that the treatment indicator z is accompanied 
# by k pre-treatment predictors x_k , k = 1,..., K:
# y = b_0 + b_1*x_1 + ... + b_k*x_k + theta*z + error

N <- 100
K <- 10
X <- array(runif(N*K, 0, 1), c(N, K))
z <- sample(c(0, 1), N, replace=TRUE)
a <- 1
b <- 1:K
theta <- 10
sigma <- 5
y <- a + X %*% b + theta*z +  rnorm(N, 0, sigma)
fake <- data.frame(X=X, y=y, z=z)

fit <- stan_glm(y ~ X + z, data=fake)
fit


# Outcome plotted vs. fitted linear predictor y_hat
# for control and treatment groups:
y_hat <- predict(fit)
par(mfrow=c(1,2), mar=c(3,3,2,2), mgp=c(1.7,.5,0), tck=-.01)
par(mfrow=c(1,2), pty="s")
for (i in 0:1){
  plot(range(y_hat,y), 
       range(y_hat,y), 
       type="n", 
       xlab=expression(paste("Linear predictor, ", hat(y))), 
       ylab="Outcome, y", main=paste("z =", i), bty="l")
  points(y_hat[z==i], y[z==i], pch=20+i)
  abline(0, 1)
}

# the residual, r = y − y_hat, vs. the fitted linear predictor y
# for control and treatment groups:
r <- y - y_hat
par(mfrow=c(1,2), mar=c(3,3,2,2), mgp=c(1.7,.5,0), tck=-.01)
par(mfrow=c(1,2))
for (i in 0:1){
  plot(range(y_hat), 
       range(r), 
       type="n", 
       xlab=expression(paste("Linear predictor, ", hat(y))), 
       ylab="Residual, r", main=paste("z =", i), bty="l")
  points(y_hat[z==i], r[z==i], pch=20+i)
  abline(0, 0)
}




# Residual plots ---------------------------------------------------------------
URL <- "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Introclass/data/gradesW4315.dat"
introclass <- read.table(file=URL, header=TRUE)

fit_1 <- stan_glm(final ~ midterm, data=introclass)
fit_1

predicted <- predict(fit_1)
resid <- introclass$final - predicted







