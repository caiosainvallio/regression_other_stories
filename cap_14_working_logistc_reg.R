# Working with logistic regression ---------------------------------------------

library("rstanarm")
options(mc.cores = parallel::detectCores())
logit <- qlogis
invlogit <- plogis


# Simulate fake data from logit model ------------------------------------------
n <- 50
a <- 2
b <- 3
x_mean <- -a/b
x_sd <- 4/b
x <- rnorm(n, x_mean, x_sd)
y <- rbinom(n, 1, invlogit(a + b*x))
fake_1 <- data.frame(x, y)
head(fake_1)



# Fit the model and save the coefficient estimates -----------------------------
fit_1 <- stan_glm(y ~ x, family=binomial(link="logit"), data=fake_1)
a_hat <- coef(fit_1)[1]
b_hat <- coef(fit_1)[2]


## Graph data and underying and fitted logistic curves -------------------------
shifted <- function(a, delta=0.008) return(ifelse(a==0, delta, ifelse(a==1, 1 - delta, a)))

par(mar=c(3,3,2,1), mgp=c(1.5,.5,0), tck=-.01)
plot(x, shifted(y), ylim=c(0, 1), xlab="x", ylab="y", yaxs="i", pch=20)
curve(invlogit(a + b*x), add=TRUE, col="gray30")
curve(invlogit(a_hat + b_hat*x), add=TRUE, lty=2, col="gray30")
x0 <- (1.5 - a) / b
text(x0, invlogit(1.5), paste("   True curve,   \n   y = invlogit(", round(a, 1), " + ", round(b, 1), "x)   ", sep=""), cex=.9, col="gray30",
     adj=if (a_hat + b_hat*x0 > 1.5) 0 else 1)
x0 <- (-1.5 - a_hat) / b_hat
text(x0, invlogit(-1.5), paste("   Fitted curve,   \n   y = invlogit(", round(a_hat, 1), " + ", round(b_hat, 1), "x)   ", sep=""), cex=.9, col="gray30",
     adj=if (a + b*x0 > -1.5) 0 else 1)



## Binned plot -----------------------------------------------------------------
K <- 5
bins <- as.numeric(cut(x, K))
x_bar <- rep(NA, K)
y_bar <- rep(NA, K)
for (k in 1:K){
  x_bar[k] <- mean(x[bins==k])
  y_bar[k] <- mean(y[bins==k])
}

par(mar=c(3,3,2,1), mgp=c(1.5,.5,0), tck=-.01)
plot(x, shifted(y), ylim=c(0, 1), xlab="x", ylab="y", yaxs="i", pch=20, cex=.8, main="Data and binned averages", cex.main=.9, col="gray50")
points(x_bar, shifted(y_bar, 0.02), pch=21, cex=1.5)





# Logistic regression as function of two predictors ----------------------------
n <- 100
beta <- c(2, 3, 4) # arbitrary choice of true coefficients in the model
x1 <- rnorm(n, 0, 0.4) # somewhat arbitary choice of scale of data, set so there will be a good mix of 0's and 1's
x2 <- rnorm(n, -0.5, 0.4)
y <- rbinom(n, 1, invlogit(cbind(rep(1, n), x1, x2) %*% beta))
fake_2 <- data.frame(x1, x2, y)

fit_2 <- stan_glm(y ~ x1 + x2, family=binomial(link="logit"), data=fake_2, refresh=0)
beta_hat <- coef(fit_2)


par(mar=c(3,3,2,1), mgp=c(1.5,.5,0), tck=-.01)
plot(x1, x2, bty="l", type="n", main="Data and 10%, 50%, 90% discrimination lines\nfrom fitted logistic regression", cex.main=.9) 
points(x1[y==0], x2[y==0], pch=20)  # dots
points(x1[y==1], x2[y==1], pch=21) # circles
abline(-beta[1]/beta[3], -beta[2]/beta[3])
abline((logit(0.9) - beta[1])/beta[3], -beta[2]/beta[3], lty=2)
abline((logit(0.1) - beta[1])/beta[3], -beta[2]/beta[3],  lty=2)










# Logistic regression with interactions ----------------------------------------

library("rstanarm")
options(mc.cores = parallel::detectCores())
logit <- qlogis
invlogit <- plogis

# data
URL = "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Arsenic/data/wells.csv"
wells <- read.csv(URL)

fit_4 <- stan_glm(switch ~ dist100 + arsenic + dist100:arsenic,
                  family=binomial(link="logit"), data=wells)
print(fit_4, digits=2)


## Constant term -----------------------------------------
invlogit(-0.15) # 0.462

# 0.463 is the estimated probability of switching, if the distance
# to the nearest safe well is 0 and the arsenic level of the current well is 0.
# 
# This is an impossible condition (since arsenic levels all exceed 0.5 in 
# our set of unsafe wells), so we do not try to interpret the constant term.
# 
# Instead, we can evaluate the prediction at the average values of 
# `dist100` = 0.48 and `arsenic` = 1.66, where the probability of switching 
# is logit^{−1}(−0.15 − 0.58 ∗ 0.48 + 0.56 ∗ 1.66 − 0.18 ∗ 0.48 ∗ 1.66) = 0.59.
#
invlogit(-0.15 - 0.58*0.48 + 0.56*1.66 - 0.18*0.48*1.66) # 0.59
# na media, 59% das pessoas irao se mudar

## In term of odd ratio
exp(-0.15) # 0.86
# as same as
0.46/(1-0.46) # 0.85
exp(-0.15 - 0.58*0.48 + 0.56*1.66 - 0.18*0.48*1.66) # 1.43
# na media, a taxa de mudanca é de 1.43






