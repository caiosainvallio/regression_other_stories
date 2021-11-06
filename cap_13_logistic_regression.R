# Logistic Regression ----------------------------------------------------------

# libraries
library(rstanarm)
library(ggplot2)

# data
URL <- "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/NES/data/nes.txt"
nes92 <- read.table(URL, header = TRUE)

fit_1 <- stan_glm(rvote ~ income, family=binomial(link="logit"), data=nes92)
print(fit_1, digits = 3)

plot(nes92$income, nes92$rvote)
curve(invlogit(coef(fit_1)[1] + coef(fit_1)[2]*x), add=TRUE)


# Probability interpretation
invlogit(-0.687 + 0.232*mean(nes92$income))
plogis(-0.687 + 0.232*mean(nes92$income))

invlogit(coef(fit_1)[1] + coef(fit_1)[2]*mean(nes92$income))
plogis(coef(fit_1)[1] + coef(fit_1)[2]*mean(nes92$income))

# A difference of 1 in income (on this 1–5 scale) corresponds to a positive 
# difference of 0.23 in the logit probability of supporting Bush.


# log(odds) interpretation
exp(0.232)





# Predictions and comparisons --------------------------------------------------

## Point prediction using `predict` --------------------------------------------
new <- data.frame(income=5)

# probability scale
# specified type="response" to get the prediction on the probability scale
pred <- predict(fit_1, type="response", newdata=new)
pred

# or directly
invlogit(-0.687 + 0.232*5)


# log(odds) scale
# type="link" to get the estimated linear predictor, 
# the prediction on the logit scale. y=log(odds)
predict(fit_1, type="link", newdata=new)

# to revert in probabilities
invlogit(predict(fit_1, type="link", newdata=new))





## Linear predictor with uncertainty using `posterior_linpred` -----------------
# the function posterior_linpred yields simulation draws for the linear
# predictor, X_new*beta

# log(odds) scale
linpred <- posterior_linpred(fit_1, newdata=new)
median(linpred)
print(c(mean(linpred), sd(linpred)))






## Expected outcome with uncertainty using `posterior_epred` -------------------

# probability scale
epred <- posterior_epred(fit_1, newdata=new)
median(epred)
print(c(mean(epred), sd(epred)))




## Predictive distribution for a new observation using `posterior_predict` -----
postpred <- posterior_predict(fit_1, newdata=new)
mean(postpred)




# Prediction given a range of input values -------------------------------------
new <- data.frame(income=1:5)
pred <- predict(fit_1, type="response", newdata=new)
linpred <- posterior_linpred(fit_1, newdata=new)
epred <- posterior_epred(fit_1, newdata=new)
postpred <- posterior_predict(fit_1, newdata=new)


# We can use epred to make statements about the population. For example, this 
# will compute the posterior probability, according to the fitted model, that 
# Bush was more popular among people with income level 5 than among people with 
# income level 4.
mean(epred[,5] > epred[,4])

# And this will compute a 95% posterior distribution for the difference in 
# support for Bush, comparing people in the richest to the second-richest 
# category:
quantile(epred[,5] - epred[,4], c(0.025, 0.975))


# We can use `postpred` to make statements about individual people. For example,
# this will compute the posterior simulations of the number of these new 
# survey respondents who support Bush:
total <- apply(postpred, 1, sum)


# And here is how to compute the probability that at least three of 
# them support Bush:
mean(total >= 3)


