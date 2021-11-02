# Plotting the data and fitted model -------------------------------------------

# libraries
library(rstanarm)
library(ggplot2)

# data set
data("kidiq")


# Displaying a regression line as a function of one input variable -------------
fit_2 <- stan_glm(kid_score ~ mom_iq, data=kidiq)

ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point() +
  geom_abline(intercept = coef(fit_2)[1], slope = coef(fit_2)[2]) +
  labs(x = "Mother IQ score", y = "Child test score") +
  jtools::theme_apa()




# Displaying two fitted regression lines ---------------------------------------
## Model with no interaction ---------------------------------------------------

fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq)

ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point(aes(color = factor(mom_hs)), show.legend = FALSE) +
  geom_abline(
    intercept = c(coef(fit_3)[1], coef(fit_3)[1] + coef(fit_3)[2]),
    slope = coef(fit_3)[3],
    color = c("gray", "black")) +
  scale_color_manual(values = c("gray", "black")) +
  labs(x = "Mother IQ score", y = "Child test score") +
  jtools::theme_apa()



