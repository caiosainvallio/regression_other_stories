# libraries
library(rstanarm)
library(ggplot2)

# dataset
data("kidiq")


# add a binary predictor -------------------------------------------------------
fit_1 <- stan_glm(kid_score ~ mom_hs, data=kidiq)
fit_1    # kid_score = 78 + 12*mom_hs + error

ggplot(kidiq, aes(factor(mom_hs), kid_score)) +
  geom_jitter(width = .08) +
  geom_abline(intercept = coef(fit_1)[1], slope = coef(fit_1)[2]) +
  labs(x = "Mother completed high scool", y = "Child test score") + 
  jtools::theme_apa()



# add a continous predictor ----------------------------------------------------
fit_2 <- stan_glm(kid_score ~ mom_iq, data=kidiq)
fit_2    # kid_score = 26 + 0.6∗mom_iq + error

ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point() +
  geom_abline(intercept = coef(fit_2)[1], slope = coef(fit_2)[2]) +
  labs(x = "Mother IQ score", y = "Child test score") + 
  jtools::theme_apa()



# including booth predictors ---------------------------------------------------
fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq)
fit_3    # kid_score = 26 + 6∗mom_hs + 0.6 mom_iq + error


ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point(aes(color = factor(mom_hs)), show.legend = FALSE) +
  geom_abline(
    intercept = c(coef(fit_3)[1], coef(fit_3)[1] + coef(fit_3)[2]),
    slope = coef(fit_3)[3],
    color = c("gray", "black")) +
  scale_color_manual(values = c("gray", "black")) +
  labs(x = "Mother IQ score", y = "Child test score") + 
  jtools::theme_apa()


