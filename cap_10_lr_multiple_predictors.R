# libraries
library(rstanarm)
library(ggplot2)

# data set
data("kidiq")




# add a binary predictor -------------------------------------------------------
fit_1 <- stan_glm(kid_score ~ mom_hs, data=kidiq)
fit_1    # kid_score = 78 + 12*mom_hs + error

ggplot(kidiq, aes(factor(mom_hs), kid_score)) +
  geom_jitter(width = .08) +
  geom_abline(intercept = coef(fit_1)[1], slope = coef(fit_1)[2]) +
  labs(x = "Mother completed high scool", y = "Child test score") + 
  jtools::theme_apa()





# add a continuous predictor ----------------------------------------------------
fit_2 <- stan_glm(kid_score ~ mom_iq, data=kidiq)
fit_2    # kid_score = 26 + 0.6∗mom_iq + error

ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point() +
  geom_abline(intercept = coef(fit_2)[1], slope = coef(fit_2)[2]) +
  labs(x = "Mother IQ score", y = "Child test score") + 
  jtools::theme_apa()





# including booth predictors ---------------------------------------------------
fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq)
fit_3    # kid_score = 26 + 6∗mom_hs + 0.6*mom_iq + error


ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point(aes(color = factor(mom_hs)), show.legend = FALSE) +
  geom_abline(
    intercept = c(coef(fit_3)[1], coef(fit_3)[1] + coef(fit_3)[2]),
    slope = coef(fit_3)[3],
    color = c("gray", "black")) +
  scale_color_manual(values = c("gray", "black")) +
  labs(x = "Mother IQ score", y = "Child test score") + 
  jtools::theme_apa()






# Interpreting regression coefficients (counterfactual interpretation) ---------

# This sort of interpretation arises in causal inference:

# “a change of 10 in maternal IQ causes,or is associated with, a change of
# 6 points in child’s score.”

# “uma mudança de 10 no QI materno causa, ou está associada a, uma mudança de
# 6 pontos na pontuação da criança.”



# More correct form:

# “When comparing two children whose mothers have the same level of education, 
# the child whose mother is x IQ points higher is predicted to have a test score 
# that is 6x higher, on average.”

# “Ao comparar duas crianças cujas mães têm o mesmo nível de educação, a criança
# cuja mãe é x pontos de QI mais alta, prevê-se que tenha uma pontuação de teste 
# 6x mais alta, em média.”










# Interactions -----------------------------------------------------------------
fit_4 <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=kidiq)
fit_4 # kid_score = -11 + 51*mom_hs + 1*mom_iq - 0.5*mom_hs*mom_iq + error

ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point(aes(color = factor(mom_hs)), show.legend = FALSE) +
  geom_abline(
    intercept = c(coef(fit_4)[1], sum(coef(fit_4)[1:2])),
    slope = c(coef(fit_4)[3], sum(coef(fit_4)[3:4])),
    color = c("gray", "black")) +
  scale_color_manual(values = c("gray", "black")) +
  labs(x = "Mother IQ score", y = "Child test score") + 
  jtools::theme_apa()

# kid_score = -11 + 51*mom_hs + 1*mom_iq - 0.5*mom_hs*mom_iq + error

# kid_score(mom_hs=0) = -11 + 51*0 + 1*mom_iq - 0.5*0*mom_iq
#                     = -11 + 1*mom_iq

# kid_score(mom_hs=1) = -11 + 51*1 + 1*mom_iq - 0.5*1*mom_iq
#                     = 40 + 1*mom_iq - 0.5*mom_iq
#                     = 40 + 0.5*mom_iq



# The coefficient on the interaction term represents the difference in the
# slope for mom_iq, comparing children with mothers who did and did not complete 
# high school


# O coeficiente no termo de interação representa a diferença na inclinação para 
# mom_iq, comparando crianças com mães que completaram e não concluíram o ensino
# médio


