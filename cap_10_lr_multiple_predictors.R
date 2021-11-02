# libraries
library(rstanarm)
library(ggplot2)

# data set
data("kidiq")




# Add a binary predictor -------------------------------------------------------
fit_1 <- stan_glm(kid_score ~ mom_hs, data=kidiq)
fit_1    # kid_score = 78 + 12*mom_hs + error

ggplot(kidiq, aes(factor(mom_hs), kid_score)) +
  geom_jitter(width = .08) +
  geom_abline(intercept = coef(fit_1)[1], slope = coef(fit_1)[2]) +
  labs(x = "Mother completed high scool", y = "Child test score") + 
  jtools::theme_apa()





# Add a continuous predictor ----------------------------------------------------
fit_2 <- stan_glm(kid_score ~ mom_iq, data=kidiq)
fit_2    # kid_score = 26 + 0.6∗mom_iq + error

ggplot(kidiq, aes(mom_iq, kid_score)) +
  geom_point() +
  geom_abline(intercept = coef(fit_2)[1], slope = coef(fit_2)[2]) +
  labs(x = "Mother IQ score", y = "Child test score") + 
  jtools::theme_apa()





# Including booth predictors ---------------------------------------------------
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






# Indicator variables ----------------------------------------------------------

# data set
URL = "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv"
earnings <- read.csv(URL)

fit_1 <- stan_glm(weight ~ height, data=earnings)
fit_1    # weight = -173.7 + 5*height + error

# predict weight of a person with 66 pounds
coefs_1 <- coef(fit_1)
predicted_1 <- coefs_1[1] + coefs_1[2]*66
predicted_1

# or we can simply use `posterior_predict`
new <- data.frame(height=66)
pred <- posterior_predict(fit_1, newdata = new)

cat("Predicted weight for a 66-inch-tall person is", 
    round(mean(pred)),
    "pounds with a sd of", 
    round(sd(pred)), 
    "\n")
# Predicted weight for a 66-inch-tall person is 153 pounds with a sd of 29






# Centering a predictor --------------------------------------------------------
# to improve interpretation

earnings$c_height <- earnings$height - 66
fit_2 <- stan_glm(weight ~ c_height, data=earnings)
fit_2 # (intercept)=153.4, sigma=28.9


# Including a binary variable in a regression ----------------------------------
# including an indicator variable for sex

fit_3 <- stan_glm(weight ~ c_height + male, data=earnings)
fit_3    # male=11.8
# comparing a man to woman of the same height, the man will be 
# predicted to be 12 pounds heavier.


# predicted weight for a 70-inch-tall woman
coefs_3 <- coef(fit_3)
predicted <- coefs_3[1] + coefs_3[2]*3.9 + coefs_3[3]*0

# or using `posterior_predict`
new <- data.frame(c_height=3.9, male=0)
pred <- posterior_predict(fit_3, newdata=new)
cat("Predicted weight for a 70-inch-tall woman is", 
    round(mean(pred)),
    "pounds with a sd of", 
    round(sd(pred)), 
    "\n")
# Predicted weight for a 70-inch-tall woman is 164 pounds with a sd of 29







# Using indicator variables for multiple levels of a categorical predictor -----
table(earnings$ethnicity)

fit_4 <- stan_glm(weight ~ c_height + male + factor(ethnicity), data=earnings)
fit_4
# factor(ethnicity)Hispanic  -6.2
# factor(ethnicity)Other    -12.4
# factor(ethnicity)White     -5.2







# Changing the baseline factor level -------------------------------------------
earnings$eth <- factor(earnings$ethnicity,
                       levels=c("White", "Black", "Hispanic", "Other"))
fit_5 <- stan_glm(weight ~ c_height + male + eth, data=earnings)
fit_5
# ethBlack      5.2
# ethHispanic  -1.0
# ethOther     -7.2



# Dummy variable ---------------------------------------------------------------
earnings$eth_white <- ifelse(earnings$ethnicity=="White", 1, 0)
earnings$eth_black <- ifelse(earnings$ethnicity=="Black", 1, 0)
earnings$eth_hispanic <- ifelse(earnings$ethnicity=="Hispanic", 1, 0)
earnings$eth_other <- ifelse(earnings$ethnicity=="Other", 1, 0)

fit_6 <- stan_glm(weight ~ height + male + eth_black + eth_hispanic + eth_other,
                  data=earnings)
fit_6
# eth_black       5.1
# eth_hispanic   -0.9
# eth_other      -7.0











# Uncertainty in predicting congressional elections ----------------------------

# data set
URL <- "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Congress/data/congress.csv"
congress <- read.csv(URL)

data88 <- data.frame(vote=congress$v88_adj, 
                     past_vote=congress$v86_adj, 
                     inc=congress$inc88)
fit88 <- stan_glm(vote ~ past_vote + inc, data=data88)
fit88




# Simulation for inferences and predictions of new data points -----------------
sims88 <- as.matrix(fit88)


data90 <- data.frame(past_vote=congress$v88_adj, inc=congress$inc90)
pred90 <- posterior_predict(fit88, newdata=data90)





# Predictive simulation for a nonlinear function of new data -------------------

dems_pred <- rowSums(pred90 > 0.5)
n_sims <- 4000
dems_pred <- rep(NA, n_sims)
for (s in 1:n_sims){
  dems_pred[s] <- sum(pred90[s, ] > 0.5)
}

summary(dems_pred)
sd(dems_pred)


# outcomes           ~ predictors           -> ok
# dependent variable ~ independent variable -> not ok









