
# Simple regression --------------------------------------------------

# dataset
URL = "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat"
hibbs <- read.table(URL, header = TRUE)


# Scatterplot
plot(x = hibbs$growth, 
     y = hibbs$vote, 
     xlab = "Average recent growth in personal income",
     ylab = "Incumbent party's vote share")


# estimate y = a + bx + error
M1 <- rstanarm::stan_glm(vote ~ growth, data = hibbs)


# add fitted line to graph
abline(coef(M1), col = "gray")


# display fitted model
print(M1)

# 3.9 percentual error
