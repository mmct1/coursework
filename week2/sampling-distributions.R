library(tidyverse)

####################################################################################
# IST Chapter 4, Exercise 4.1
#
# Table 4.4 presents the probabilities of the random variable Y:
#
#   Value | Probability
#   ------|------------
#     0   |    1p
#     1   |    2p
#     2   |    3p
#     3   |    4p
#     4   |    5p
#     5   |    6p
#
# These probabilities are a function of the number p, the probability of
# the value "0". Answer the following questions:

# 1. What is the value of p? 
# 1p+2p+3p+4p+5p+6p = 1     
# => 21p = 1
# => p = 1/21

# 2. P(Y < 3) = ?
# P(Y=0)+P(Y=1)+P(Y=2) = 1/21 + 2/21 + 3/21 = 6/21
#  P(Y < 3) = 6/21

# 3. P(Y = odd) = ?
# P(Y=1)+P(Y=3)+P(Y=5) = 2/21 + 4/21 + 6/21 = 12/21
#  P(Y = odd) = 12/21

# 4. P(1 <= Y < 4) = ?
# P(Y=1)+P(Y=2)+P(Y=3) = 2/21 + 3/21 + 4/31 = 9/21
# P(1 <= Y < 4) = 9/21

# 5. P(|Y - 3| < 1.5) = ?
# P(Y=2)+P(Y=3)+P(Y=4) = 3/21 + 4/21 + 5/21 = 12/21
#  P(|Y - 3| < 1.5) = 12/21

# 6. E(Y) = ?
# 0(1/21) + 1(2/21) + 2(3/21) + 3(4/21) + 4(5/21) + 5(6/21) = 70/21 = 3.3333
# E(Y) = 3.3333

# 7. Var(Y) = ?
E_y <- 3.3333
y_vals <- c(0,1,2,3,4,5)
p_vals <- c(1/21,2/21,3/21,4/21,5/21,6/21)
Var_y <- sum((y_vals-E_y)^2*p_vals)
Var_y
# Var(Y)=2.2222

# 8. What is the standard deviation of Y?
sqrt(Var_y)
# 1.4907

####################################################################################
# IST Chapter 4, Exercise 4.2
#
# One invests $2 to participate in a game of chance. In this game a coin
# is tossed three times. If all tosses end up "Head" then the player wins
# $10. Otherwise, the player loses the investment.

# 1. What is the probability of winning the game?
# HHH, HHT, HTH, HTT, TTT, TTH, THT, THH
# Sample space has size 8, where only one option is considered a win => 
# The probability of winning the game is 1/8

# 2. What is the probability of losing the game?
# 7/8

# 3. What is the expected gain for the player that plays this game?
#    (Notice that the expectation can obtain a negative value.)
# 7/8 probability of losing $2, 1/8 prob of winning $10 (-$2 for playing=8)
# Expected value/expected gain => summming the outcome*their probability
# (-2)(7/8) + (8)(1/8) = -1.75 + 1 = -0.75
# -$0.75 (or, expectation is losing 75 cents)


####################################################################################
# IST Chapter 6, Exercise 6.1
#
# Consider the problem of establishing regulations concerning the maximum
# number of people who can occupy a lift. In particular, we would like to
# assess the probability of exceeding maximal weight when 8 people are
# allowed to use the lift simultaneously and compare that to the probability
# of allowing 9 people into the lift.
#
# Assume that the total weight of 8 people chosen at random follows a
# Normal distribution with a mean of 560kg and a standard deviation of 57kg.
# Assume that the total weight of 9 people chosen at random follows a
# Normal distribution with a mean of 630kg and a standard deviation of 61kg.
#
# 1. What is the probability that the total weight of 8 people exceeds 650kg?
# X~N(560,57^2)
# P(X>650) = 1-P(X<=650)
1-pnorm(650,560,57)
# 0.05717406

# 2. What is the probability that the total weight of 9 people exceeds 650kg?
# X~N(630,61^2)
# P(X>650) = 1-P(X<=650)
1-pnorm(650,630,61)
# 0.3715054

# 3. What is the central region that contains 80% of distribution of the
#    total weight of 8 people?
# 80% = 90% - 10%
qnorm(.90,560,57) #upper
qnorm(.10,560,57) #lower
# the range 486.95 to 633.05

# 4. What is the central region that contains 80% of distribution of the
#    total weight of 9 people?
qnorm(.90,630,61)
qnorm(.10,630,61)
# the range 551.83 to 708.17

# Hint: use pnorm() and qnorm().


####################################################################################
# IST Chapter 7, Exercise 7.1
#
# The file "pop2.csv" contains information associated to the blood pressure
# of an imaginary population of size 100,000:
# http://pluto.huji.ac.il/~msby/StatThink/Datasets/pop2.csv
#
# Variables: id, sex, age, bmi, systolic, diastolic, group
#
# Our goal is to investigate the sampling distribution of the sample average
# of the variable "bmi". We assume a sample of size n = 150.

pop2 <- read_csv("http://pluto.huji.ac.il/~msby/StatThink/Datasets/pop2.csv")

# 1. Compute the population average of the variable "bmi".
pop_mean <- mean(pop2$bmi) #24.98446

# 2. Compute the population standard deviation of the variable "bmi".
# sqrt((1/nrow(pop2)) * (sum((pop2$bmi-pop_mean)^2))) veryyyy dramatic approach, forgot about sd, but gets the same thing
standard_dev <- sd(pop2$bmi) #4.188511

# 3. Compute the expectation of the sampling distribution for the sample
#    average of the variable.
N <- 150
X_bar <- rep(0,10^5)
for (i in 1:10^5) {
    X_samp <- sample(pop2$bmi,N)
    X_bar[i] <- mean(X_samp)
}
sample_mean <- mean(X_bar)
sample_mean
# gives values close to population mean

# 4. Compute the standard deviation of the sampling distribution for the
#    sample average of the variable.
standard_error <- sd(X_bar)

# 5. Identify, using simulations, the central region that contains 80% of
#    the sampling distribution of the sample average.
quantile(X_bar,c(.10,.90))

# 6. Identify, using the Central Limit Theorem, an approximation of the
#    central region that contains 80% of the sampling distribution of the
#    sample average.
# qnorm(.90,sample_mean,standard_error) #upper
# qnorm(.10,sample_mean,standard_error) #lower
qnorm(c(.10,.90),sample_mean,standard_error) #lower

# Hint: for (5), use replicate() to draw many samples of size 150,
# compute the mean of bmi for each, then use quantile().
# For (6), use qnorm() with the expectation and sd from (3) and (4).
