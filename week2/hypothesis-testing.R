library(tidyverse)

####################################################################################
# IST Chapter 9, Exercise 9.1
magnets <- read_csv("http://pluto.huji.ac.il/~msby/StatThink/Datasets/magnets.csv")

# 1. What is the sample average of the change in score between the
#    patient's rating before the application of the device and the
#    rating after the application?
mean(magnets$change) #3.5

# 2. Is the variable "active" a factor or a numeric variable?
# Factor - there are only 2 unique values (1 and 2)
# But is.factor(magnets$active) returns FALSE?

# 3. Compute the average value of the variable "change" for the patients that
#    received an active magnet and average value for those that received an
#    inactive placebo. (Hint: Notice that the first 29 patients received an
#    active magnet and the last 21 patients received an inactive placebo. The
#    subsequence of the first 29 values can be obtained via "change[1:29]" and
#    the last 21 values via "change[30:50]".)
mean(magnets$change[1:29]) #5.241379
mean(magnets$change[30:50]) #1.095238

# think a nicer way to do it would be
magnets %>%
    group_by(active) %>%
    summarize(avg=mean(change))


# 4. Compute the sample standard deviation of the variable "change" for the
#    patients that received an active magnet and the sample standard deviation
#    for those that received an inactive placebo.
sd(magnets$change[1:29]) #3.236568
sd(magnets$change[30:50]) #1.578124

# or
magnets %>%
    group_by(active) %>%
    summarize(stan_dev=sd(change))

# 5. Produce a boxplot of the variable "change" for the patients that received
#    an active magnet and for patients that received an inactive placebo. What
#    is the number of outliers in each subsequence?
magnets %>%
    ggplot(aes(x=active,y=change, fill=active)) +
    geom_boxplot() +
    scale_x_discrete(labels=c("Active Magnet","Placebo")) +
    labs(x="Treatment Type",y="Change",title="Change in Pain Rating (Before/After Treatment) by Treatment Type")+
    theme(legend.position="none")
# looks like no outliers in active, and 3 in inactive, but want to double check
# number of outliers function
outliers <- function(col) {
    q1 <- quantile(col, 0.25)
    q3 <- quantile(col,0.75)
    iqr <- IQR(col)

    sum(col < (q1-1.5*iqr) | col > (q3+1.5*iqr))
}

outliers(magnets$change[1:29]) #0
outliers(magnets$change[30:50]) #4, not 3

####################################################################################
# IST Chapter 10, Exercise 10.1
#
# In Subsection 10.3.2 we compare the average against the midrange as estimators
# of the expectation of the measurement. The goal of this exercise is to repeat
# the analysis, but this time compare the average to the median as estimators of
# the expectation in symmetric distributions.
#
# 1. Simulate the sampling distribution of average and the median of a sample
#    of size n = 100 from the Normal(3, 2) distribution. Compute the expectation
#    and the variance of the sample average and of the sample median. Which of
#    the two estimators has a smaller mean square error?
n <- 100
mu <- 3
sig <- sqrt(2)

x_bar <-  replicate(100000, mean(rnorm(n,mu,sig)))
mean(x_bar) 
var(x_bar) #generally outputs a smaller var than x_med
mean((mu-x_bar)^2)

x_med <- replicate(100000, median(rnorm(n,mu,sig)))
mean(x_med)
var(x_med)
mean((mu-x_med)^2)
# variance of x_bar and MSE of x_bar are smaller than those of x_med

# 2. Simulate the sampling distribution of average and the median of a sample
#    of size n = 100 from the Uniform(0.5, 5.5) distribution. Compute the
#    expectation and the variance of the sample average and of the sample
#    median. Which of the two estimators has a smaller mean square error?
n <- 100
a <- 0.5
b <- 5.5
expectation <- (a+b)/2
var <- ((b-a)^2)/12

x_bar <- replicate(100000, mean(runif(n,a,b)))
mean(x_bar)
var(x_bar)
mean((mu-x_bar)^2)

x_med <- replicate(100000, median(runif(n,a,b)))
mean(x_med)
var(x_med)
mean((mu-x_med)^2)
# variance of x_bar and MSE of x_bar are smaller than those of x_med

####################################################################################
# IST Chapter 10, Exercise 10.2
#
# The goal in this exercise is to assess estimation of a proportion in a
# population on the basis of the proportion in the sample.
#
# The file "pop2.csv" was introduced in Exercise 7.1 of Chapter 7. This file
# contains information associated to the blood pressure of an imaginary
# population of size 100,000. One of the variables in the file is a factor by
# the name "group" that identifies levels of blood pressure. The levels of this
# variable are "HIGH", "LOW", and "NORMAL".
#
# The file "ex2.csv" contains a sample of size n = 150 taken from the given
# population. The file "ex2.csv" corresponds to the observed sample and the file
# "pop2.csv" corresponds to the unobserved population.

pop2 <- read_csv("http://pluto.huji.ac.il/~msby/StatThink/Datasets/pop2.csv")
ex2 <- read_csv("http://pluto.huji.ac.il/~msby/StatThink/Datasets/ex2.csv")

# 1. Compute the proportion in the sample of those with a high level of blood
#    pressure.
# 2. Compute the proportion in the population of those with a high level of
#    blood pressure.
# 3. Simulate the sampling distribution of the sample proportion and compute
#    its expectation.
# 4. Compute the variance of the sample proportion.
# 5. It is proposed in Section 10.5 that the variance of the sample proportion
#    is Var(P_hat) = p(1 - p)/n, where p is the probability of the event (having
#    a high blood pressure in our case) and n is the sample size (n = 150 in our
#    case). Examine this proposal in the current setting.

####################################################################################
# ISRS Exercise 2.2 - Heart transplants, Part II
#
# Exercise 1.50 introduces the Stanford Heart Transplant Study. Of the 34
# patients in the control group, 4 were alive at the end of the study. Of the 69
# patients in the treatment group, 24 were alive.
#
# Contingency table:
#                                    Group
#                       --------------------------
#                        Control  Treatment  Total
#          ---------------------------------------
#                Alive      4        24       28
#          ---------------------------------------
#  Outcome       Dead       30       45       75
#          ---------------------------------------
#                Total      34       69      103
#          ---------------------------------------
#
# (a) What proportion of patients in the treatment group and what proportion
#     of patients in the control group died?
# (b) One approach for investigating whether or not the treatment is effective
#     is to use a randomization technique.
#     i. What are the claims being tested? Use the same null and alternative
#          hypothesis notation used in the section.
#     ii. The paragraph below describes the set up for such approach, if we were
#     to do it without using statistical software. Fill in the blanks with a
#     number or phrase, whichever is appropriate. 
#          We write alive on _______ cards representing patients who were
#          alive at the end of the study, and dead on ______ cards representing
#          patients who were not. Then, we shuffle these cards and split them
#          into two groups: one group of size _______ representing treatment, and
#          another group of size _________ representing control. We calculate the
#          difference between the proportion of dead cards in the treatment and
#          control groups (treatment - control) and record this value. We repeat
#          this many times to build a distribution centered at ________. Lastly, we
#          calculate the fraction of simulations where the simulated differences
#          in proportions are _________. If this fraction is low, we conclude that it is
#          unlikely to have observed such an outcome by chance and that the null
#          hypothesis should be rejected in favor of the alternative.
#     iii. What do the simulation results suggest about the effectiveness of
#          the transplant program? (See textbook for figure.)

####################################################################################
# ISRS Exercise 2.6 
# An experiment conducted by the MythBusters, a science entertainment TV program
# on the Discovery Channel, tested if a person can be subconsciously influenced
# into yawning if another person near them yawns. 50 people were randomly
# assigned to two groups: 34 to a group where a person near them yawned
# (treatment) and 16 to a group where there wasn't a person yawning near them
# (control). The following table shows the results of this experiment.
#
# Contingency table:
#                         --------------------------
#                         Control  Treatment  Total
#          ---------------------------------------
#               Yawn         10       4        14
#  Result       Not Yawn     24       12       36
#          ---------------------------------------
#                Total       34       16       50
#          ---------------------------------------
#
# A simulation was conducted to understand the distribution of the test
# statistic under the assumption of independence: having someone yawn near
# another person has no influence on if the other person will yawn. In order to
# conduct the simulation, a researcher wrote yawn on 14 index cards and not yawn
# on 36 index cards to indicate whether or not a person yawned. Then he shuffled
# the cards and dealt them into two groups of size 34 and 16 for treatment and
# control, respectively. He counted how many participants in each simulated
# group yawned in an apparent response to a nearby yawning person, and
# calculated the difference between the simulated proportions of yawning as
# ˆptrtmt,sim − pˆctrl,sim. This simulation was repeated 10,000 times using
# software to obtain 10,000 differences that are due to chance alone. The
# histogram shows the distribution of the simulated differences.
#
# (a) What are the hypotheses?
# (b) Calculate the observed difference between the yawning rates under the
#     two scenarios.
# (c) Estimate the p-value using the figure and determine the conclusion of
#     the hypothesis test.

####################################################################################
# IST Exercise 9.2 
# In Chapter 13 we will present a statistical test for testing
# if there is a difference between the patients that received the active magnets
# and the patients that received the inactive placebo in terms of the expected
# value of the variable that measures the change. The test statist for this
# problem is taken to be
#  T = (X_bar_1 - X_bar_2) / sqrt(S_1^2/29 + S_2^2/21)
#
# where X_bar_1 and X_bar_2 are the sample averages for the 29 patients that 
# receive active magnets and for the 21 patients that receive inactive placebo, 
# respectively. The quantities S_1^2 and S_^2 are the sample variances for each
# of the two samples. Our goal is to investigate the sampling distribution 
# of this statistic in a case where both expectations are equal to each other 
# and to compare this distribution to the observed value of the statistic.
#
# 1. Assume that the expectation of the measurement is equal to 3.5, regardless
#    of what the type of treatment that the patient received. We take the
#    standard deviation of the measurement for patients the receives an active
#    magnet to be equal to 3 and for those that received the inactive placebo we
#    take it to be equal to 1.5. Assume that the distribution of the
#    measurements is Normal and there are 29 patients in the first group and 21
#    in the second. Find the interval that contains 95% of the sampling
#    distribution of the statistic.
# 2. Does the observed value of T (computed from the "magnets" data) fall
#    inside or outside the interval computed in 1?
