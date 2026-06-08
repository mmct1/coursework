#################################################################################
# Reproduce this table in ISRS 5.29 using the original dataset in body.dat.txt
# Coefficients:
#               Estimate  Std. Error  t value  Pr(>|t|)
# (Intercept)  -105.0113      7.5394   -13.93    0.0000
# height          1.0176      0.0440    23.13    0.0000
body <- read.table("body.dat.txt", header = TRUE)

# key to variables @ bottom of page: https://jse.amstat.org/v11n2/datasets.heinz.html
# col23 - weight
# col24 - height

body2 <- body %>%
    select(23,24) %>%
    rename("weight"="X65.6", "height"="X174.0")
    
body2 %>%
    ggplot(aes(x=height,y=weight)) + 
        geom_point()
        #sanity check, looks like the scatterplot from 5.29

model <- lm(weight~height, body2)
model
# output aligns closely with Estimate col in 5.29

coef_table <- summary(model)$coefficients %>% as.data.frame()

coef_table <- coef_table %>%
    mutate(across(.cols = c("Estimate","Std. Error"), .fns = (\(x)round(x,4))))
coef_table <- coef_table %>%
    mutate(across(.cols = "t value", .fns = (\(x)round(x,2))))
coef_table <- coef_table %>%
    mutate(across(.cols = "Pr(>|t|)", .fns = (\(x)formatC(x,digits=4,format="f"))))
coef_table
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept) -105.0691     7.5437  -13.93   0.0000
# height         1.0180     0.0440   23.13   0.0000
# some of the vals in cols 1 and 2 dont match exactly? but pretty close
###################################################################################
# ISRS Exercise 6.1
#  The Child Health and Development Studies investigate a range of
# topics. One study considered all pregnancies between 1960 and 1967 among women in the Kaiser
# Foundation Health Plan in the San Francisco East Bay area. Here, we study the relationship
# between smoking and weight of the baby. The variable smoke is coded 1 if the mother is a
# smoker, and 0 if not. The summary table below shows the results of a linear regression model for
# predicting the average birth weight of babies, measured in ounces, based on the smoking status of
# the mother.
# Coefficients:
#               Estimate  Std. Error  t value  Pr(>|t|)
# (Intercept)    123.05        0.65   189.60    0.0000
# smoke           -8.94        1.03    -8.65    0.0000

# The variability within the smokers and non-smokers are about equal and the distributions are
# symmetric. With these conditions satisfied, it is reasonable to apply the model. (Note that we
# don’t need to check linearity since the predictor has only two levels.)
babyweights <- read.table("babyweights.txt", header = TRUE)

# REPRODUCING RESULTS FROM THE BOOK
model1 <- lm(bwt~smoke,babyweights)
summary(model1)$coefficients

# a. Write the equation of the regression line.
# y = -8.94x + 123.05

# b. Interpret the slope in this context, and calculate the predicted birth weight of babies born to
# smoker and non-smoker mothers.
# slope: if a mother is a smoker, a baby is anticipated to have a birth weight 8.94 less ounces than
# those born to non-smokers

# c. Is there a statistically significant relationship between the average birth weight and smoking?
# yes, the p-value is reported as 0.0000, so less than the typical alpha of 0.05

###################################################################################
# ISRS Exercise 6.2
# Exercise 6.1 introduces a data set on birth weight of babies.
#Another variable we consider is parity, which is 0 if the child is the first born, and 1 otherwise.
#The summary table below shows the results of a linear regression model for predicting the average
# birth weight of babies, measured in ounces, from parity
# Coefficients:
#               Estimate  Std. Error  t value  Pr(>|t|)
# (Intercept)    120.07        0.60   199.94    0.0000
# parity          -1.93        1.19    -1.62    0.1052

# REPRODUCING RESULTS FROM THE BOOK
model2 <- lm(bwt~parity,babyweights)
summary(model2)$coefficients

# a. Write the equation of the regression line.
# y = -1.93x + 120.07

# b. Interpret the slope in this context, and calculate the predicted birth weight of first borns and
#    others.
# slope: if a child is not firstborn, their birthweight is expected to be 1.93 less ounces
# predicted birth weights:
#       first borns = 120.07 ounces
#       others = 118.14

# c. Is there a statistically significant relationship between the average birth weight and parity?
# no, p-value is 0.1052 which is larger than the standard alpha of 0.05

###################################################################################
# ISRS Exercise 6.3
# We considered the variables smoke and parity, one at a time, in
# modeling birth weights of babies in Exercises 6.1 and 6.2. A more realistic approach to modeling
# infant weights is to consider all possibly related variables at once. Other variables of interest
# include length of pregnancy in days (gestation), mother’s age in years (age), mother’s height in
# inches (height), and mother’s pregnancy weight in pounds (weight). Below are three observations
# from this data set.

# Data set observations (n = 1,236):
#        bwt  gestation  parity  age  height  weight  smoke
# 1      120        284       0   27      62     100      0
# 2      113        282       0   33      64     135      0
# ...
# 1236   117        297       0   38      65     129      0

# Coefficients:
#               Estimate  Std. Error  t value  Pr(>|t|)
# (Intercept)    -80.41       14.35    -5.60    0.0000
# gestation        0.44        0.03    15.26    0.0000
# parity          -3.33        1.13    -2.95    0.0033
# age             -0.01        0.09    -0.10    0.9170
# height           1.15        0.21     5.63    0.0000
# weight           0.05        0.03     1.99    0.0471
# smoke           -8.40        0.95    -8.81    0.0000

# REPRODUCING RESULTS FROM THE BOOK
model3 <- lm(bwt~.,babyweights)
round(summary(model3)$coefficients,2)

# a. Write the equation of the regression line that includes all variables:
# y = -80.41 + 0.44(gestation) - 3.33(parity) - 0.01(age) + 1.15(height) + 0.05(weight) - 8.40(smoke)

# b. Interpret the slopes of gestation and age in this context:
# gestation slope: for every additional day in length of pregnancy, the birth weight is
# predicted to increase by 0.44 ounces
# age slope: for each additional year in mothers age, the birth weight is predicted to 
# decrease by 0.01 ounces

# c. The coefficient for parity is different than in the linear model shown in Exercise 6.2. Why
#    might there be a difference?
# there might be a difference because collinearity can lead p-values of a variable to change

# d. Calculate the residual for the first observation in the dataset.
#         bwt  gestation  parity  age  height  weight  smoke
#  1      120        284       0   27      62     100      0
# observed birthweight = 120
# predicted birthweight = -80.41 + 0.44(284) - 0.01(27) + 1.15(62) + 0.05(100) = 120.58
# residual = observed - predicted
# residual = 120 - 120.58 = -0.58

# e. The variance of the residuals is 249.28, and the variance of the birth weights of all babies
#    in the data set is 332.57. Calculate the R^2 and the adjusted R^2. Note that there are 1,236
#    bservations in the data set.
# R^2 = 1 - (variability in residuals/variability in outcome)
# R^2 = 1 - (249.28/332.57) = 0.2504
# adjusted R^2 = R^2 * (n-1)/(n-k-1) where n is the num cases used to fit the model and k is num predictors
# adjusted R^2 = 0.2504 * (1236-1)/(1236-6-1) = 0.2516