##################################################################################
# ISRS Exercise 5.20
# Part III. Exercise 5.13 introduces data on shoulder girth and
# height of a group of individuals. The mean shoulder girth is 
# 108.20 cm with a standard deviation of 10.37 cm. The mean height 
# is 171.14 cm with a standard deviation of 9.41 cm. The correlation
# between height and shoulder girth is 0.67
# See textbook for image

# a. Write the equation of the regression line for predicting height.
# one point (x_bar, y_bar) is (108.20, 171.14)
# b1=(sy/sx)R -> b1=(9.41/10.37)(0.67)=0.608
# 171.14 = b0+0.608(108.20) -> b0 = 105.35
# EQUATION: y = 105.35 + 0.608x

# b. Intepret the slope and the intercept in this context.
# intercept: represents the anticipated height when shoulder girth is 0
# (not realistic, which makes sense as to why the x-axis only begins around 85 cm)
# slope: for each 1cm increase shoulder girth, expected height increases by .608cm

# c. Calculate R^2 of the regression line for predicting height from 
#    shoulder girth, and interpret in the context of the application. 
# R^2 = (.67)^2 = 0.4489
# so about 45% of the variability in height (response) can be explained 
# by a linear relationship with shoulder girth (explanatory variable)

# d. A randomly selected student from your class has a shoulder girth 
#    of 100 cm. Predict the height of this student using the model.
# y = 105.35 + 0.608(100)
# y = 166.15
# predicted height is 166.15 cm

# e. The student from part (d) is 160 cm tall. Calculate the residual, 
#    and explain what this residual means.
# residual = 160 - 166.15 = -6.15
# it means that the student was 6.15 cm shorter than predicted

# f. A one year old has a shoulder girth of 56 cm. Would it be 
#    appropriate to use this linear model to predict the height of this child?
# no, it wouldn't be appropriate, because we don't necessarily know how data will act 
# outside of the bounds of the window we originally observed. (extrapolation)

##################################################################################
# ISRS Exercise 5.29
# The scatterplot and least squares summary below show the relationship
# between weight measured in kilograms and height measured in centimeters
# of 507 physically active individuals
# See textbook for scatterplot.

# Coefficients:
#               Estimate  Std. Error  t value  Pr(>|t|)
# (Intercept)  -105.0113      7.5394   -13.93    0.0000
# height          1.0176      0.0440    23.13    0.0000

# a. Describe the relationship between height and weight.
# positive linear relationship between height (x) and weight (y)
# so as height increases weight generally increases

# b. Write the equation of the regression line. Interpret the slope
#    and intercept in context.
# col1 gives b0, b1
# y = 1.0176x - 105.0113
# slope: for each 1cm increase in height, weight is predicted to increase by 1.0176kg
# intercept: when height is 0, weight is expected to be -105.0113 - similar to last 
# problem, not realistic or relevant to the window of data we're looking at

# c.Do the data provide strong evidence that an increase in height 
#   is associated with an increase in weight? State the null and 
#   alternative hypotheses, report the p-value, and state your conclusion.
# H_0: an increase in height is not associated with an increase in weight
# H_A: an increase in height is associated with an increase in weight
# second row last column gives the p-value = 0.0000, so we can reject the null hypothesis 
# (strong evidence that height increase is associated with weight increase)

# d. The correlation coefficient for height and weight is 0.72. 
#    Calculate R^2 and interpret it in context.
# R^2 = (0.72)^2 = 0.5184
# so about 52% of the variability in weight (response) can be explained 
# by a linear relationship with height (explanatory variable)