setwd("~/Desktop/School/Math 425 Applied Stats Models/MATH425-Assignment-1")
gpa_data = read.csv("data/gpa-act.csv", header=T)

# x = ACT SCORE
x = gpa_data$act
# y = GPA
y = gpa_data$gpa

# Set graph to 1x1
par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# (a) Obtain the least squares estimates of B0 and B1, and state the estimated
#     regression function.
# B0 = 2.114, B1 = 0.0388 
# Estimated Regression Function: GPA = 2.114 + (0.0388*ACT)


# Fit the GPA & ACT data to Simple Linear Regression mode, lm: y = B0 + B1x 
gpa_model = lm(y~x, data = gpa_data)

# Utilize the lm.summary() function to examine properties of the fitted model
summary(gpa_model)
# Estimated coefficient, Standard error, T-statistic,Two-sided P-Value

B0 = 2.114    # Y-Intercept
B1 = 0.0388   # Slope
# GPA = 2.114 + (0.0388*ACT)

# ------------------------------------------------------------------------------
# (b) Plot the estimated regression function and the data. Does the estimated 
#     regression function appear to fit the data well?
# The regression function does appear to fit the data well, it is well centered
# at what appears to be minimal variance

# X-Y Scatter Plot of ACT-GPA
plot(x, y)

# Linear Regression Line: y = 2.114 + 0.0388x
abline(a = B0, b = B1)

# ------------------------------------------------------------------------------
# (c) Obtain a point estimate of the mean freshman GPA for students with ACT 
#     test score X = 30.
# Point Estimate = 3.278
# (Upper, Fit, Lower) : (4.525, 3.278, 2.032)


# When using lm(), must always use the same variable names EVERY TIME you refer
# to data. 
# Ex. Once define x = gpa_data$act, ONLY use x never use gpa_data$act 

# Using the lm.predict() function: model, new_data_frame, prediction
predict(gpa_model, data.frame(x=30), interval = "prediction")
# Point Estimate is the "fit" value

# ------------------------------------------------------------------------------
# (d) What is the point estimate of the change in the mean response when the 
#     entrance test score increases by one point?
# The Point Estimate of the Change in the Mean Response when
# the ACT increases = B1 * Amt Increased = B1 * 1 = 0.0388