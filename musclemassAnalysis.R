#******************************************************************************
# WILL HAVE TO CHANGE setwd TO YOUR OWN DIRECTORY
setwd("~/Desktop/School/Math 425 Applied Stats Models/MATH425-Assignment-1")
#******************************************************************************

muscle_data = read.csv("data/musclemass-age.csv", header=T)

# X = AGE
x = muscle_data$age 
# Y = Muscle Mass
y = muscle_data$musclemass 

# ------------------------------------------------------------------------------
# (a) Obtain the estimated regression function. Plot the estimated regression 
#function and the data. Does a linear regression function appear to give a good 
# fit here? Does your plot?
# The regression function does appear to fit the data well, it is well centered
# at what appears to be minimal variance, but as the x increases so does the 
# data's variance, making it difficult at the top-range of the data

musclemass_model = lm(y~x, data = muscle_data) # SLR model

summary(musclemass_model)
B0 = 156.346  # Y-Intercept
B1 = -1.190   # Slope

plot(x, y) # Scatter plot
abline(B0, B1) # SLR line y = -1.190x + 156.346

# ------------------------------------------------------------------------------
# (b.1) Obtain a point estimate of the difference in the mean muscle mass for 
#       women differing in age by one year
# The Point Estimate of the difference in the Mean Muscle Mass when
# the Age increases = B1 * Amt increased = B1 * 1 = -1.190


# ------------------------------------------------------------------------------
# (b.2) Obtain a point estimate of the mean muscle mass for women aged 
#       X = 60 years
# Point Estimate = 84.946
# C.I (Lower, Upper) : (68.450, 101.443)

# predict(model, new_data_frame(dependentVariable=x), predict)
predict(musclemass_model, data.frame(x=60), interval = "prediction")


# ------------------------------------------------------------------------------
# (b.3) Obtain the value of the residual for the eighth case
# Residuals[8] = 4.443


# modelName$residuals[index]
musclemass_model$residuals[8]


# ------------------------------------------------------------------------------
# (b.4) Obtain a point estimate of 𝜎2.


# Need to use ANOVA(model)
anova(musclemass_model)



