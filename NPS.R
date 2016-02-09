#NPS
# This is a R code to predict sales performance of 'New Products' based on the Bass Diffusion Model

# The Bass model of diffusion is used to estimate the speed new technologies will be adopted and works on 4 major product parameters
# Market for the product (People who will buy the product) - M
# The coefficient of innovation (or coefficient of external influence) - P
# The coefficient of imitation (or coefficient of internal influence) - Q

library(minpack.lm)

#Forecasting Package
require(propagate)

#Sales of parent/similar products - Set your own here
PSales <- c(0, 2130, 3212, 6320, 8236, 7213, 4213)

#Period of sales - Set your own here
Time <- length(Psales)

#Change in Sales
Tdelta <- (1:100)/length(PSales)

#Bass Model Formula
BassModel <- Sales ~ (M * ( ( (P+Q)^2 / P) * exp(-(P+Q) * Time) ) / (1+ (Q/P) * exp(-(P+Q) * Time))^2)

# Avg. P and Q values used - Use your own if known
#Use best assessment values for M

BassFor.nlsLM <- nlsLM(BassFor , start = list( M = 10000, P = 0.03, Q = 0.38))
(BassFor.nlsLM <- update(BassFor.nlSLM) )
summary(BassFor.nlsLM)

#Prediction Model - Use your assessment values here
EstSales <- seq(1, 10000, by = 2000)

#New data frame with colnames as 'time' to run 'predictNLS' function
EstSales <- data.frame(time = EstSales)

#Confidence set at default values of 95%, change alpha to your own levels here
BassFor_predicted <- predictNLS(BassFor.nlSLM, newdata = EstSales, alpha = 0.05)

#Co-efficients for the model
BCef <- coef(Bass.nlsLM)
M <- BCef[1]
P <- BCef[2]
Q <- BCef[3]

#Total modelled sales for M
Ts <- exp(-(P + Q) * Tdelta)

#Plot - Probability Distribution Function
BassPDF <- M * ((P + Q)^2/P) * M/(1 + (Q/P) * Ts)^2
plot(Tdelta, BassPDF, xlab = "Years", ylab = "Sales", type = "l")
points(Tdelta, Sales)

# Credits - Used script partly from 'Introductory Time Series with R' by P. Cowpertwait and A. Metcalfe, 2009
