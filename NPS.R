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
PSales <- c(2130, 3212, 6320, 8236, 7213)

#Period of sales - Set your own here
Timee <- 1:length(PSales)

#Change in Sales
Tdelta <- (1:100)/length(PSales)

#Bass Model Formula
BassModel <- PSales ~ (M * ( ( (P+Q)^2 / P) * exp(-(P+Q) * Timee) ) / (1+ (Q/P) * exp(-(P+Q) * Timee))^2)

# Avg. P and Q values used - Use your own if known
#Use best assessment values for M

BassFor.nlsLM <- nlsLM(BassModel , start = list( M = max(PSales), P = 0.03, Q = 0.38))
BassFor.nlsLM <- update(BassFor.nlsLM)
summary(BassFor.nlsLM)

#Prediction Model - Use your values here
EstSales <- seq(1, 20, by = 1)

#New data frame with colnames as 'time' to run 'predictNLS' function
EstSales <- data.frame(Timee = EstSales)

#Confidence set at default values of 95%, change alpha to your own levels here
BassFor_predicted <- predictNLS(BassFor.nlsLM, newdata = EstSales)

BassFor_predicted

SalesPeak <- (M * (P+Q)^2) / 4*Q
TimePeak <- ((P+Q)^-1)* log(Q/P)

SalesPeak
TimePeak

#Co-efficients for the model
BCef <- coef(BassFor.nlsLM)
M <- BCef[1]
P <- BCef[2]
Q <- BCef[3]

#Reset totalsales for M
Ts <- exp(-(P + Q) * Tdelta)

#Plot - Probability Distribution Function
BassPDF <- M * ((P + Q)^2/P) * Ts/(1 + (Q/P) * Ts)^2
plot(Tdelta, BassPDF, xlab = "Years", ylab = "Sales", type = "l")
points(Timee, PSales)



# Credits - Used script partly from 'Introductory Time Series with R' by P. Cowpertwait and A. Metcalfe, 2009
