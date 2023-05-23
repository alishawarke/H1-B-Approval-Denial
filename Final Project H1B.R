##################################################
#   Analysis on H1B approval and denial
##################################################

library(lmtest)
library(sandwich)

H1B <- data.frame(H1B_Data_Project)

##################################################
#   Logistic Regression
##################################################

# Regressing the status on the wage.
# To test what effect does yearly wage has on approval and denial of the H1B application.

H1Blogit <- glm(status ~ wage, 
                 family = binomial(link = "logit"), 
                 data = H1B)
coeftest(H1Blogit, vcov. = vcovHC, type = "HC1")

plot(x = H1B$wage,
     y = H1B$status,
     main = "Probit and Logit Models Model of the Probability of Denial, \n Given the yearly wage",
     xlab = "Yearly Wage",
     ylab = "Certified/Denied",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.9)

abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "H1B denied")
text(2.5, -0.1, cex= 0.8, "H1B approved")

x <- seq(0, 1500000, 500000)
y_logit <- predict(H1Blogit, list(wage = x), type = "response")
lines(x, y_logit, lwd = 1.5, col = "blue", lty = 2)

# Regressing status with wage and worksite (state)

H1Blogit2 <- glm(status ~ wage + worksite, 
                  family = binomial(link = "logit"), 
                  data = H1B)

coeftest(H1Blogit2, vcov. = vcovHC, type = "HC1")

#Regressing the status with wage and dependent status (married/not married) 

H1Blogit3 <- glm(status ~ wage + dependent, 
                 family = binomial(link = "logit"), 
                 data = H1B)

coeftest(H1Blogit3, vcov. = vcovHC, type = "HC1")


