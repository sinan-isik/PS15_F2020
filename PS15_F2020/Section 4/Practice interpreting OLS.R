# Alice Lepissier

# Find data-sets to practice with here:
# https://vincentarelbundock.github.io/Rdatasets/datasets.html

#install.packages("faraway")
library(faraway)
attach(pima)
pima <- pima
help(pima)

plot(pima$bmi, pima$diastolic,
     xlab = "Body Mass Index", 
     ylab = "Diastolic blood pressure (mm Hg)", #mmHg is millimeters of mercury
     main = "Blood pressure as a function of BMI")

model1 <- lm(diastolic ~ bmi, data = pima)
summary(model1)
abline(model1, col = "green")


#install.packages("reshape2")
library(reshape2)
attach(tips)
tips <- tips
help(tips)

plot(tips$size, tips$tip,
     xlab = "Size of the party", 
     ylab = "Tips in dollars",
     main = "Tips as a function of the size of dinner party")

model2 <- lm(tip ~ size, data = tips)
summary(model2)
abline(model2, col = "purple")


#install.packages("boot")
library(boot)
attach(survival)
rats <- survival
help(survival)

plot(rats$dose, rats$surv,
     xlab = "Dose of radiation administered (in rads)", 
     ylab = "% survival rate of batches of rats",
     main = "Rats' survival rate as a function of radiation dosage administered")

model3 <- lm(surv ~ dose, data = rats)
summary(model3)
abline(model3, col = "cyan")


library(car)
attach(Freedman)
Freedman <- Freedman
help(Freedman)

plot(Freedman$density, Freedman$crime,
     xlab = "Population per square mile", 
     ylab = "Crime rate per 100,000",
     main = "Crime rates in US metropolitan areas in 1968")

model4 <- lm(crime ~ density, data = Freedman)
summary(model4)
abline(model4, col = "orange")

attach(UN)
UN <- UN
help(UN)

plot(UN$ppgdp, UN$infantMortality,
     xlab = "GDP per capita (US dollars)", 
     ylab = "Infant mortality rate (deaths per 1,000 live births)",
     main = "Infant mortality as a function of GDP per capita")

model5 <- lm(infantMortality ~ ppgdp, data = UN)
summary(model5)
abline(model5, col = "pink")

