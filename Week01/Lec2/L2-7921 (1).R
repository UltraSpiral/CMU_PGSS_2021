# Where to find datasets?
library(datasets)

seatbelts <- datasets::Seatbelts

df <- as.data.frame(seatbelts)
# df$Date <- rownames(seatbelts)

View(df)

# install.packages("zoo")
# install.packages("xts")
library(zoo)
library(xts)
# df$Date <- as.yearmon(time(seatbelts))
df$Date <- as.Date(time(seatbelts))


df$law <- as.factor(df$law)
sapply(df,class)

boxplot(DriversKilled ~ law, data = df)
t.test(DriversKilled ~ law, data = df)

boxplot(VanKilled ~ law, data = df)
t.test(VanKilled ~ law, data = df)


## If the hypothesis that drivers killed were fewer when seatbelt law is in effect is TRUE
## then, can we predict, based on the measurable number of deaths on a given day, whether 
##the law was in effect or not
      # This is a classification problem

#  Y = Law
#  X = [DriversKilled, VanKilled]

#  Logistic Regression
model <- glm (formula = law ~ DriversKilled + VanKilled, family = "binomial", data = df)
summary(model)

exp(model$coefficients)
##  DriversKilled     VanKilled 
##   0.9490347        0.5935072 

predictionsOfLawInEffect <- predict(model, newdata = df)
predictionsOfLawInEffect_CATEGORICAL <- predictionsOfLawInEffect > -2

table(df$law, predictionsOfLawInEffect_CATEGORICAL) #Confusion matrix


plot(density(exp(predictionsOfLawInEffect)))   #Plot of the "ODDS" of each row being classified for the law being in effect
# ODDS = p / (1 - p); where ODDS = exp(predictionsOfLawInEffect)
# => (1 - p)*ODDS = p
# => p = ODDS/(1+ODDS)

# Probability of each row  corresponding to the law being in effect: 
plot(density(exp(predictionsOfLawInEffect)/(1+exp(predictionsOfLawInEffect))))  # plot of the "ODDS" of each row being classified for the law being in effect
summary(exp(predictionsOfLawInEffect)/(1+exp(predictionsOfLawInEffect)))
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000343 0.0030743 0.0338102 0.1197917 0.1595388 0.8213848

# Linear Regression for REGRESSION of the number of people that died ON the Xs
#  /regressors law-being-in-effect and total number of drivers
# Y = number of people that died
# X = [law, drivers, PetrolPrice]

model2 <- lm (DriversKilled ~ law, data = df)
summary (model2)
