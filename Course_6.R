
## basic correlation --------------------------------------#
# correlatie in beeld met een gewone scatterplot
library(tidyverse)
data(mtcars)
qplot(mtcars$wt, mtcars$mpg)
ggplot(aes(wt, mpg), data = mtcars) +
        geom_point()

# de precieze waarde van de correlatie
# i.e. de correlatie-coëfficiënt
cor(mtcars$wt, mtcars$mpg)

# voor een compleet overzicht
# ggally:ggpairs 
library(GGally)
ggpairs(mtcars)

## basic linear regression  -------------------------------#
library(tidyverse)
data(mtcars)
df_in <- mtcars %>% 
  select(hp, wt)
my_model <- lm(wt ~ hp, data = df_in)
class(my_model)
summary(my_model)

## characteristics of a model-object
my_model
my_model$coefficients
my_model$coefficients[1]
my_model$coefficients[2]

## de confidence interval van de coefficient
summary(my_model)
confint(my_model)


## oefening met enkelvoudige regressie
## Probeer om met behulp van lineaire regressie te voorspellen
## wat de stopafstand van een auto is,  bij een snelheid van 
## Maak hierbij gebruik van de datasets 'cars'

data("cars")
summary(cars)

## create additional columns for the predictions and the residuals
new <- select(df_in, hp)
df_out <- df_in %>% 
  mutate(preds = round(predict(my_model, new), digits = 3)) %>% 
  mutate(resids = round(wt - preds, digits = 3))
head(df_out)

## plotting the regression using out own model
df_out %>% 
  ggplot(aes(hp, wt)) +
  geom_point() +
  geom_line(color='red', data = df_out, aes(hp, preds))
        
## plotting the regression using ggplot's lm function
df_out %>% 
  ggplot(aes(hp, wt)) +
  geom_point() +
  geom_smooth(method = "lm")

## to create a 4-panel check of outliers
par(mfrow = c(2,2))
plot(my_model)

## for a separate Q-Q residuals plot
std_model <- rstandard(my_model)
qqnorm(std_model,
  ylab="Std. residuals",
  xlab="Normal scores",
  main="Normal Q-Q")
qqline(std_model)

## of
qqnorm(mtcars$mpg)
qqline(mtcars$mpg)

## meervoudige lineaire regressie
Boston <- read.csv("https://raw.githubusercontent.com/Scavetta/conf_tensorflow_training_day1/master/1_Deep_Learning_Intro/data/boston_keras.csv")
summary(Boston)
attach(Boston)
fit1 <- lm(MEDV ~ RM)
fit2 <- lm(MEDV ~ RM + CRIM)
fit3 <- lm(MEDV ~ RM + CRIM + NOX)
fit4 <- lm(MEDV ~ RM + CRIM + NOX + AGE)
fit5 <- lm(MEDV ~ RM + CRIM + NOX + AGE + LSTAT)
anova(fit1, fit2, fit3, fit4, fit5)

## De uitslag bekijkend: 
## De significantie verbeterde toen ik CRIM toevoegde in model 2, 
## in model 3 werd het wat minder (NOX is dus niet zo interessant),
## model 4 werd helemaal slecht (AGE lijkt geen goede optie) en
## werd weer beter toen ik LSTAT toevoegde in model 5.
## DUS: RM willen we graag, CRIM helpt en LSTAT helpt ook.
## even nog wat andere variabelen bekijken.

fit1 <- lm(MEDV ~ RM)
fit2 <- lm(MEDV ~ RM + LSTAT)
fit3 <- lm(MEDV ~ RM + LSTAT + CHAS)
fit4 <- lm(MEDV ~ RM + LSTAT + CHAS + PTRATIO)
fit5 <- lm(MEDV ~ RM + LSTAT + CHAS + PTRATIO + DIS)
anova(fit1, fit2, fit3, fit4, fit5)

## na wat spelen met de resterende variabelen:
fit <- lm(MEDV ~ RM + LSTAT + PTRATIO + CHAS)
summary(fit)
