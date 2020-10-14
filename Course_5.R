
library(tidyverse)
data(mtcars)
hist(mtcars$wt, breaks = 30)

## Statistische grootheden -----------------------------------------------#
# belangrijkste karakteristieken van een normale verdeling
mean(mtcars$wt, na.rm = TRUE)
var(mtcars$wt, na.rm = TRUE)
sd(mtcars$wt, na.rm = TRUE)
median(mtcars$wt, na.rm = TRUE)

## Werken met verdelingen ------------------------------------------------#
# tonen van de verdeling
data(mtcars)
hist(mtcars$wt, breaks = 30)

# genereren en plotten van veel voorkomende verdelingen
# normaal verdeeld: rnorm(n, mu, sigma)
dist <-rnorm(100, 200, 5)
hist(dist, breaks = 40)

## de standaard normale verdeling (mu=0, sigma=1)
## een normale verdeling is standaard als het gemiddelde 0 is en de sd 1
dist <-rnorm(100)
hist(dist, breaks = 40)

## de binomiale verdeling rbinom(n, trials, pi)
## 'trials' is het aantal worpen
dist <-rbinom(100, 1, 0.25)
hist(dist, breaks = 2)

## de poisson verdeling rbinom(n, lambda)
dist <-rpois(100, 2)
hist(dist)

## de uniforme verdeling runif(n, minx, max)
dist <-runif(100, 0, 40)
hist(dist, breaks = 20)

## tip: een handige manier om probability density functions te plotten (pdf)
library(visualize)
visualize.norm(-2)
visualize.norm(c(-1,1), section = "tails")

## Variantie analyse ----------------------------------------------------#

## klaarzetten dataset voor analyse
data(iris)
head(iris)
df <- iris %>% 
  select(Species, Sepal.Width)
table(df$Species)

## visuele controle normale verdeling
## via een histogram per plantsoort
df %>%
   ggplot(aes(x = Sepal.Width)) +
        geom_histogram(bins = 20) +
        facet_grid(. ~ Species)

## via een density-lijn per plantsoort
df %>%
        ggplot(aes(x = Sepal.Width)) +
        geom_density() +
        facet_grid(. ~ Species)

## visuele controle normale verdeling
## via drie density-lijnen in één plaatje
## een density-lijn is een smooth lijn door een histogram
df %>%
        ggplot(aes(x = Sepal.Width)) +
        geom_density(aes(group = Species, 
                     color = Species,
                     fill = Species),
                     alpha = 0.3)

## het analyseren van de variantie tussen de groepen
result <- aov(Sepal.Width ~ Species, data = df)
class(result)
summary(result)


## het nader bekijken en visualiseren van de variantie
tky <- TukeyHSD(result)
tky
plot(tky, las = 1, col = "red")
