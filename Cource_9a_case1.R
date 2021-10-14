library(tidyverse)

# The first step before you start modeling is to explore your data. In this course we’ll practice using tidyverse functions for exploratory data analysis. Start off this case study by examining your data set and visualizing the distribution of fuel efficiency. The ggplot2 package, with functions like ggplot() and geom_histogram(), is included in the tidyverse.
# 
# Instructions
# 
# The first time you run a code exercise, it may take a little while for your Docker container on mybinder.org to load. ⏳ Try to be patient!
#   
#   Wherever you see ___ in a code exercise, replace it with the correct code as instructed. Run the code (via the button) to see if it will run, and submit it (via the other button) to check if it’s correct.
# 
# The tidyverse metapackage is loaded for you, so you can use readr and ggplot2.
# 
# Take a look at the cars2018 object using glimpse().
# Use the appropriate column from the data set in the call to aes() so you can plot a histogram of fuel efficiency (miles per gallon, mpg).
# Set the correct x and y labels.

cars2018 <- read_csv("data/cars2018.csv")

## Read and explore data
# Print the cars2018 object
glimpse(___)

# Plot the histogram
ggplot(cars2018, aes(x = ___)) +
  geom_histogram(bins = 25) +
  labs(___ = "Fuel efficiency (mpg)",
          ___ = "Number of cars")

# Before embarking on more complex machine learning models, it’s a good idea to build the simplest possible model to get an idea of what is going on. In this case, that means fitting a simple linear model using base R’s lm() function.
# 
# Instructions
# 
# Use select() to deselect the two columns model and model_index from the model; these columns tell us the individual identifiers for each car and it would not make sense to include them in modeling.
# Fit mpg as the predicted quantity, explained by all the predictors, i.e., . in the R formula input to lm(). (You may have noticed the log distribution of MPG in the last exercise, but don’t worry about fitting the logarithm of fuel efficiency yet.)
# Print the summary() of the model.

## Prep and fit
# Deselect the 2 columns to create cars_vars
car_vars <- cars2018 %>%
  ___(-model, -model_index)

# Fit a linear model
fit_all <- ___(___ ~ ., data = ___)

# Print the summary of the model
___(fit_all)

# In tidymodels, you specify models using three concepts.
# 
# Model type differentiates models such as logistic regression, decision tree models, and so forth.
# Model mode includes common options like regression and classification; some model types support either of these while some only have one mode. (Notice in the example on this slide that we didn't need to set the mode for linear_reg() because it only does regression.)
# Model engine is the computational tool which will be used to fit the model. Often these are R packages, such as "lm" for OLS or the different implementations of random forest models.
# After a model has been specified, it can be fit, typically using a symbolic description of the model (a formula) and some data. We're going to start fitting models with data = car_train, as shown here. This means we're saying, "Just fit the model one time, on the whole training set". Once you have fit your model, you can evaluate how well the model is performing.

# Training models based on all of your data at once is typically not a good choice. 🚫 Instead, you can create subsets of your data that you use for different purposes, such as training your model and then testing your model.
# 
# Creating training/testing splits reduces overfitting. When you evaluate your model on data that it was not trained on, you get a better estimate of how it will perform on new data.
# 
# Instructions
# 
# Load the tidymodels metapackage, which also includes dplyr for data manipulation.
# Create a data split that divides the original data into 80%/20% sections and (roughly) evenly divides the partitions between the different types of transmission.
# Assign the 80% partition to car_train and the 20% partition to car_test.

car_vars <- readRDS("data/c1_car_vars.rds")

# Load tidymodels
___

# Split the data into training and test sets
set.seed(1234)
car_split <- car_vars %>%
  initial_split(prop = ___, strata = ___)

car_train <- training(___)
car_test <- testing(___)

glimpse(car_train)
glimpse(car_test)

car_vars <- readRDS("data/c1_car_vars.rds")

# Load tidymodels
library(tidymodels)

# Split the data into training and test sets
set.seed(1234)
car_split <- car_vars %>%
  initial_split(prop = 0.8, strata = transmission)

car_train <- training(car_split)
car_test <- testing(car_split)

glimpse(car_train)
glimpse(car_test)

# Now that your car_train data is ready, you can fit a set of models with tidymodels. When we model data, we deal with model type (such as linear regression or random forest), mode (regression or classification), and model engine (how the models are actually fit). In tidymodels, we capture that modeling information in a model specification, so setting up your model specification can be a good place to start. In these exercises, fit one linear regression model and one random forest model, without any resampling of your data.
# 
# Instructions
# 
# Load the tidymodels metapackage.
# Fit a basic linear regression model to your car_train data.
# (Notice that we are fitting to log(mpg) since the fuel efficiency had a log normal distribution.)

car_train <- readRDS("data/c1_train.rds")
car_test <- readRDS("data/c1_test.rds")

# Load tidymodels
___

# Build a linear regression model specification
lm_mod <- ___ %>%
  set_engine("lm")

# Train a linear regression model
fit_lm <- lm_mod %>%
  fit(log(mpg) ~ ., 
      data = ___)

# Print the model object
fit_lm

car_test <- readRDS("data/c1_test.rds")

# Load tidymodels
library(tidymodels)

# Build a linear regression model specification
lm_mod <- linear_reg() %>%
  set_engine("lm")

# Train a linear regression model
fit_lm <- lm_mod %>%
  fit(log(mpg) ~ ., 
      data = car_train)

# Print the model object
fit_lm

library(tidymodels)

car_train <- readRDS("data/c1_train.rds")
car_test <- readRDS("data/c1_test.rds")

# Build a random forest model specification
rf_mod <- ___ %>%
  set_engine("randomForest") %>%
  set_mode("regression")

# Train a random forest model
fit_rf <- rf_mod %>%
  ___(log(mpg) ~ ., 
      data = ___)

# Print the model object
fit_rf


library(tidymodels)

car_train <- readRDS("data/c1_train.rds")
car_test <- readRDS("data/c1_test.rds")

# Build a random forest model specification
rf_mod <- rand_forest() %>%
  set_engine("randomForest") %>%
  set_mode("regression")

# Train a random forest model
fit_rf <- rf_mod %>%
  fit(log(mpg) ~ ., 
      data = car_train)

# Print the model object
fit_rf


# The fit_lm and fit_rf models you just trained are in your environment. It’s time to see how they did! 🤩 How are we doing do this, though?! 🤔 There are several things to consider, including both what metrics and what data to use.
# 
# For regression models, we will focus on evaluating using the root mean squared error metric. This quantity is measured in the same units as the original data (log of miles per gallon, in our case). Lower values indicate a better fit to the data. It’s not too hard to calculate root mean squared error manually, but the yardstick package offers convenient functions for this and many other model performance metrics.
# 
# Instructions
# 
# Load the tidymodels metapackage, to access yardstick functions.
# Create new columns for model predictions from each of the models you have trained, first linear regression and then random forest.
# Evaluate the performance of these models using metrics() by specifying the column that contains the real fuel efficiency.

car_train <- readRDS("data/c1_train.rds")
fit_lm <- readRDS("data/c1_fit_lm.rds")
fit_rf <- readRDS("data/c1_fit_rf.rds")

# Load tidymodels
library(___)

# Create the new columns
results <- car_train %>%
  mutate(mpg = log(mpg)) %>%
  bind_cols(predict(___, car_train) %>%
              rename(.pred_lm = .pred)) %>%
  bind_cols(predict(___, car_train) %>%
              rename(.pred_rf = .pred))

# Evaluate the performance
metrics(results, truth = ___, estimate = .pred_lm)
metrics(results, truth = ___, estimate = .pred_rf)

car_train <- readRDS("data/c1_train.rds")
fit_lm <- readRDS("data/c1_fit_lm.rds")
fit_rf <- readRDS("data/c1_fit_rf.rds")

# Load tidymodels
library(tidymodels)

# Create the new columns
results <- car_train %>%
  mutate(mpg = log(mpg)) %>%
  bind_cols(predict(fit_lm, car_train) %>%
              rename(.pred_lm = .pred)) %>%
  bind_cols(predict(fit_rf, car_train) %>%
              rename(.pred_rf = .pred))

# Evaluate the performance
metrics(results, truth = mpg, estimate = .pred_lm)
metrics(results, truth = mpg, estimate = .pred_rf)

# “But wait!” you say, because you have been paying attention. 🤔 “That is how these models perform on the training data, the data that we used to build these models in the first place.” This is not a good idea because when you evaluate on the same data you used to train a model, the performance you estimate is too optimistic.
# 
# Let’s evaluate how these simple models perform on the testing data instead.
# 
# Instructions
# 
# What do you need to change to evaluate how the models perform on the testing data, instead of the training data?

library(tidymodels)

car_test <- readRDS("data/c1_test.rds")
fit_lm <- readRDS("data/c1_fit_lm.rds")
fit_rf <- readRDS("data/c1_fit_rf.rds")

# Create the new columns
results <- ___ %>%
  mutate(mpg = log(mpg)) %>%
  bind_cols(predict(fit_lm, ___) %>%
              rename(.pred_lm = .pred)) %>%
  bind_cols(predict(fit_rf, ___) %>%
              rename(.pred_rf = .pred))

# Evaluate the performance
metrics(results, truth = mpg, estimate = .pred_lm)
metrics(results, truth = mpg, estimate = .pred_rf)

car_test <- readRDS("data/c1_test.rds")
fit_lm <- readRDS("data/c1_fit_lm.rds")
fit_rf <- readRDS("data/c1_fit_rf.rds")

# Create the new columns
results <- car_test %>%
  mutate(mpg = log(mpg)) %>%
  bind_cols(predict(fit_lm, car_test) %>%
              rename(.pred_lm = .pred)) %>%
  bind_cols(predict(fit_rf, car_test) %>%
              rename(.pred_rf = .pred))

# Evaluate the performance
metrics(results, truth = mpg, estimate = .pred_lm)
metrics(results, truth = mpg, estimate = .pred_rf)

# You just trained models one time on the whole training set and then evaluated them on the testing set. Statisticians have come up with a slew of approaches to evaluate models in better ways than this; many important ones fall under the category of resampling.
# 
# The idea of resampling is to create simulated data sets that can be used to estimate the performance of your model, say, because you want to compare models. You can create these resampled data sets instead of using either your training set (which can give overly optimistic results, especially for powerful ML algorithms) or your testing set (which is extremely valuable and can only be used once or at most twice).

# The first resampling approach we're going to try in this course is called the bootstrap. Bootstrap resampling means drawing with replacement from our original dataset and then fitting on that dataset.
# 
# Let's think about...cars! 🚗🚌🚙🚕
# 
# Let's say our training dataset has 900 cars in it.
# To make a bootstrap sample, we draw with replacement 900 times from that training data to get the same size sample that we started with.
# 
# Since we're drawing with replacement, we will probably draw some cars more than once. We then fit our model on that new set of 900 cars that contains some duplicates, and evaluate the model on the cars that are not included in the new set of 900.
# 
# Then we do that again.
# We draw 900 times from the training dataset with replacement again and fit another model. We repeat that some number of times, look at all the models we fit on the bootstrap samples, determine each model's individual performance by evaluating on the cars that were not included in each bootstrap resample, and then take an average of the performance metrics.
# 
# This approach does take longer, obviously, than fitting on the data one time. In your exercise, you will have a subset of the complete dataset to try this out with.
# 
# I am very happy to be able to tell you that creating resamples is not too complicated with tidymodels. There are functions such as bootstraps() and similar for other types of resampling. The default behavior is to do 25 bootstrap resamplings, but you can change this if you want to. Notice that we resampled the car_train dataset, which is the training data.
# 
# The column splits is of type list. Instead of containing numbers or characters, this column contains lists. Each split in that column keeps track of which of the original data points are in the analysis set for that resample.

car_boot <- bootstraps(car_train)

# Once you have created a set of resamples, you can use the function fit_resamples() to fit a model to each resample and compute performance metrics for each.
# 
# The code on this slide shows how to fit our model specification lm_mod to the 25 bootstrap resamples in car_boot. This will fit our regression model 25 times, each time to a different bootstrapped version of the training data. We also determine how well our regression model performed 25 times, each time on the smaller subset of training data set aside when fitting. The fitted models themselves are just thrown away and not stored in the output, because they are only used for computing performance metrics.
# 
# To fit the random forest to these resamples and find performance metrics, we would use rf_mod instead.

lm_mod %>%
  fit_resamples(
    log(mpg) ~ .,
    car_boot,
    control = control_resamples(save_pred = TRUE)
  )

# We will not save the fitted models but we are going to save our predictions in fit_resamples() using save_pred = TRUE. This is so we can be especially clear about what it is that we are comparing during this process.
# 
# Each car has a real fuel efficiency as reported by the Department of Energy and then we have built models that predict fuel efficiency for each car. When we evaluate a model, we are calculating how far apart each predicted value is from each real value.
# 
# In this lesson, you also are going to visualize these differences, like you see here. The x-axis has the actual fuel efficiency and the y-axis has the predicted fuel efficiency for each kind of model.
# 
# The difference between linear regression and random forest isn't huge here, but in this case, we can see visually that the random forest model is performing better. The slope for the random forest model is closer to the dotted line (the slope = 1 line) and the spread around the line is smaller for the random forest model.

## Bootstrap Resampling
# In the last set of exercises, you trained linear regression and random forest models without any resampling. Resampling can help us evaluate our machine learning models more accurately.
# 
# Let’s try bootstrap resampling, which means creating data sets the same size as the original one by randomly drawing with replacement from the original. In tidymodels, the default behavior for bootstrapping is 25 resamplings, but you can change this using the times argument in bootstraps() if desired.
# 
# Instructions
# 
# The data set available in your environment is 10% of its original size, to allow the code in this exercise to evaluate quickly. (This means you will see some warnings, such as about rank-deficient fits.)
# 
# Create bootstrap resamples to evaluate these models. The function to create this kind of resample is bootstraps().
# Evaluate both kinds of models, the linear regression model and the random forest model.
# Use the bootstrap resamples you created car_boot for evaluating both models.

library(tidymodels)

car_train <- readRDS("data/c1_train_10_percent.rds")

lm_mod <- linear_reg() %>%
  set_engine("lm")

rf_mod <- rand_forest() %>%
  set_engine("randomForest") %>%
  set_mode("regression")

## Create bootstrap resamples
car_boot <- ___(car_train)

# Evaluate the models with bootstrap resampling
lm_res <- ___ %>%
  fit_resamples(
    log(mpg) ~ .,
    resamples = ___,
    control = control_resamples(save_pred = TRUE)
  )

rf_res <- ___ %>%
  fit_resamples(
    log(mpg) ~ .,
    resamples = ___,
    control = control_resamples(save_pred = TRUE)
  )

library(tidymodels)

car_train <- readRDS("data/c1_train_10_percent.rds")

lm_mod <- linear_reg() %>%
  set_engine("lm")

rf_mod <- rand_forest() %>%
  set_engine("randomForest") %>%
  set_mode("regression")

## Create bootstrap resamples
car_boot <- bootstraps(car_train)

# Evaluate the models with bootstrap resampling
lm_res <- lm_mod %>%
  fit_resamples(
    log(mpg) ~ .,
    resamples = car_boot,
    control = control_resamples(save_pred = TRUE)
  )

rf_res <- rf_mod %>%
  fit_resamples(
    log(mpg) ~ .,
    resamples = car_boot,
    control = control_resamples(save_pred = TRUE)
  )

glimpse(rf_res)

## Plot results

# You just trained models on bootstrap resamples of the training set and now have the results in lm_res and rf_res. These results are available in your environment, trained using the entire training set instead of 10% only. Now let’s compare them.
# 
# Notice in this code how we use bind_rows() from dplyr to combine the results from both models, along with collect_predictions() to obtain and format predictions from each resample.
# 
# Instructions
# 
# First collect_predictions() for the linear model.
# Then collect_predictions() for the random forest model.

library(tidymodels)

lm_res <- readRDS("data/c1_lm_res.rds")
rf_res <- readRDS("data/c1_rf_res.rds")

results <-  bind_rows(___ %>%
                           collect_predictions() %>%
                           mutate(model = "lm"),
                         ___ %>%
                           collect_predictions() %>%
                           mutate(model = "rf"))

glimpse(results)

lm_res <- readRDS("data/c1_lm_res.rds")
rf_res <- readRDS("data/c1_rf_res.rds")

results <-  bind_rows(lm_res %>%
                        collect_predictions() %>%
                        mutate(model = "lm"),
                      rf_res %>%
                        collect_predictions() %>%
                        mutate(model = "rf"))

glimpse(results)

#Wonderful! Sit back and run the given code to visualize the results!

library(tidymodels)

lm_res <- readRDS("data/c1_lm_res.rds")
rf_res <- readRDS("data/c1_rf_res.rds")

results <-  bind_rows(lm_res %>%
                        collect_predictions() %>%
                        mutate(model = "lm"),
                      rf_res %>%
                        collect_predictions() %>%
                        mutate(model = "rf"))

results %>%
  ggplot(aes(`log(mpg)`, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(aes(color = id), size = 1.5, alpha = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm") +
  facet_wrap(~ model)

# Congratulations on finishing the first case study! 🙌 Both the model metrics and the plots show that the random forest model is performing better. We can predict fuel efficiency more accurately with a random forest model.



