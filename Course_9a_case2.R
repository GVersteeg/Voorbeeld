# In this case study, you are going to use data from the annual Developer Survey to build predictive models. First, you'll do exploratory data analysis to understand what's in the dataset, and how some of the quantities in the survey are distributed, and then you'll practice your machine learning skills by training classification models.

# Every year, the data for the Stack Overflow Developer Survey is made public, so this is all data that you can access and analyze yourself. I've already done some data cleaning and preparation, but we'll practice some of that in this chapter as well. There are a lot of predictive modeling possibilities in this dataset.

# The specific question we are going to address is what makes a developer more likely to work remotely. Developers can work in their company offices or they can work remotely, and it turns out that there are specific characteristics of developers, such as the size of the company that they work for, how much experience they have, or where in the world they live, that affect how likely they are to be a remote developer.

stack_overflow %>% 
  count(remote)

# That is what you are going to model! One of the things you'll notice right away about this dataset, however, is that the proportion of developers who are remote and those who work in an office is not balanced. This kind of class imbalance can have a significant negative impact on model performance, so we are going to have to cope with it. We will need to preprocess our data before we model it.
## Class Imbalance

# Anytime you are planning to implement modeling, it is always a good idea to explore your dataset. Start off this modeling analysis by checking out how many remote and non-remote developers you have to work with, where they live, and how much experience they have.
# 
# Instructions
# 
# Take a look at the stack_overflow object.
# In the calls to count(), check out the distributions for remote status first, and then country.

library(tidyverse)
stack_overflow <- read_csv("data/stack_overflow.csv")

# Take a look at stack_overflow
glimpse(___)

# First count for `remote`
stack_overflow %>% 
  count(___, sort = TRUE)

# then count for `country`
stack_overflow %>% 
  ___(___, sort = TRUE)

stack_overflow <- read_csv("data/stack_overflow.csv")

# Take a look at stack_overflow
glimpse(stack_overflow)

# First count for `remote`
stack_overflow %>% 
  count(remote, sort = TRUE)

# then count for `country`
stack_overflow %>% 
  count(country, sort = TRUE)

# Instructions
# 
# Use the appropriate column from the data set so you can plot a boxplot with remote status on the x-axis and professional experience on the y-axis.


library(tidyverse)
stack_overflow <- read_csv("data/stack_overflow.csv")

ggplot(stack_overflow, aes(___, ___)) +
  geom_boxplot() +
  labs(x = NULL,
       y = "Years of professional coding experience")

library(tidyverse)
stack_overflow <- read_csv("data/stack_overflow.csv")

ggplot(stack_overflow, aes(remote, years_coded_job)) +
  geom_boxplot() +
  labs(x = NULL,
       y = "Years of professional coding experience")

# Great! 👍 Notice the distribution of countries, the effect of experience, and most importantly, the imbalance between remote and non-remote workers in this dataset.

# Before you deal with the imbalance in the remote/not remote classes, first split your data into training and testing sets. You create subsets of your data for training and testing your model for the same reasons you did before: to reduce overfitting and obtain a more accurate estimate for how your model will perform on new data.
# 
# Instructions
# 
# Create a data split that divides the original data into 80%/20% sections and about evenly divides the sections between the different classes of remote.
# 
# Load the tidymodels metapackage.
# Create stack_split:
#   
#   For the first argument to initial_split(), use a value for prop of 0.8.
# For the second argument to initial_split(), stratify the split by remote status.
# Use training() to assign the 80% partition to stack_train and use testing() to assign the 20% partition to stack_test.

stack_overflow <- read_csv("data/stack_overflow.csv") %>%
  mutate(remote = factor(remote, levels = c("Remote", "Not remote"))) %>%
  mutate_if(is.character, factor)

# Load tidymodels
library(___)

# Create stack_select dataset
stack_select <- stack_overflow %>%
  select(-respondent)

# Split the data into training and testing sets
set.seed(1234)
stack_split <- stack_select %>%
  initial_split(___,
                   strata = ___)

stack_train <- ___(stack_split)
stack_test <- ___(stack_split)

glimpse(stack_train)
glimpse(stack_test)

stack_overflow <- read_csv("data/stack_overflow.csv") %>%
  mutate(remote = factor(remote, levels = c("Remote", "Not remote"))) %>%
  mutate_if(is.character, factor)

# Load tidymodels
library(tidymodels)

# Create stack_select dataset
stack_select <- stack_overflow %>%
  select(-respondent)

# Split the data into training and testing sets
set.seed(1234)
stack_split <- stack_select %>%
  initial_split(prop = 0.8,
                strata = remote)

stack_train <- training(stack_split)
stack_test <- testing(stack_split)

glimpse(stack_train)
glimpse(stack_test)

# Great job creating training and testing data sets! Now you can deal with that class imbalance.

# It's good that we're going to talk about class imbalance because it comes up a lot in real life. In many practical, real-world situations, there are a lot more of one kind of category in a dataset than another. In our example here, there are about ten times more non-remote developers than there are remote developers. What can happen in a situation like this is that a machine learning model will always predict the majority class or otherwise exhibit poor performance on the metrics that we care about

stack_overflow %>% 
  count(remote)

# This is in fact what happens with our dataset here (I know because I tested it out ✅) so we need to do something to address this imbalance. There are a variety of options available to you, which vary from quite simple to more complex, and we're going to start with a simple option.

## Class imbalance

stack_overflow %>% 
  count(remote)
# A tibble: 2 x 2
# remote         n
# <fct>      <int>
# 1 Remote       718
# 2 Not remote  6273

# Downsampling
# - Remove some of the majority class so it has less effect on the predictive model
# - Randomly remove examples from the majority class until it is the same size as the minority class

# In this case study, we're going to implement downsampling, also known as undersampling. With this approach, we randomly remove observations from the majority class until it's the same size as the minority class and both classes can have the same effect on the machine learning model we're training.
# Are we really going to throw out a large percentage of our data here?! 😱 Yes! We do this because such an approach can be helpful at producing a useful model that can recognize both classes, instead of only one.

# In our case study, there are roughly ten times more non-remote developers compared to the remote developers.
# When we implement downsampling, we remove some of the non-remote developers until the proportion is equal and the classes are balanced. This approach is simple to implement and understand, but there are other more complex approaches to class imbalance available as well.

library(themis)

stack_recipe <- recipe(remote ~ ., data = stack_train) %>% 
  step_downsample(remote)

# Downsampling is an example of a preprocessing step for modeling. In tidymodels, you can preprocess your data using recipes. The recipe shown in this slide only has one preprocessing step (downsampling, that comes from an extra add-on package called themis), but you can implement many steps on one dataset during preprocessing. There are an enormous number of different kinds of preprocessing you can do, from creating indicator variables to implementing principal component analysis to extracting date features and more.

stack_prep <- prep(stack_recipe)
bake(stack_prep, new_data = NULL)

# When you prep() a recipe, you estimate the required parameters from a data set for the preprocessing steps in that recipe (as an example, think about finding the mean and standard deviation if you are centering and scaling).
# When you bake() a prepped recipe with new_data = NULL, you get the preprocessed data back out.
# You don't typically need to prep() and bake() recipes when you use tidymodels, but they are helpful functions to have in your toolkit for confirming that recipes are doing what you expect.

# Does it make sense to try to change the class imbalance of the test set?
# No, it does not! 🙅 You want the set test to look like new data that your model will see in the future, so you don't want to mess with the class balance there; you want to see how your model will perform on imbalanced data, even if you have trained it on artificially balanced data.

# There are multiple possible approaches to dealing with class imbalance. ⚖️ Here, you will implement downsampling using the step_downsample() function from the themis package.
# 
# Instructions
# Use a recipe() to preprocess your training data.
# Downsample this data with respect to the remote status of the developers.

library(themis)

stack_train <- readRDS("data/c2_train.rds")

stack_recipe <- ___(remote ~ ., data = stack_train) %>% 
  step_downsample(___)

stack_recipe

library(themis)

stack_train <- readRDS("data/c2_train.rds")

stack_recipe <- recipe(remote ~ ., data = stack_train) %>% 
  step_downsample(remote)

stack_recipe

# Once your recipe is defined, you can estimate the parameters required to actually preprocess the data, and then extract the processed data. This typically isn’t necessary is you use a workflow() for modeling, but it can be helpful to diagnose problems or explore your preprocessing results.
# 
# Instructions
# 
# First, prep() the recipe.
# Then, bake() the prepped recipe with new_data = NULL to see the processed training data.
# Check out the results of counting remote status after downsampling! You likely will not need to prep() and bake() when building a model but you can use this to check and explore, as well as to troubleshoot when things go wrong.

library(tidymodels)
library(themis)

stack_train <- readRDS("data/c2_train.rds")

stack_recipe <- recipe(remote ~ ., data = stack_train) %>% 
  step_downsample(remote)

stack_prep <- prep(___)
stack_down <- bake(___, new_data = NULL)

stack_down %>%
  count(remote)
library(themis)

stack_train <- readRDS("data/c2_train.rds")

stack_recipe <- recipe(remote ~ ., data = stack_train) %>% 
  step_downsample(remote)

stack_prep <- prep(stack_recipe)
stack_down <- bake(stack_prep, new_data = NULL)

stack_down %>%
  count(remote)

# Now you have a data set with balanced classes, ready for machine learning!
# When you bake() the prepped recipe stack_prep with new_data = NULL, you extract the processed (i.e. balanced) training data.

# Now that you have understood and implemented downsampling, or undersampling, we can finally get down to the business of building supervised machine learning models to predict which developers work remotely and which do not

# Classification models
# - Logistic regression
# - Decision tree

# Unlike the first case study, when you built regression models to predict a numeric or continuous variable, in this case study you are going to build classification models, to predict the class: remote or not remote. We are going to stick with two methods to understand and implement classification models, logistic regression and a decision tree.
# 
# There are lots of other options, and one of the great characteristics of using tidymodels for predictive modeling is that if you want to try something else, you can extend your work to new model types within the same framework. 💁

# Logistic regression
glm_spec <- logistic_reg() %>%
  set_engine("glm")

# Decision tree
tree_spec <- decision_tree() %>%         
  set_engine("rpart") %>%      
  set_mode("classification") 

# We are going to use model specifications from parsnip to set up the models. Notice here that one model is logistic regression while the other is a decision tree. How do we combine these model specifications with the data preprocessing we need to do from our recipe? 🤔

## Build a workflow
# You have a few options for that, but one straightforward way is to use a workflow(), an object that makes it easier to carry around pieces of, well, modeling workflows! The components of a workflow() go together like LEGO blocks; you add a preprocessor like a recipe or a formula, and a model.
# If you don't add one of those components (for example, stack_wf on this slide) the workflow() holds an empty spot ready for, say, the model. You may find this a convenient way to write your modeling code when you want to fit with the same preprocessor but different model specifications.

stack_wf <- workflow() %>%
  add_recipe(stack_recipe)

stack_wf %>%
  add_model(glm_spec)

## Fit a workflow
# A workflow() can be fit in much the same way a model can, and all the pieces are composable and pipeable. 🎉

stack_wf <- workflow() %>%
  add_recipe(stack_recipe)

stack_wf %>%
  add_model(tree_spec) %>%
  fit(data = stack_train)

## Evaluating your model
# Classification models can be evaluated using a confusion matrix. This kind of matrix or table counts which examples were classified correctly and incorrectly. Notice here that with the decision tree model being evaluated with the testing data, 73 real remote developers were classified as remote, and 898 not remote developers were classified as not remote. The conf_mat() function in yardstick is very helpful!

results %>%
  conf_mat(truth = remote, estimate = .pred_tree)

# Often you want other measures of model performance, though, in a format that will be friendlier to data frames, and for that you can use other functions from the yardstick package.
# Here we can see the overall accuracy, as well as the positive and negative predictive values, for the logistic regression model evaluated on the testing data. We'll pass three arguments to these functions: first, a data frame with real data on remote status and predicted remote status for our observations, and then the columns in that data frame that give us the true class and the predicted class for each developer in our data set.

accuracy(results, truth = remote, estimate = .pred_glm)
# # A tibble: 1 x 3
# .metric  .estimator .estimate
# <chr>    <chr>          <dbl>
#   1 accuracy binary         0.657

ppv(results, truth = remote, estimate = .pred_glm)
# # A tibble: 1 x 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
#   1 ppv     binary         0.160

npv(results, truth = remote, estimate = .pred_glm)
# # A tibble: 1 x 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
#   1 npv     binary         0.941

# Finally! 😁 It’s time to train predictive models for this data set of Stack Overflow Developer Survey responses. We will specify our machine learning models with parsnip, and use workflows for convenience.
# 
# Instructions
# 
# Specify a logistic regression model using logistic_reg().
# Build a workflow() to hold your modeling components.
# Add your model specification to your workflow() before fitting.

library(themis)

stack_train <- readRDS("data/c2_train.rds")

stack_recipe <- recipe(remote ~ ., data = stack_train) %>% 
  step_downsample(remote)

## Build a logistic regression model
glm_spec <- ___ %>%
  set_engine("glm")

## Start a workflow (recipe only)
stack_wf <- ___ %>%
  add_recipe(stack_recipe)

## Add the model and fit the workflow
stack_glm <- stack_wf %>%
  add_model(___) %>%
  fit(data = stack_train)

# Print the fitted model
stack_glm

library(themis)

stack_train <- readRDS("data/c2_train.rds")

stack_recipe <- recipe(remote ~ ., data = stack_train) %>% 
  step_downsample(remote)

## Build a logistic regression model
glm_spec <- logistic_reg() %>%
  set_engine("glm")

## Start a workflow (recipe only)
stack_wf <- workflow() %>%
  add_recipe(stack_recipe)

## Add the model and fit the workflow
stack_glm <- stack_wf %>%
  add_model(glm_spec) %>%
  fit(data = stack_train)

# Print the fitted model
stack_glm

# Instructions
# Build a decision tree model with downsampling.
# 
# Specify a decision tree regression model using decision_tree().
# Add your recipe stack_recipe to your workflow().
# Fit your workflow, after you have added your model to it.

library(tidymodels)
library(themis)

stack_train <- readRDS("data/c2_train.rds")

stack_recipe <- recipe(remote ~ ., data = stack_train) %>% 
  step_downsample(remote)

## Build a decision tree model
tree_spec <- ___ %>%         
  set_engine("rpart") %>%      
  set_mode("classification") 

## Start a workflow (recipe only)
stack_wf <- workflow() %>%
  ___(___)

## Add the model and fit the workflow
stack_tree <- stack_wf %>%
  add_model(tree_spec) %>%
  __(data = stack_train)

# Print the fitted model
stack_tree

library(tidymodels)
library(themis)

stack_train <- readRDS("data/c2_train.rds")

stack_recipe <- recipe(remote ~ ., data = stack_train) %>% 
  step_downsample(remote)

## Build a decision tree model
tree_spec <- decision_tree() %>%         
  set_engine("rpart") %>%      
  set_mode("classification") 

## Start a workflow (recipe only)
stack_wf <- workflow() %>%
  add_recipe(stack_recipe)

## Add the model and fit the workflow
stack_tree <- stack_wf %>%
  add_model(tree_spec) %>%
  fit(data = stack_train)

# Print the fitted model
stack_tree

# A confusion matrix describes how well a classification model (like the ones you just trained!) performs. A confusion matrix tabulates how many examples in each class were correctly classified by a model. In your case, it will show you how many remote developers were classified as remote and how many non-remote developers were classified as non-remote; the confusion matrix also shows you how many were classified into the wrong categories.
# 
# Here you will use the conf_mat() function from yardstick to evaluate the performance of the two models you trained, stack_glm and stack_tree. The models available in your environment were trained on the training data.
# 
# Instructions
# 
# Print the confusion matrix for the stack_glm model on the stack_test data. If we wanted to compare more than two modeling options, we should definitely create some resampled data sets like we did in the first case study. This case study is already getting long, so let’s stick with the testing data.
# 
# Note that the first argument to conf_mat() is truth and the second is estimate.

library(tidymodels)

stack_train <- readRDS("data/c2_train.rds")
stack_test <- readRDS("data/c2_test.rds")
stack_glm <- readRDS("data/stack_glm.rds")
stack_tree <- readRDS("data/stack_tree.rds")

results <- stack_test %>%
  bind_cols(predict(stack_glm, stack_test) %>%
              rename(.pred_glm = .pred_class))

# Confusion matrix for logistic regression model
results %>%
  conf_mat(___ = remote, ___ = .pred_glm)

results %>%
  conf_mat(truth = remote, estimate = .pred_glm)
# Truth
# Prediction   Remote Not remote
# Remote         85        454
# Not remote     49        810

# Instructions
# 
# Print the confusion matrix for the stack_tree model on the stack_test data.

stack_train <- readRDS("data/c2_train.rds")
stack_test <- readRDS("data/c2_test.rds")
stack_glm <- readRDS("data/stack_glm.rds")
stack_tree <- readRDS("data/stack_tree.rds")

results <- stack_test %>%
  bind_cols(predict(stack_tree, ___) %>%
              rename(.pred_tree = .pred_class))

# Confusion matrix for decision tree model
results %>%
  ___(___ = remote, ___ = .pred_tree)


stack_train <- readRDS("data/c2_train.rds")
stack_test <- readRDS("data/c2_test.rds")
stack_glm <- readRDS("data/stack_glm.rds")
stack_tree <- readRDS("data/stack_tree.rds")

results <- stack_test %>%
  bind_cols(predict(stack_tree, stack_test) %>%
              rename(.pred_tree = .pred_class))

# Confusion matrix for decision tree model
results %>%
  conf_mat(truth = remote, estimate = .pred_tree)

# Truth
# Prediction   Remote Not remote
# Remote         73        359
# Not remote     61        905

# Nice! A confusion matrix is used to evaluate the performance of models, so you should use the testing set.

# The conf_mat() function is helpful but often you also want to store specific performance estimates for later, perhaps in a dataframe-friendly form. The yardstick package is built to handle such needs. For this kind of classification model, you might look at the positive or negative predictive value or perhaps overall accuracy.
# 
# The models available in your environment, stack_glm and stack_tree were trained on the training data.
# 
# Instructions
# 
# Predict values for logistic regression (stack_glm) and decision tree (stack_tree).
# Calculate both accuracy and positive predictive value for these two models.

library(tidymodels)

stack_train <- readRDS("data/c2_train.rds")
stack_test <- readRDS("data/c2_test.rds")
stack_glm <- readRDS("data/stack_glm.rds")
stack_tree <- readRDS("data/stack_tree.rds")

results <- stack_test %>%
  bind_cols(predict(stack_glm, ___) %>%
              rename(.pred_glm = .pred_class)) %>%
  bind_cols(predict(stack_tree, ___) %>%
              rename(.pred_tree = .pred_class))

## Calculate accuracy
___(results, truth = remote, estimate = .pred_glm)
___(results, truth = remote, estimate = .pred_tree)

## Calculate positive predict value
___(results, truth = remote, estimate = .pred_glm)
___(results, truth = remote, estimate = .pred_tree)


library(tidymodels)

stack_train <- readRDS("data/c2_train.rds")
stack_test <- readRDS("data/c2_test.rds")
stack_glm <- readRDS("data/stack_glm.rds")
stack_tree <- readRDS("data/stack_tree.rds")

results <- stack_test %>%
  bind_cols(predict(stack_glm, stack_test) %>%
              rename(.pred_glm = .pred_class)) %>%
  bind_cols(predict(stack_tree, stack_test) %>%
              rename(.pred_tree = .pred_class))

## Calculate accuracy
accuracy(results, truth = remote, estimate = .pred_glm)
accuracy(results, truth = remote, estimate = .pred_tree)

## Calculate positive predict value
ppv(results, truth = remote, estimate = .pred_glm)
ppv(results, truth = remote, estimate = .pred_tree)

# Congratulations on finishing the second case study! 🏆 In terms of overall accuracy and positive predictive value, the decision tree model outperforms the logistic regression model. We can predict the remote status of a developer more accurately with a decision tree model.

