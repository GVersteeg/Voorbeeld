## Surveying Catholic sisters in 1967

# The last case study in this course uses an extensive survey of Catholic nuns fielded in 1967 to once more put your practical machine learning skills to use. You will predict the age of these religious women from their responses about their beliefs and attitudes.

# It is a particularly compelling one, where we are going to practice some advanced skills in modeling.



Codebook: https://curate.nd.edu/downloads/0v838051f6x
CSV-file: https://curate.nd.edu/downloads/fn106w94g79

# In 1967, a Catholic nun named Sister Marie Augusta Neal who had a PhD in sociology from Harvard fielded a survey of all members of Catholic women's religious communities, i.e., nuns. It was a survey with over 600 questions that went out to over 130,000 individual sisters and was fielded in a time of significant change both for the Catholic church and society in general.
# This survey is so big with so much data that we're actually only giving you a subset, about one quarter of it, in your environment here during the case study. The whole survey is available online and it is pretty interesting, so I encourage you to take a look for yourself.

# There is demographic information in the survey, along with a lot of diverse kinds of questions. About 60 of the survey questions were agreement questions, with a statement (of some kind) that the survey asked the respondent to agree or disagree with on this scale. The answers are coded as integers from 1 to 5.

# Response	                  Code
# Disagree very much	        1
# Disagree somewhat	          2
# Neither agree nor disagree	3
# Agree somewhat	            4
# Agree very much	            5

# The integer 1 corresponds to "disagree very much" and 5 corresponds to "agree very much". This kind of coding is convenient for our modeling purposes here. The data that you will have available within this case study is only this subset of the survey: the agree/disagree statements, plus age.

# The original survey question asked for age in bins of 10 years, under 20, from 21 to 30, from 31 to 40, and so forth. I have recoded these as numeric values corresponding to the top of each age bin, 20, 30, 40, and so on. This means that this isn't quite a continuous variable, since we haven't measured these women's ages very precisely, but we can do a pretty good job modeling it as if it is. There are more sophisticated modeling approaches possible with these kinds of measurements that you could explore.

# Opinions and attitudes in the 1960s
# - "Catholics should boycott indecent movies."
# - "In the past 25 years, this country has moved dangerously close to socialism."
# - "I would rather be called an idealist than a practical person."

# These agreement questions on the survey are mostly social and religious in nature, focusing on what the respondent believes about society, the place of religion within the world, and her own role.
# I do want to point out that this survey was fielded during the 1960s and there are some words used, for example for people of color, that are not used in professional or academic environments today. This is a historical dataset that we can approach and understand as centered in its context.

# In this case study, we are going to spend a bit more effort on exploratory data analysis. You are going to create a tidy version of the survey data with one row per observation.
# In the original way the data is structured, there is one row per respondent with a separate column for each answer. After you tidy the data, using the function pivot_longer(), you will have one row for each combination of respondent and question, a much longer and skinnier dataset. This kind of tidy data structure is well suited to exploratory data analysis.

sisters67 %>%
  select(-sister) %>%
  pivot_longer(-age, names_to = "question", values_to = "rating")

# The first step before you start modeling is to explore your data, and we are going to spend a little more time on this step in this last case study. To start with, check out the distribution of ages for the respondents in this survey. 📊 (Keep in mind throughout this case study that the data you have in your environment is one quarter of the real survey data.)
# 
# Instructions
# - Call glimpse() on sisters67 to take a look at the structure of the data. Notice how many columns there are, and what their characteristics are.
# - Plot a histogram of age.

sisters67 <- read_csv("data/sisters.csv")

# View sisters67
glimpse(___)

# Plot the histogram
ggplot(sisters67, aes(x = ___)) +
  ___(binwidth = 10)

sisters67 <- read_csv("data/sisters.csv")

# View sisters67
glimpse(sisters67)

# Plot the histogram
ggplot(sisters67, aes(x = age)) +
  geom_histogram(binwidth = 10)

# We have lots of survey respondents in the middle age ranges, and just a few respondents at the extremes.

# Embracing tidy data principles is a powerful option for exploratory data analysis. When your data is tidy, you can quickly iterate in getting to know your data better and making exploratory plots. Let’s transform this wide data set into a tidy data frame with one observation per row, and then check out some characteristics of this subset of the original survey.
# Note: There is a column called sister in this dataset that is an identifier for each survey respondent. We are removing this column in the exercise using select().
# 
# Instructions
# - Use the pivot_longer() function to transform the wide data set with each survey question in a separate column to a narrow, tidy data set with each survey question in a separate row.
# - View the structure of this tidy data set using glimpse().

sisters67 <- read_csv("data/sisters.csv")

# Print the structure of sisters67
glimpse(sisters67)

# Tidy the data set
tidy_sisters <- sisters67 %>%
  select(-sister) %>%
  ___(-age, names_to = "question", values_to = "rating")
# Print the structure of tidy_sisters
___

sisters67 <- read_csv("data/sisters.csv")

# Print the structure of sisters67
glimpse(sisters67)

# Tidy the data set
tidy_sisters <- sisters67 %>%
  select(-sister) %>%
  pivot_longer(-age, names_to = "question", values_to = "rating")

# Print the structure of tidy_sisters
str(tidy_sisters)


# Next look at question agreement overall.

# Instructions
# - Group by age and summarize the rating column to see how the overall agreement with all questions varied by age.
# - Count the rating column to check out how many respondents agreed or disagreed overall.

library(tidyverse)
tidy_sisters <- readRDS("data/tidy_sisters.rds")

# Overall agreement with all questions by age
tidy_sisters %>%
  ___(age) %>%
  ___(rating = mean(rating, na.rm = TRUE))

# Number of respondents agreed or disagreed overall
tidy_sisters %>%
  ___(rating)


library(tidyverse)
tidy_sisters <- readRDS("data/tidy_sisters.rds")

# Overall agreement with all questions by age
tidy_sisters %>%
  group_by(age) %>%
  summarise(rating = mean(rating, na.rm = TRUE))

# Number of respondents agreed or disagreed overall
tidy_sisters %>%
  count(rating)


# This tidy version of the data set is convenient for deep dives into exploratory data analysis.

## Exploratory data analysis with tidy data
# You just created a tidy version of this survey data, which allows you to quickly ask and answer many different kinds of questions in exploratory data analysis.

# Counting agreement
tidy_sisters %>%
  count(rating)

# A tibble: 5 x 2
# rating      n
# <dbl>   <int>
# 1     1 1303555
# 2     2  844311
# 3     3  645401
# 4     4 1108859
# 5     5 1110154

# You can easily see how many times survey respondents chose each option on the agreement scale with just one call to dplyr's count() function.
# We can see here that 1 was the most commonly chosen option; this corresponds to "disagree very much". The options for disagree very much, agree very much, and agree somewhat are chosen much more often than the other two.

## Overall agreement with age
tidy_sisters %>%
  group_by(age) %>%
  summarise(rating = mean(rating))

# A tibble: 9 x 2
# age rating
# <dbl>  <dbl>
# 1    20   2.86
# 2    30   2.81
# 3    40   2.83
# 4    50   2.94
# 5    60   3.10
# 6    70   3.26
# 7    80   3.42
# 8    90   3.51
# 9   100   3.60

# You can also check out how the overall answers to all survey questions vary with age. There were more statements on the survey that older respondents agreed with than statements younger respondents agreed with.

## Agreement on questions by age
tidy_sisters %>%
  filter(question %in% paste0("v", 153:170)) %>%
  group_by(question, rating) %>%
  summarise(age = mean(age)) %>%
  ggplot(aes(rating, age, color = question)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point(size = 2) +
  facet_wrap(~question)

# We can go a few steps further and dig into how the answers to individual questions depend on age. This code first filters to a subset of questions on the survey, groups by these survey questions and the possible answers to them, and then calculates the mean age for each possible answer to each of the questions we're considering.
# This is now getting closer to what we really care about and we can then pipe it to ggplot2 to make an exploratory plot to visualize these relationships. 📊

# These are the relationships that we want to build a machine learning model to understand and use for prediction. Exploratory data analysis is an important first step so that you as a machine learning practitioner understand something about your data and what your model will be capable of learning.
# Once you have done that important exploration, you can build a very simple model and then create training and testing sets. 💫

# In this case study, you are going to split the original data into three sets:
# - training,
# - validation, and
# - test sets.
# You've already used training and test sets throughout this course, and in this last case study we're going to talk about how to use a validation set to choose a model. 🧐

# In previous case studies, we used resampling to choose a model. Resampling lets you use your training data to create simulated datasets. These simulated datasets can help you learn which model is best without relying on performance metrics for the training set as a whole (which are overly optimistic) or the testing set (which can only be used one time at the very end of your analysis.)
# If you have enough data, you may not need resampling at all and instead can divide your data into a training set, a validation set, and a testing set. Instead of computing performance metrics on resampled datasets, you can compute them for the validation set. This survey of nuns is quite large so we can create a validation set to use for choosing a model. You can think of a validation set as a single resample.

# To split your data into three sets (training, validation, and testing), first make an initial_split() to split off your testing set from the rest. Then use the function validation_split() to create what you can think of as a single resample of that data.

set.seed(123)
sisters_splits <- initial_split(sisters_select, strata = age)

sisters_other <- training(sisters_splits)
sisters_test <- testing(sisters_splits)

set.seed(123)
sisters_val <- validation_split(sisters_other, strata = age)

sisters_val

## Visualize agreement with age
# The tidied version of the survey data that you constructed is available in your environment. You have many options at your fingertips with this tidy data now. Make a plot that shows how agreement on a subset of the questions changes with age. 📉 In this exercise, we are using filter() to subset the data to just a subset of the questions on the survey to look at.

# Instructions
# - Group by two variables, question and rating, so you can calculate an average age for each answer to each question.
# - Summarize for each grouping to find an average age.
# - Choose the correct geom to make a line plot.

library(tidyverse)
tidy_sisters <- readRDS("data/tidy_sisters.rds")

# Visualize agreement with age
tidy_sisters %>%
  filter(question %in% paste0("v", 153:170)) %>%
  ___(question, rating) %>%
  ___(age = mean(age, na.rm = TRUE)) %>%
  ggplot(aes(rating, age, color = question)) +
  ___(show.legend = FALSE) +
  facet_wrap(~question, nrow = 3)

# Visualize agreement with age
tidy_sisters %>%
  filter(question %in% paste0("v", 153:170)) %>%
  group_by(question, rating) %>%
  summarise(age = mean(age, na.rm = TRUE)) %>%
  ggplot(aes(rating, age, color = question)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~question, nrow = 3)

# You can see in this plot how some responses exhibit more agreement with age, some exhibit less disagreement with age, and some do not show any relationship.


## Splitting the data

# It’s time to split your data into different sets now. You’ve done this three times already in this course, but in this last case study we are also going to create a validation set. Using a validation set is a good option when you have enough data (otherwise, you can use resampling).

# Instructions
# Create two data partitions:
# - Specify one to split between testing and everything else.
# - Specify another one to split between validation and training.

library(tidyverse)
library(tidymodels)
sisters_select <- read_csv("data/sisters.csv") %>%
  select(-sister)

# Split off the testing set
set.seed(123)
sisters_split <- ___(sisters_select, strata = age)

sisters_other <- training(___)
sisters_test <- testing(___)

# Create the validation split
set.seed(123)
sisters_val <- ___(sisters_other, strata = age)

glimpse(sisters_val)


library(tidymodels)
sisters_select <- read_csv("data/sisters.csv") %>%
  select(-sister)

# Split off the testing set
set.seed(123)
sisters_split <- initial_split(sisters_select, strata = age)

sisters_other <- training(sisters_split)
sisters_test <- testing(sisters_split)

# Create the validation split
set.seed(123)
sisters_val <- validation_split(sisters_other, strata = age)

glimpse(sisters_val)

# You have now created training, validation, and testing sets

# This new validation set you just created will be used to…
# - train your models.
# - compare models you have trained and choose which one to use.
# - do the final evaluation step where you estimate the performance of your model on new data.

## Tune model hyperparameters
# You have prepared training, validation, and test sets and now it's time to build predictive models.

# In this last case study, you are going to work with model hyperparameters for the first time in this course. Some model parameters cannot be learned directly from a dataset during model training; these kinds of parameters are called hyperparameters. 💥 Some examples of hyperparameters include the number of predictors that are sampled at splits in a tree-based model (we call this mtry in tidymodels) or the learning rate in a boosted tree model (we call this learn_rate).
# Instead of learning these kinds of hyperparameters during model training, we can estimate the best values for these values by training many models on a resampled data set (like the validation set you just created) and measuring how well all these models perform. This process is called tuning.
# You can identify which parameters to tune() in a model specification as shown here. Let's build a decision tree model to predict age for our nuns, and tune the cost complexity and the maximum tree depth

tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

# Model hyperparameters aren't the only things you can tune. You can also tune steps in your preprocessing pipeline. This recipe has two steps:
# - First, this recipe centers and scales all those numeric predictors we have in this dataset, cataloging the nuns' responses to the survey questions.
# - Second, this recipe implements principal component analysis on these same predictors. Except... this recipe identifies that we want to implement PCA and we aren't sure how many predictors we should use. We want to choose the best 🏆 number of predictors.

## Preprocessing and tuning
sisters_recipe <- recipe(age ~ ., data = sisters_other) %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

## Grid of tuning parameters
# You have a couple of options for how to choose which possible values for the tuning parameters to try. One option is to set up a grid of possible parameter values.
# Here, we are using default ranges for cost complexity and tree depth, and we are going to try 3 to 12 principal components. When we set levels = 5, we are saying we want five levels for each parameter, which means there will be 125 (5 * 5 * 5) total models to try.
# You can use the function tune_grid() to fit all these models; you can tune either a workflow or a model specification with a set of resampled data, such as the validation set you created (i.e. a single resample).

grid_regular(num_comp(c(3, 12)),
             cost_complexity(),
             tree_depth(),
             levels = 5)

# A tibble: 125 x 3
#    num_comp cost_complexity tree_depth
# <int>           <dbl>      <int>
# 1         3    0.0000000001          1
# 2         5    0.0000000001          1
# 3         7    0.0000000001          1
# 4         9    0.0000000001          1
# 5        12    0.0000000001          1
# 6         3    0.0000000178          1
# 7         5    0.0000000178          1
# 8         7    0.0000000178          1
# 9         9    0.0000000178          1
# 10       12    0.0000000178          1

# You train these 125 possible models on the training data and use the validation data to compare all the results in terms of performance. We won't use the testing data until the very end of our modeling process, when we use it to estimate how our model will perform on new data.

# For some modeling use cases, an approach with three data partitions is overkill, perhaps a bit too much, but if you have enough data that you can use some of these powerful machine learning algorithms or techniques, the danger you face is underestimating your uncertainty for new data if you estimate it with data that you used to pick a model.
# To get a reliable estimate from tuning, for example, you need to use another heldout dataset for assessing the models, either a validation set or a set of simulated datasets created through resampling.

# This dataset of extensive survey responses from Catholic nuns in the 1960s is a great demonstration of all of these issues. You will use your validation set to find which values of the parameters (cost complexity, tree depth, and number of principal components) result in the highest R-squared and lowest RMSE. Notice here that we get the best results with a tree depth of 4 and 5 principal components.
# As you work through the final set of exercises, you will see all of this come together, along with all the other practical predictive modeling skills we've explored in this course.

# It’s time to build a modeling workflow() for this last dataset. We aren’t going to fit this dataset just once, but instead many times! We are going to use this workflow() to tune hyperparameters both in our model specification and our preprocessing recipe.

# Instructions
# Let’s start with our preprocessing tuning.
# - Add two preprocessing steps to this recipe, first to normalize and them to implement PCA.
# - Specify that we want to tune() the number of principal components.


library(tidymodels)
sisters_other <- readRDS("data/c4_other.rds")

sisters_recipe <- recipe(age ~ ., data = sisters_other) %>% 
  ___(all_predictors()) %>%
  ___(all_predictors(), num_comp = ___)

sisters_recipe

library(tidymodels)
sisters_other <- readRDS("data/c4_other.rds")

sisters_recipe <- recipe(age ~ ., data = sisters_other) %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

sisters_recipe

# Next let’s build our model specification with tuning.

# Instructions
# - Start by specifying that we want to train a decision_tree() model.
# - Add the two parameters we want to tune, cost complexity and tree depth.

library(tidymodels)

tree_spec <- ___(
  ___ = tune(),
  ___ = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_spec

library(tidymodels)

tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_spec

# Excellent! This model specification is ready for tuning.

# Finally, let’s put our recipe and model specification together in a workflow(), for ease of use.

# Instructions
# - First set up a workflow() object.
# - Add the recipe to the workflow().
# - Add the model to the workflow()

library(tidymodels)
sisters_other <- readRDS("data/c4_other.rds")

sisters_recipe <- recipe(age ~ ., data = sisters_other) %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_wf <- ___ %>%
  ___(sisters_recipe) %>%
  ___(tree_spec)

tree_wf

tree_wf <- workflow() %>%
  add_recipe(sisters_recipe) %>%
  add_model(tree_spec)

tree_wf

# Your workflow is ready to go!

# Let’s create a grid! 💃To tune our hyperparameters, we need a set of possible values for each parameter to try. In this case study, we’ll work through a regular grid of hyperparameter values.

# Instructions
# - Use the function grid_regular() to create a grid of tuning parameters.
# - Add the function for the tree depth tuning parameter, after the cost complexity tuning parameter function.

library(tidymodels)
tree_grid <- ___(num_comp(c(3, 12)),
                 cost_complexity(),
                 ___,
                 levels = 5)

glimpse(tree_grid)


library(tidymodels)
tree_grid <- grid_regular(num_comp(c(3, 12)),
                          cost_complexity(),
                          tree_depth(),
                          levels = 5)

glimpse(tree_grid)

# Rows: 125
# Columns: 3
# $ num_comp        <int> 3, 5, 7, 9, 12, 3, 5, 7, 9, 12, 3, 5, 7, 9, 12, 3, 5, …
# $ cost_complexity <dbl> 1.000000e-10, 1.000000e-10, 1.000000e-10, 1.000000e-10…
# $ tree_depth      <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …

# In this case study, you will fit a whole set of models for a grid of hyperparameters like this.

# It’s time to finally tune! The recipe, model, workflow, and grid are built here for you, and now you can put them together to find out which combination of parameters results in the best performance. (There is a smaller grid created here so the tuning will evaluate faster.)

# Instructions
# - Use the function tune_grid() to tune your model.
# - For the first argument, add your tuneable workflow.
# - For the third argument, add the grid of possible parameters.

library(tidymodels)

sisters_other <- readRDS("data/c4_other.rds")
sisters_val <- readRDS("data/c4_val.rds")

sisters_recipe <- recipe(age ~ ., data = sisters_other) %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_wf <- workflow() %>%
  add_recipe(sisters_recipe) %>%
  add_model(tree_spec)

tree_grid <- grid_regular(num_comp(c(3, 12)),
                          cost_complexity(),
                          tree_depth(),
                          levels = 2)
set.seed(123)
tree_res <- ___(
  ___,
  resamples = sisters_val,
  grid = ___
)

glimpse(tree_res)

set.seed(123)
tree_res <- tune_grid(
  tree_wf,
  resamples = sisters_val,
  grid = tree_grid
)

# Rows: 1
# Columns: 4
# $ splits   <named list> [<val_split[10847 x 3613 x 14460 x 66]>]
# $ id       <chr> "validation"
# $ .metrics <list> [<tbl_df[16 x 7]>]
# $ .notes   <list> [<tbl_df[0 x 1]>]
# Nice tuning results! 👍

## Visualize tuning results
# Now that you have trained models for many possible tuning parameters, let’s explore the results.
# (The results available in your environment were trained over a larger grid of tuning parameters.)

# Instructions
# - As a first step, use the function collect_metrics() to extract the performance metrics from the tuning results.
# - In the call to aes(), put cost_complexity on the x-axis and assign tree_depth to the color aesthetic.

library(tidyverse)
library(tidymodels)

tree_res <- readRDS("data/c4_tree_res.rds")

tree_metrics <- tree_res %>%
  ___ 

glimpse(tree_metrics)

tree_metrics %>%
  mutate(tree_depth = factor(tree_depth),
         num_comp = paste("num_comp =", num_comp),
         num_comp = fct_inorder(num_comp)) %>%
  ggplot(aes(___, mean, color = ___)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  scale_x_log10() +
  facet_grid(.metric ~ num_comp, scales = "free")

tree_res <- readRDS("data/c4_tree_res.rds")

tree_metrics <- tree_res %>%
  collect_metrics() 

glimpse(tree_metrics)

tree_metrics %>%
  mutate(tree_depth = factor(tree_depth),
         num_comp = paste("num_comp =", num_comp),
         num_comp = fct_inorder(num_comp)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  scale_x_log10() +
  facet_grid(.metric ~ num_comp, scales = "free")

# Rows: 250
# Columns: 8
# $ cost_complexity <dbl> 1e-10, 1e-10, 1e-10, 1e-10, 1e-10, 1e-10, 1e-10, 1e-10…
# $ tree_depth      <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, …
# $ num_comp        <int> 3, 3, 5, 5, 7, 7, 9, 9, 12, 12, 3, 3, 5, 5, 7, 7, 9, 9…
# $ .metric         <chr> "rmse", "rsq", "rmse", "rsq", "rmse", "rsq", "rmse", "…
# $ .estimator      <chr> "standard", "standard", "standard", "standard", "stand…
# $ mean            <dbl> 14.6920952, 0.1547873, 14.6920952, 0.1547873, 14.69209…
# $ n               <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
# $ std_err         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

# Notice that the tree depth of 4 looks the best.

# You just visualized the tuning results, but you can also select the best set of tuning parameters and update your workflow() with these values.

# Instructions
# - Use the function select_best() to extract the hyperparameters with the lowest RMSE from the tuning results.
# - Pipe the original workflow object to finalize_workflow() with that best decision tree as an argument, to update it.

library(tidyverse)
library(tidymodels)

tree_res <- readRDS("data/c4_tree_res.rds")
sisters_other <- readRDS("data/c4_other.rds")

sisters_recipe <- recipe(age ~ ., data = sisters_other) %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_wf <- workflow() %>%
  add_recipe(sisters_recipe) %>%
  add_model(tree_spec)

tree_wf


best_tree <- tree_res %>%
  ___("rmse")

best_tree

final_wf <- tree_wf %>% 
  ___(best_tree)

final_wf



best_tree <- tree_res %>%
  select_best("rmse")

best_tree

final_wf <- tree_wf %>% 
  finalize_workflow(best_tree)

final_wf

# This finalized workflow can be used to predict on new data.

# We haven’t touched the testing data throughout this analysis, but here at the very end, we can come back to it and estimate how well our model will perform on new data. If all has gone well, our performance metrics such as RMSE will be about the same as from the validation set, indicating that we did not overfit during our tuning procedure. Let’s use the last_fit() function to fit to the entire training set and evaluate on the testing set.

# Instructions
# - Fit to the training set and evaluate on the testing set using last_fit().
# - Access the performance metrics for the testing set using collect_metrics().

library(tidyverse)
library(tidymodels)

sisters_select <- read_csv("data/sisters.csv") %>%
  select(-sister)
final_wf <- readRDS("data/c4_final_wf.rds")

set.seed(123)
sisters_split <- initial_split(sisters_select, strata = age)

final_tree <- final_wf %>%
  ___(sisters_split) 

final_tree %>%
  ___


final_tree <- final_wf %>%
  last_fit(sisters_split) 

final_tree %>%
  collect_metrics()

# You tuned model hyperparameters for a decision tree (cost complexity and tree depth) along with the number of components to use in PCA for data preprocessing. The resulting decision tree (shown here) exhibits some sensible behavior, with the first principal component being the variable for the first split, etc.

## Predicting age

# n= 14460 
# 
# node), split, n, deviance, yval
# * denotes terminal node
# 
# 1) root 14460 3691121.000 48.11134  
#   2) PC1< 1.744274 10223 1988405.000 43.87362  
#     4) PC1< -0.3827951 7068 1163392.000 41.99774  
#        8) PC3< 1.289199 5743  864820.600 41.02037  
#          16) PC2>=-6.154196 5715  840659.100 40.90114 *
#          17) PC2< -6.154196 28    7496.429 65.35714 *
#        9) PC3>=1.289199 1325  269307.500 46.23396  
#          18) PC3< 2.638801 1079  211851.500 45.33828 *
#          19) PC3>=2.638801 246   52793.500 50.16260 *
#     5) PC1>=-0.3827951 3155  744421.700 48.07607  
#        10) PC3< 0.2146542 1643  342640.400 45.51430  
#          20) PC4>=-3.03208 1629  332107.700 45.36525 *
#          21) PC4< -3.03208 14    6285.714 62.85714 *
#        11) PC3>=0.2146542 1512  379282.300 50.85979  
#          22) PC1< 0.5623101 693  160369.100 48.51371 *
#          23) PC1>=0.5623101 819  211871.300 52.84493 *
#   3) PC1>=1.744274 4237 1076169.000 58.33609  
#      6) PC1< 4.98542 3009  761850.300 55.43702

# You tuned model hyperparameters for a decision tree (cost complexity and tree depth) along with the number of components to use in PCA for data preprocessing. The resulting decision tree (shown here) exhibits some sensible behavior, with the first principal component being the variable for the first split, etc.

final_tree <- final_wf %>%
  last_fit(sisters_split) 

final_tree %>%
  collect_metrics()

# A tibble: 2 x 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
# 1 rmse    standard      14.0  
# 2 rsq     standard       0.236

# The performance metrics you achieved on the testing set indicate that you did not overfit during the tuning process.

# You divided your data into three subsets: a training set to train your model, a validation set to tune hyperparameters, and then a testing set to evaluate the performance of your model and estimate how it will do with new data.
# You can also use a validation set to choose between different models.
# - Build your model with your training data
# - Choose your model and model hyperparameters with your validation data
# - Evaluate your model with your testing data


## Diverse data, powerful tools

# - Fuel efficiency of cars 🚗
# - Developers working remotely in the Stack Overflow survey 💻
# - Voter turnout in 2016 🗳
# - Catholic nuns' ages based on beliefs and attitudes ⛪

# This analysis was just the final example in the series of predictive projects you approached in this course.
# You learned how to go from raw data to exploring that data to training models to evaluating those models using the tidymodels metapackage and important tools for exploratory data analysis from the tidyverse ecosystem, like dplyr and ggplot2.

## Practical machine learning
# - Dealing with class imbalance
# - Measuring performance with resampling (bootstrap, cross-validation)
# - Tuning hyperparameters
# - Try out multiple modeling approaches for each new problem
# - Overall, gradient tree boosting and random forest (https://arxiv.org/abs/1708.05070v1) perform well
# - Never skip exploratory data analysis ✅

# And perhaps most importantly, never skip exploratory data analysis when you build machine learning models. It is time well spent, because when you understand a data set better, you can do a better job of building accurate models that perform better.

# In this course, I've chosen to spend our energy and time on some of the issues that I have found to be the most important and most impactful from my real world, practical predictive modeling experience. These include knowing what to do when you are faced with class imbalance, having some options in your toolkit for resampling approaches, and optimizing model hyperparametrs.

# To keep going in your machine learning journey, check out the resources at tidymodels.org (https://www.tidymodels.org/). This site is a central location for resources and documentation for tidymodels packages, and there is a ton to explore and learn. 🚀
# There are five articles at Get Started (https://www.tidymodels.org/start/) that guide you through what you need to know to use tidymodels, and many more resources at Learn (https://www.tidymodels.org/learn/).

# The high level takeaways that you should remember from this course are first, that each time you have a new predictive modeling problem you are working on, you need to try out multiple different kinds of models. You don't know ahead of time which kind of model is going to perform best. This paper linked on this slide uses some super interesting analysis to show that most often, the two kinds of models that perform best are gradient tree boosting and random forest.
# However, depending on how much data you have and the specifics of your problem, that may not be true, so you have to try it for yourself. Also, start with a simple model to compare to.









