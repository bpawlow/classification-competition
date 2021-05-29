# Boosted tree tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)

# load required objects ----
load('data_setup.rda')

# Define model ----
bt_model <- boost_tree(mode = "classification",
                       min_n = tune(),
                       mtry = tune(),
                       learn_rate = tune()) %>% 
  set_engine("xgboost")

# set-up tuning grid ----

bt_param <- parameters(bt_model) %>% 
  update(mtry = mtry(range = c(1, 26)))

bt_param

# define tuning grid

bt_grid <- grid_regular(bt_param, levels = 5)

bt_grid

# workflow ----

bt_workflow <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(recipe)

bt_tuned <- bt_workflow %>% 
  tune_grid(resamples = folds, grid = bt_grid)

# Write out results & workflow

save(bt_workflow, 
     bt_tuned, file = "boosted_tree.rds") 