# Multivariate adaptive regression splines (MARS) tuning----

# Load package(s) ----
library(tidyverse)
library(tidymodels)

# load required objects ----
load('data_setup.rda')

# Define model ----
mars_model <- mars(mode = "classification",
                   num_terms = tune(),
                   prod_degree = tune()) %>% 
  set_engine("earth")

# set-up tuning grid ----

mars_param <- parameters(mars_model) %>% 
  update(num_terms = num_terms(range = c(1, 376)))

mars_param

# define tuning grid

mars_grid <- grid_regular(mars_param, levels = 5)

mars_grid

# workflow ----

mars_workflow <- workflow() %>% 
  add_model(mars_model) %>% 
  add_recipe(recipe)

mars_tuned <- mars_workflow %>% 
  tune_grid(resamples = folds, grid = mars_grid)

# Write out results & workflow

save(mars_workflow, 
     mars_tuned, file = "mars.rds") 