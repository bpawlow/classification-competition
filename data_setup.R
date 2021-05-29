# Setting up ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(corrplot)
library("dplyr")

# Seed
set.seed(100)

## load data
training <- read_csv("train.csv") %>%
  janitor::clean_names() # clean up column names

testing <- read_csv("test.csv") %>%
  janitor::clean_names() %>% 
  as_tibble() %>%
  mutate_if(is_character, as_factor)

typeof(training) 

# Change outcome to factor

training <- training %>% 
  as_tibble() %>%
  mutate(hi_int_prncp_pd = as_factor(hi_int_prncp_pd)) %>% 
  mutate_if(is_character, as_factor) %>% 
  select(-c(id, last_credit_pull_d, earliest_cr_line, addr_state))

## Visualizing correlation
  
## Some stand-out correlation plots
  
ggplot(training, aes(x=loan_amnt, y=hi_int_prncp_pd)) + 
  geom_point() + 
  geom_smooth(method="loess")

ggplot(training, aes(x=out_prncp_inv, y=hi_int_prncp_pd)) + 
  geom_point(color = 'red') + 
  geom_smooth(method="loess") # out_prncp inv decreases target

ggplot(training, aes(x=int_rate, y=hi_int_prncp_pd)) + 
  geom_point(color = 'green') + 
  geom_smooth(method="loess") # out_prncp inv decreases target

# Modeling and Recipes 

#setting up recipe for regression model(s)

recipe <- recipe(hi_int_prncp_pd ~ .,
                    data = training) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors(), -all_outcomes())

prep(recipe) %>% 
  bake(new_data = NULL)

#V-fold cross-validation

folds <- vfold_cv(data = training, v = 5, repeats = 3, strata = hi_int_prncp_pd)

folds

#Training/tuning different models

## Saving objects for tuning in other R-scripts
save(folds, recipe,
     training, testing, file = "data_setup.rda")
