# Model Prediction Calculations and Assessment ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)

### MARS MODEL

#load necessary mode tuning and workflow info

load('data_setup.rda')
load(file = "mars.rds") 

mars_tuned %>% 
  collect_metrics() %>% 
  arrange(desc(mean)) %>% 
  filter(.metric == 'accuracy') %>% 
  head(1) # showing best parameters for MARS MODEL

mars_workflow_tuned <- mars_workflow %>% 
  finalize_workflow(select_best(mars_tuned, metric = "accuracy"))

mars_results <- fit(mars_workflow_tuned, training)

data_metric <- metric_set(accuracy)

submission <- predict(mars_results, new_data = testing) %>% 
  mutate(Id = 1:3818) %>% 
  rename(
    Category = .pred_class
  ) %>% 
  select(Id, Category)

#exporting to csv

write_csv(submission, 'submission.csv')


### BOOSTED TREE MODEL

#load necessary mode tuning and workflow info

load(file = "boosted_tree.rds") 

bt_tuned %>% 
  collect_metrics() %>% 
  arrange(desc(mean)) %>% 
  filter(.metric == 'accuracy') %>% 
  head(1) # showing best parameters for boosted tree

bt_workflow_tuned <- bt_workflow %>% 
  finalize_workflow(select_best(bt_tuned, metric = "accuracy"))

bt_results <- fit(bt_workflow_tuned, training)

submission_2 <- predict(bt_results, new_data = testing) %>% 
  mutate(Id = 1:3818) %>% 
  rename(
    Category = .pred_class
  ) %>% 
  select(Id, Category)

#exporting to csv

write_csv(submission_2, 'submission_2.csv')

  