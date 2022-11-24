library(knitr)
library(rsample)
library(corrplot)
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(ggthemes)
library(recipes)
library(corrr)
library(parsnip)
library(workflows)
library(dplyr)
library(yardstick)
library(discrim)
library(klaR)
library(ISLR)
library(rpart.plot)
library(vip)
library(janitor)
library(randomForest)
library(xgboost)
library(ranger)
library(kknn)

setwd("~/Downloads/PSTAT231/Final_Project") # setting to final proj directory

load('data/eda_wta_df.RData')
load('data/clean_wta_df.RData')

# needed to drop country bc otherwise there were too many varables being introduced (46 countries)
total_wta_df_clean <- total_wta_df_clean %>% dplyr::select(-c(player_ioc, opp_ioc, p_set1, o_set1))
set.seed(789)  # setting a seed so the split is the same
wta_split <- total_wta_df_clean %>%
  initial_split(prop = 0.7, strata = "win")

wta_train <- training(wta_split) # training split (1056 rows, 32 columns)
wta_test <- testing(wta_split) # testing split(2462 rows, 32 columns)

wta_recipe <-   recipe(win ~ .,  data = wta_train) %>% 
  step_dummy(all_nominal_predictors()) %>%  # dummy predictor on categorical 
  step_zv(all_predictors()) %>% 
  step_center(all_predictors()) %>%   # standardizing our predictors
  step_scale(all_predictors()) 

summary(wta_recipe)

wta_folds <- vfold_cv(wta_train, v = 10, strata = win) 
save(wta_folds, wta_recipe, wta_train, wta_test, file = "data/model_setup.RData")

load('data/model_setup.RData')

# Grid
penalty_grid <- elas_grid <- grid_regular(penalty(), levels = 5)

# Log
log_model <- logistic_reg() %>% #elastic
  set_engine("glm") %>% 
  set_mode("classification")

wta_workflow_log <- workflow() %>% 
  add_model(log_model )  %>% 
  add_recipe(wta_recipe)

save(log_model , wta_workflow_log, file = "data/log_tune.rda")

# Lasso
lasso_grid <- grid_regular(penalty(), mixture(range = c(0, 1)), levels = 5)

lasso_model <- 
  logistic_reg(penalty = tune(), mixture = tune()) %>% # mixture = 1, lasso model
  set_mode("classification") %>% 
  set_engine("glmnet") 

wta_workflow_lasso <- workflow() %>% 
  add_model(lasso_model )  %>% 
  add_recipe(wta_recipe)

tune_lasso <- tune_grid(
  wta_workflow_lasso,
  resamples = wta_folds, 
  grid = lasso_grid
)

save(tune_lasso , wta_workflow_lasso, file = "data/lasso_tune.rda")

# DL 
DL_model <- parsnip::discrim_linear() %>%  # doesnt have tuning
  set_engine("MASS") %>% 
  set_mode("classification")

wta_workflow_DL<- workflow() %>% 
  add_model(DL_model)  %>% 
  add_recipe(wta_recipe)

save(wta_workflow_DL, file = "data/DL_tune.rda")

# DQ
DQ_model <- discrim_quad() %>%   # doesnt have tuning
  set_engine("MASS") %>% 
  set_mode("classification")

wta_workflow_DQ<- workflow() %>% 
  add_model(DQ_model)  %>% 
  add_recipe(wta_recipe)

save(wta_workflow_DQ, file = "data/DQ_tune.rda")

# Decisioin Trees

tree_model <- decision_tree(tree_depth = tune(), 
                            min_n = tune(), 
                            cost_complexity = tune()) %>%
  set_engine("rpart")  %>%
  set_mode("classification")

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          min_n(), 
                          levels = 3)

wta_workflow_tree <- workflow() %>%
  add_model(tree_model) %>%
  add_recipe(wta_recipe)

tune_tree <- wta_workflow_tree %>% tune_grid(grid = tree_grid,
  resamples = wta_folds)

save(tune_tree, wta_workflow_tree, file = "data/dec_tree_tune.rda")


# KNN
knn_model <- 
  nearest_neighbor(
    neighbors = tune(),
    mode = "classification") %>% 
  set_engine("kknn")

knn_workflow <- workflow() %>% 
  add_model(knn_model) %>% 
  add_recipe(wta_recipe)

knn_params <- hardhat::extract_parameter_set_dials(knn_model)
knn_grid <- grid_regular(knn_params, levels = 8)

knn_tune <- knn_workflow %>% 
  tune_grid(
    # what will it fit the workflow to
    resamples = wta_folds, 
    # how does it complete the models in those workflows
    grid = knn_grid)

save(knn_tune , knn_workflow, file = "data/knn_tune.rda")

# Boosted Tree
bt_model <- boost_tree(mode = "classification",
                       min_n = tune(),
                       mtry = tune(),
                       learn_rate = tune()) %>% 
  set_engine("xgboost")

bt_workflow <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(wta_recipe)

bt_params <- hardhat::extract_parameter_set_dials(bt_model) %>% 
  update(mtry = mtry(range= c(2, 25)),
         learn_rate = learn_rate(range = c(-5, 0.2))
  )

bt_grid <- grid_regular(bt_params, levels = 3)

bt_tune <- bt_workflow %>% 
  tune_grid(resamples = wta_folds, grid = bt_grid)

save(bt_tune, bt_workflow, file = "data/bt_tune.rda")

