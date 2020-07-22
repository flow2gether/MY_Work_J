library(tidymodels)  
library(tidyverse)
library(lubridate)
library(vip)      


Cancel<-
  read.csv("CANCEL_DATA_ALL_2.csv",header=TRUE) %>%
  mutate_if(is.character,as.factor) %>%
  mutate_at(c("DAY"),as.Date)%>% 
  filter(TARGET=='구제')
  as_tibble() 

Cancel_Pre<-
  read.csv("CANCEL_PREDICTION.csv",header=TRUE) %>%
  mutate_if(is.character,as.factor) %>%
  mutate_at(c("DAY"),as.Date)%>% 
  as_tibble()  

glimpse(Cancel)

Cancel %>%
  count(INTERVAL) %>%
  mutate(prop=n/sum(n))

set.seed(123)
splits<-initial_split(Cancel,strata=INTERVAL)

Cancel_other<-training(splits)
Cancel_test<-testing(splits)

set.seed(234)
val_set<-validation_split(Cancel_other,
                          strata=INTERVAL,
                          prop=0.8)


#RANDOM FOREST
cores <- parallel::detectCores()

rf_mod<-
  rand_forest(mtry=tune(),min_n=tune(),trees=1000) %>%
  set_engine("ranger",num.threads=cores) %>%
  set_mode("regression")

rf_recipe <-
  recipe(INTERVAL ~ ., data = Cancel_other) %>%
  step_date(DAY) %>%
  step_holiday(DAY) %>% 
  step_rm(DAY) 

rf_workflow<-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(rf_recipe)

set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE))


rf_res %>% 
  show_best(metric="rmse")

autoplot(rf_res)

rf_best<-
  rf_res %>%
  select_best(metric = "rmse")

rf_best<-
  rf_res %>%
  select_best(metric = "rsq")

rf_res %>% 
  collect_predictions()

rf_auc <-
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>%
  mutate(model="Rand Forest")

autoplot(rf_auc)



#최종
last_rf_mod <- 
  rand_forest(mtry = 10, min_n = 4, trees = 1000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("regression")


last_rf_workflow <-
  rf_workflow %>%
  update_model(last_rf_mod)

set.seed(345)

last_rf_fit<-
  last_rf_workflow %>% 
  last_fit(splits)

last_rf_fit %>% 
  collect_metrics()

last_rf_fit %>%
  pluck(".workflow",1) %>%
  pull_workflow_fit() %>%
  vip(num_features=15)

last_rf_fit %>% 
  collect_predictions() %>%
  roc_curve(INTERVAL,.pred) %>%
  autoplot()


#새로운 데이터로 예측하기
set.seed(5678)

rf_recipe_all <-
  recipe(INTERVAL ~ ., data=splits) %>%
  prep()

Cancel_traing<-rf_recipe_all %>% juice()

last_rf_mod %>% 
  fit(INTERVAL ~ .,data = Cancel_traing) %>% 
  predict(Cancel_Pre) 


#새로운 데이터로 예측하기
set.seed(5678)

rf_recipe_all <-
  recipe(INTERVAL ~ ., data=splits) %>%
  prep()

Cancel_traing<-rf_recipe_all %>% juice()

last_rf_mod %>% 
  fit(INTERVAL ~ .,data = Cancel_traing) %>% 
  predict(Cancel_Pre) %>% 
  bind_cols(Cancel_Pre)
