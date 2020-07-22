library(tidymodels)  
library(tidyverse)
library(lubridate)
library(vip)      

# 분석 데이터 불러오기
Rescue_Analyze<-
  read.csv("Rescue Analyze2.csv",header=TRUE) %>%
  mutate_at(c("DAY","DAY_CHANGE"),as.Date) %>%  
  mutate(AMOUNT=as.integer(AMOUNT)) %>% 
  mutate_if(is.character,as.factor) %>%
  as_tibble() 

# EDA
glimpse(Rescue_Analyze)

Rescue_Analyze %>%
  count(TARGET) %>%
  mutate(prop=n/sum(n))

set.seed(123)
splits<-initial_split(Rescue_Analyze,strata=TARGET)

Cancel_other<-training(splits)
Cancel_test<-testing(splits)

set.seed(234)
val_set<-validation_split(Cancel_other,
                          strata=TARGET,
                          prop=0.8)

 
#RANDOM FOREST
cores <- parallel::detectCores()

rf_mod<-
  rand_forest(mtry=tune(),min_n=tune(),trees=1000) %>%
  set_engine("ranger",num.threads=cores) %>%
  set_mode("classification")

rf_recipe <-
  recipe(TARGET ~ ., data = Cancel_other) %>%
  step_date(DAY,DAY_CHANGE) %>%
  step_rm(DAY,DAY_CHANGE)

rf_workflow<-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(rf_recipe)

set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_res %>% 
  show_best(metric="roc_auc")

autoplot(rf_res)

rf_best<-
  rf_res %>%
  select_best(metric = "roc_auc")

rf_res %>% 
  collect_predictions()

rf_auc <-
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>%
  roc_curve(TARGET,.pred_구제, .pred_무리게재,.pred_정상이월) 

autoplot(rf_auc)
  

#최종
last_rf_mod <- 
  rand_forest(mtry = 13, min_n = 30, trees = 1000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("classification")


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
  roc_curve(TARGET,.pred_구제, .pred_무리게재,.pred_정상이월)  %>%
  autoplot()


#새로운 데이터로 예측하기
Cancel_Pre<-
  read.csv("CANCEL_PREDICTION_2.csv",header=TRUE) %>%
  mutate_if(is.character,as.factor) %>%
  mutate_at(c("DAY","DAY_CHANGE"),as.Date)%>% 
  as_tibble()  

set.seed(5678)

rf_recipe_all <-
  recipe(TARGET ~ ., data=splits) %>%
  step_date(DAY,DAY_CHANGE) %>%
  step_rm(DAY,DAY_CHANGE) %>%
  prep()

Cancel_ap<-bake(rf_recipe_all,new_data = Cancel_Pre)

rf_recipe_all <-
  recipe(TARGET ~ ., data=Rescue_Analyze) %>%
  step_date(DAY,DAY_CHANGE) %>%
  step_rm(DAY,DAY_CHANGE) %>%
  prep()

Cancel_juice<-rf_recipe_all %>% juice()
Cancel_ap<-bake(rf_recipe_all,new_data = Cancel_Pre)

last_rf_mod %>% 
  fit(TARGET ~ .,data = Cancel_juice ) %>% 
  predict(Cancel_ap) %>% bind_cols(Cancel_Pre)->RESULT


last_rf_mod %>% 
  fit(TARGET ~ .,data = Cancel_juice ) %>% 
  predict(Cancel_ap,type="prob") %>% bind_cols(Cancel_Pre)


#Classification models using a neural network

TEXT<-'[:blank:]|주식회사|유한회사|학교법인|[:punct:]|주'

Cancel_other_nn<-Cancel_other %>% 
  mutate_at(c("AGENCY","ADVERTISER","ADVERTISER_R"),str_replace_all,pattern=TEXT,replacement="") %>% 
  mutate_if(is.character,as.factor)

Cancel_Test_nn<-Cancel_test %>% 
  mutate_at(c("AGENCY","ADVERTISER","ADVERTISER_R"),str_replace_all,pattern=TEXT,replacement="") %>% 
  mutate_if(is.character,as.factor) 

nn_recipe <-
  recipe(TARGET ~ ., data = Cancel_other_nn) %>%
  step_date(DAY,DAY_CHANGE) %>%
  step_rm(DAY,DAY_CHANGE) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_BoxCox(all_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  prep(retain=T)

set.seed(69883)

nn_fit <- 
  mlp(epochs = 100, hidden_units = 5, dropout = 0.1) %>% 
  set_mode("classification") %>% 
  set_engine("keras",verbose=0) %>% 
  fit(TARGET ~ .,data = juice(nn_recipe))


nn_test<-bake(nn_recipe, new_data=Cancel_Test_nn)

nn_result <- Cancel_test %>% bind_cols(predict(nn_fit,new_data=nn_test), predict(nn_fit,new_data=nn_test, type="prob"))

nn_result %>%roc_auc(truth=TARGET,.pred_구제, .pred_무리게재,.pred_정상이월) 
nn_result %>%accuracy(truth=TARGET,.pred_class)
nn_result %>%conf_mat(truth=TARGET,.pred_class)
