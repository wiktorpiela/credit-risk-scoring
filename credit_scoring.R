library("tidyverse")
library("tidymodels")
library("ggridges")
library("probably")

add_predictions <- function(main_df,model,data,name_1,name_0){
  
  preds <- predict(model, data, type = "prob") %>% 
    set_names(c(name_1,name_0))
  
  main_df <- add_column(main_df, preds)
  
  return(main_df)
}

credit <- read_csv("data/credit_risk_dataset.csv") %>% 
  distinct() %>% 
  filter(person_age<100,
         person_emp_length<100) %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(loan_percent_income = loan_amnt/person_income,
         cb_person_default_on_file = as.factor(ifelse(cb_person_default_on_file=="N",0,1)),
         loan_status = as.factor(loan_status),
         loan_status = fct_relevel(loan_status, "1")) #ref 

skimr::skim(credit)

# eda ---------------------------------------------------------------------
# select_if(credit, is.numeric) %>% 
#   cor(use="complete.obs")
# 
# 
# ggplot(credit[sample(nrow(credit),2000),],aes(person_age,cb_person_cred_hist_length))+
#   geom_jitter()
# 
# set.seed(123)
# credit[sample(nrow(credit),2000),] %>% 
#   mutate(loan_status = fct_recode(loan_status,
#                                   `Nie było opóźnień w spłatach`="0",
#                                   `Były opóźnienia w spłatach`="1")) %>%
#   ggplot(aes(loan_amnt,person_income,col=loan_status))+
#   geom_jitter()+
#   geom_smooth()+
#   theme_minimal()+
#   labs(x = "Kwota kredytu",
#        y = "Roczny przychód",
#        fill="Klienci, u których:")+
#   theme(legend.position = "bottom")
#   
# # oprocentowanie kredytów mocna uzależnione od loan grade
# ggplot(credit, aes(loan_int_rate,loan_grade,fill=stat(y)))+
#   geom_density_ridges_gradient()+
#   viridis::scale_fill_viridis()+
#   theme_minimal()+
#   theme(legend.position = "none")+
#   labs(y = "Grade",
#        x = "Oprocentowanie kredytu")
# 
# descr::crosstab(credit$loan_status,
#                 credit$loan_grade,
#                 prop.c = TRUE)
# credit %>% 
#   mutate(loan_status = fct_recode(loan_status,
#                                   `Nie było opóźnień w spłatach`="0",
#                                   `Były opóźnienia w spłatach`="1")) %>% 
#   ggplot(aes(loan_percent_income,fill=loan_status))+
#   geom_density(alpha=0.7)+
#   theme_minimal()+
#   labs(x = "Relacja kwota kredytu/roczny przychód",
#        y = "",
#        fill="Klienci, u których:")
# 
# cor(as.numeric(credit$cb_person_default_on_file),as.numeric(credit$loan_status))
# 
# descr::crosstab(credit$cb_person_default_on_file,
#                 credit$loan_status,
#                 prop.r = TRUE,
#                 plot=FALSE)
# 
# #loan intent a wiek
# ggplot(credit, aes(person_age,fill=loan_intent))+
#   geom_density()+
#   scale_x_log10()
# #  
# ggplot(credit, aes(loan_int_rate,fill=loan_intent))+
#   geom_density()+
#   scale_x_log10()
# #  
# ggplot(credit, aes(loan_intent, person_age, fill=loan_status))+
#   geom_boxplot()+
#   scale_y_log10()+
#   facet_wrap(vars(person_home_ownership))
# # 
# descr::crosstab(credit$loan_intent,
#                 credit$loan_grade,
#                 prop.c = TRUE)
# 
# descr::crosstab(credit$loan_status,
#                 credit$loan_grade,
#                 prop.c = TRUE)

# ggplot(credit, aes(person_emp_length,fill=person_home_ownership))+
#   geom_density()

credit %>% 
  mutate(loan_status = fct_recode(loan_status,
                                  `Klasa pozytywna` = "1",
                                  `Klasa negatywna` = "0")) %>% 
  ggplot(aes(loan_status, fill=loan_status))+geom_bar()+
  scale_y_continuous(labels = function(x) format(x, big.mark=" "))+
  labs(x = "",
       y = "Zliczenia")+
  scale_fill_manual(values = c("#377EB8","#E41A1C"))+
  theme_minimal()+
  theme(legend.position = "none")

# modelowanie ---------------------------------------------------------
set.seed(123)
credit_split <- initial_split(credit, prop = 0.8, strata = loan_status)
credit_train <- training(credit_split)
credit_test <- testing(credit_split)

model_train_performance <- tibble(
  actual = credit_train$loan_status
  )

model_test_performance <- tibble(
  actual = credit_test$loan_status
)

credit_recipe <- recipe(loan_status~.,credit_train) %>% 
  step_impute_bag(loan_int_rate)

# log reg  -----------------------------------------------------------------

glm_model <- logistic_reg()

glm_wflow <- workflow() %>%
  add_model(glm_model) %>% 
  add_recipe(credit_recipe)

glm_fit <- glm_wflow %>% 
  fit(credit_train)


model_train_performance <- add_predictions(model_train_performance,
                                           glm_fit,
                                           credit_train,
                                           "glm_pred_1",
                                           "glm_pred_0")

model_test_performance <- add_predictions(model_test_performance,
                                          glm_fit,
                                          credit_test,
                                          "glm_pred_1",
                                          "glm_pred_0")


# naive bayes -------------------------------------------------------------
library("discrim")

bayes_model <- naive_Bayes()

bayes_wflow <- workflow() %>%
  add_model(bayes_model) %>% 
  add_recipe(credit_recipe)

bayes_fit <- bayes_wflow %>% 
  fit(credit_train)

xx <- predict(bayes_fit, credit_train, type="prob")

model_train_performance <- add_predictions(model_train_performance,
                                           bayes_fit,
                                           credit_train,
                                           "bayes_pred_1",
                                           "bayes_pred_0")

model_test_performance <- add_predictions(model_test_performance,
                                          bayes_fit,
                                          credit_test,
                                          "bayes_pred_1",
                                          "bayes_pred_0")

# knn ---------------------------------------------------------------------
knn_model <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_recipe <- credit_recipe %>%
  step_normalize(all_numeric_predictors())

knn_wflow <- workflow() %>%
  add_model(knn_model) %>%
  add_recipe(knn_recipe)

knn_tune <- knn_wflow %>% tune_grid(
  resamples = vfold_cv(credit_train, v=5, strata=loan_status),
  grid = data.frame(neighbors = seq(55,80,2)),
  metrics = metric_set(roc_auc),
  control = control_grid(verbose = TRUE)
  )

collect_metrics(knn_tune)
autoplot(knn_tune)
show_best(knn_tune, metric = "roc_auc")
params <- select_best(knn_tune, metric = "roc_auc")

knn_fit <- knn_wflow %>%
  finalize_workflow(params) %>%
  fit(credit_train)


model_train_performance <- add_predictions(model_train_performance,
                                           knn_fit,
                                           credit_train,
                                           "knn_pred_1",
                                           "knn_pred_0")

model_test_performance <- add_predictions(model_test_performance,
                                          knn_fit,
                                          credit_test,
                                          "knn_pred_1",
                                          "knn_pred_0")

# drzewo decyzyjne --------------------------------------------------------

rpart_model <- decision_tree(cost_complexity = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

rpart_wflow <- workflow() %>%
  add_model(rpart_model) %>%
  add_recipe(credit_recipe)

rpart_tune <- rpart_wflow %>% tune_grid(
  resamples = vfold_cv(credit_test, v = 5, strata = loan_status),
  grid = 10,
  metrics = metric_set(roc_auc),
  control = control_grid(verbose = TRUE)
)

collect_metrics(rpart_tune)
autoplot(rpart_tune)
show_best(rpart_tune, metric = "roc_auc")
params <- select_best(rpart_tune, metric = "roc_auc")

rpart_fit <- rpart_wflow %>%
  finalize_workflow(params) %>%
  fit(credit_train)


model_train_performance <- add_predictions(model_train_performance,
                                           rpart_fit,
                                           credit_train,
                                           "rpart_pred_1",
                                           "rpart_pred_0")

model_test_performance <- add_predictions(model_test_performance,
                                          rpart_fit,
                                          credit_test,
                                          "rpart_pred_1",
                                          "rpart_pred_0")

# random forest -----------------------------------------------------------

ranger_model <- rand_forest(mtry = tune()) %>% 
  set_engine("ranger",
             importance = "impurity") %>% 
  set_mode("classification")

ranger_wflow <- workflow() %>%
  add_model(ranger_model) %>%
  add_recipe(credit_recipe)

ranger_tune <- ranger_wflow %>% tune_grid(
  resamples = vfold_cv(credit_train, v = 5, strata = loan_status),
  grid = 10,
  metrics = metric_set(roc_auc),
  control = control_grid(verbose = TRUE)
)

autoplot(ranger_tune)
show_best(ranger_tune, metric = "roc_auc")
params <- select_best(ranger_tune, metric = "roc_auc")

ranger_fit <- ranger_wflow %>% 
  finalize_workflow(params) %>%
  fit(credit_train)

model_train_performance <- add_predictions(model_train_performance,
                                           ranger_fit,
                                           credit_train,
                                           "ranger_pred_1",
                                           "ranger_pred_0")

model_test_performance <- add_predictions(model_test_performance,
                                          ranger_fit,
                                          credit_test,
                                          "ranger_pred_1",
                                          "ranger_pred_0")



# ewaluacja modeli --------------------------------------------------------


model_test_performance


roc_glm <- roc_curve(model_test_performance, actual, glm_pred_1) %>% 
  mutate(model = "glm")
roc_ranger <- roc_curve(model_test_performance, actual, ranger_pred_1) %>% 
  mutate(model = "ranger")
roc_knn <- roc_curve(model_test_performance, actual, knn_pred_1) %>% 
  mutate(model = "knn")

roc <- bind_rows(roc_glm,roc_ranger,roc_knn)

ggplot(roc, aes(x = 1 - specificity, y = sensitivity, col = model)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()


roc_auc(model_test_performance, actual, glm_pred_1) %>% mutate(model = "glm")
roc_auc(model_test_performance, actual, bayes_pred_1) %>% mutate(model = "naive_bayes")
roc_auc(model_test_performance, actual, knn_pred_1) %>% mutate(model = "knn")
roc_auc(model_test_performance, actual, rpart_pred_1) %>% mutate(model = "rpart")
roc_auc(model_test_performance, actual, ranger_pred_1) %>% mutate(model = "knn")


roc %>% 
  pivot_longer(cols = 2:3,
               names_to="measure",
               values_to = "value") %>% 
  ggplot(aes(.threshold, value, col=measure))+
  geom_line()+
  facet_wrap(vars(model))
























#to robic na testowym
# model_train_performance <- model_train_performance %>% 
#   mutate(glm_pred = as.factor(ifelse(.pred_1>0.5,1,0)),
#          bay_pred = as.factor(ifelse(bay_pred_1>0.5,1,0)),
#          glm_pred = fct_relevel(glm_pred,"1"),
#          bay_pred = fct_relevel(bay_pred,"1"))
# 
# precision(model_train_performance, truth = actual, estimate = glm_pred)
# precision(model_train_performance, truth = actual, estimate = bay_pred)
# precision(model_train_performance, truth = actual, estimate = knn_pred)
# 
# roc_auc(model_train_performance, truth = actual, estimate = .pred_1)
# roc_auc(model_train_performance, truth = actual, estimate = bay_pred_1)

