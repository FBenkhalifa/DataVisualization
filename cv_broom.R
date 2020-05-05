library(rsample)
library(combinat)
library(furrr)
library(stringi)
library(pander)
ls(package:rsample)

h <- vfold_cv(mydata, v = 10)
log_fits <- folds$splits %>% 
  map(analysis) %>% 
  map(glm, formula = Survived ~ ., family = binomial(link = "logit"))

mydata_rec <-
  recipe(Survived ~ ., data = mydata) %>% 
  step_dummy(all_predictors()) %>% 
  step_interact(terms = ~ starts_with("Sex"):starts_with("Age")) %>% 
  step_zv(all_predictors()) %>% 
  prep() %>% 
  juice()

cv_log_reg <- function(.v = 10, .repeats = 1, .vars, .mydata){

  # 1 Generate cv with .v folds and .repeats repeats
  mydata_reduced <- .mydata %>% select(Survived, all_of(.vars))
  
  # 2 Create dummies and interaction terms
  # mydata_reduced <-
  #   recipe(Survived ~ ., data = mydata) %>% 
  #   step_dummy(all_predictors()) %>% 
  #   step_interact(terms = ~ starts_with("Sex"):starts_with("Age")) %>% 
  #   step_zv(all_predictors()) %>% 
  #   prep() %>% 
  #   juice() 
  
  folds <- vfold_cv(mydata_reduced, v = .v, repeats = .repeats)
  
  # 2 Define the log model as usual in the procedure of the package parsnip
  # log_model <- 
  #   logistic_reg() %>% 
  #   set_engine("glm")
  
  
  # 3 Fit the logistic model on each of the CV folds and receive .repeats * .v logistic
  #   model fits
  # folds$glm_fits <- map(.x = folds$splits,
  #                       ~ log_model %>%
  #                         fit_xy(y = select(analysis(.x), "Survived"),
  #                                x = select(analysis(.x), .vars))
  # )
  
  folds$glm_fits <- map(.x = folds$splits, 
                        ~ glm(Survived ~ ., data = analysis(.x), family = "binomial")
  )
  
  # 4 Define the metrics of interest as usual with the yardstick package
  class_metrics <- metric_set(accuracy, kap)
  
  # 5 Create a new column with the predicted and true values to feed into the
  #   *class_metrics* function
  folds$preds <- map2(.x = folds$glm_fits, 
                      .y = folds$splits,
                      ~ tibble(.pred_prob = predict(.x, .y %>% assessment(.), type = "response"),
                               .pred_class = ifelse(.pred_prob < 0.5, "No", "Yes") %>% as.factor(),
                               .truth = .y %>% assessment(.) %>% pull(Survived)
                               )
  )
                      
# 
# folds$preds <- map2(.x = folds$glm_fits, 
#                     .y = folds$splits,
#                     ~ predict(.x, .y %>% assessment(.)) %>% 
#                       add_column(., .truth = .y %>% assessment(.) %>% 
#                                    pull(Survived)))
  
  # 6 Run the class metrics function over the predictions of each fold
  folds$metrics <- map(.x = folds$preds, 
                       ~ class_metrics(., 
                                       truth = .truth, 
                                       estimate = .pred_class))
  
  # 7 Get the cross validation means
  cv_res <- folds$metrics %>% 
    exec(bind_rows, ., .id = "fold") %>% 
    group_by(.metric) %>% 
    summarize(mean = mean(.estimate)) %>% 
    column_to_rownames(var = ".metric") %>% 
    t() %>% 
    as_tibble()
  
  # 8 Fit the glm on the whole dataset
  cv_res$fit <- lst(glm(Survived ~ ., 
              data = mydata_reduced, 
              family = "binomial"))
  
  # 7 Return the folds object
  return(bind_cols(folds = tibble(lst(folds)), cv_res) %>% rename(folds = `lst(folds)`))
}

combn(c(1,2), 5)
permn(c(1,2,3))

all_vars <- mydata_rec %>% colnames %>% .[-c(1:4)]

grid <- expand.grid(rep(list(0:1), length(all_vars))) %>% 
  set_names(all_vars) %>% 
  imap(~ifelse(.x == 1, .y, "")) %>% 
  do.call(cbind, .) %>% 
  as_tibble() %>% 
  mutate(Class_X2nd = "Class_X2nd", Class_X3rd = "Class_X3rd", Class_Crew = "Class_Crew") %>% 
  as.matrix() %>% 
  t() %>% 
  as_tibble()
  


cv_log_reg %>% debugonce()
cv_log_reg(.vars = c("Class_X3rd", "Class_X2nd", "Class_Crew"), .mydata = mydata_rec) 

plan(multiprocess)

model_metrics <- future_map_dfr(grid, ~cv_log_reg(.vars = stri_remove_empty(.), .mydata = mydata_rec), .id = "combination", .progress = TRUE) %>% 
  arrange(desc(accuracy, kap))

out_of_sample <- model_metrics %>% select(combination, accuracy, kap) %>% pivot_longer(cols = -combination, names_to = "Regression") %>% pivot_wider(names_from = "combination", values_from = "value")
coef_tbl <- model_metrics$fit %>% 
  set_names(model_metrics$combination) %>% 
  map_dfr(tidy, .id = "combination") %>% 
  select(-std.error, -statistic) %>% 
  unite(col = "estimate", )
  pivot_longer(-c(combination, term), names_to = "metric", values_to = "value") %>% 
  pivot_wider(names_from = combination, values_from = value, values_fill = list("" = NA)) %>% 
  rename(Regression = names) %>% 
  add_row(out_of_sample) %>% 
  select(c("Regression", select(., -Regression) %>% map_int(~sum(!is.na(.))) %>% sort() %>% names())) %>% 
  mutate_at("Regression", ~ as.factor(.) %>% fct_relevel(.,
    c("(Intercept)",
      "Class_X2nd",
      "Class_X3rd",
      "Class_Crew",
      "Sex_Male",
      "Age_Child",
      "Sex_Male_x_Age_Child",
      "accuracy",
      "kap"))) %>% 
  arrange(Regression) 


  
  model_metrics %>% mutate_at("combination", ~as.factor(.) %>% fct_relevel(coef_tbl %>% select(., -Regression) %>% map_int(~sum(!is.na(.))) %>% sort() %>% names())) %>% 
    arrange(combination) %>% 
    pull(fit) %>% 
    exec(stargazer, ., title = "Results", align = TRUE, type = "text", table.placement="H",
         omit.stat=c("f", "ser"), order=c("Constant",
                                          "Class_X2nd",
                                          "Class_X3rd",
                                          "Class_Crew",
                                          "Sex_Male",
                                          "Age_Child",
                                          "Sex_Male_x_Age_Child"),
         add.lines= model_metrics %>% select( c(kap, accuracy)) %>% imap(~c(paste0("cv_",.y), round(.x, digits = 3))))
  
  
  model_metrics %>% select(c(kap, accuracy)) %>% imap(~c(paste0("cv_",.y), round(.x, digits = 3)))
  
  
model_metrics$fit[[1]] %>% tidy

coef_tbl <- model_metrics$fit %>% 
  set_names(model_metrics$combination) %>% 
  map_dfr(~coef(.) %>% tidy(.), .id = "combination") %>% 
  pivot_wider(names_from = combination, values_from = x) %>% 
  rename(Regression = names) %>% 
  add_row(out_of_sample) %>% 
  
coef_tbl %>% select(-Regression) %>% map_int(~sum(!is.na(.))) %>% sort() %>% names()

model_metrics$fit %>% 
  set_names(model_metrics$combination) %>% 
  map(~coef(.) %>% tidy(.)) %>% 
  t() %>% 
  as_data_frame() %>% 
  rownames_to_column(var = "Variable") %>% 
  as_tibble()

# Now fit the data on the whole dataset and not only the CVs
model_metrics$fits <- map(.x = grid, 
                          ~ glm(Survived ~ ., 
                                data =  select(mydata_rec, Survived, 
                                               all_of(stri_remove_empty(.))), 
                                               family = "binomial"))


model_metrics$fit %>% exec(stargazer, ., title = "Results", align = TRUE, out = "tbl.txt", 
                           table.placement="H",omit.stat=c("f", "ser"), order=c("Constant",
                                                                                "Class_X2nd",
                                                                                "Class_X3rd",
                                                                                "Class_Crew",
                                                                                "Sex_Male",
                                                                                "Age_Child",
                                                                                "Sex_Male_x_Age_Child"))

library(pander)
model_metrics$V73$folds$glm_fits %>% pander
h$accuracy %>% unique

model_metrics %>% mutate_at("combination", ~as.factor(.) %>% fct_relevel(coef_tbl %>% select(., -Regression) %>% map_int(~sum(!is.na(.))) %>% sort() %>% names())) %>% 
  arrange(combination) %>% 
  pull(fit) %>% 
  exec(stargazer, ., title = "Results", align = TRUE, out = "tbl.txt", 
       table.placement="H",omit.stat=c("f", "ser"), order=c("Constant",
                                                            "Class_X2nd",
                                                            "Class_X3rd",
                                                            "Class_Crew",
                                                            "Sex_Male",
                                                            "Age_Child",
                                                            "Sex_Male_x_Age_Child"))
