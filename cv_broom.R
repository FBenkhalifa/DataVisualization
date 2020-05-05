library(rsample)
library(combinat)
library(furrr)
library(stringi)
ls(package:rsample)

h <- vfold_cv(mydata, v = 10)
log_fits <- folds$splits %>% 
  map(analysis) %>% 
  map(glm, formula = Survived ~ ., family = binomial(link = "logit"))

mydata_rec <-
  recipe(Survived ~ ., data = mydata) %>% 
  step_dummy(all_predictors(), one_hot = TRUE) %>% 
  step_interact(terms = ~ starts_with("Sex"):starts_with("Age")) %>% 
  step_zv(all_predictors()) %>% 
  prep()

cv_log_reg <- function(.v = 10, .repeats = 1, .vars, .mydata){

    # 1 Generate cv with .v folds and .repeats repeats
  mydata_reduced <- .mydata %>% select(Survived, all_of(.vars))
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
  
  # 7 Return the folds object
  return(bind_cols(folds = tibble(lst(folds)), cv_res) %>% rename(folds = `lst(folds)`))
}

combn(c(1,2), 5)
permn(c(1,2,3))

all_vars <- mydata %>% colnames %>% .[-c(1:5)]

grid <- expand.grid(rep(list(0:1), length(all_vars))) %>% 
  set_names(all_vars) %>% 
  imap(~ifelse(.x == 1, .y, "")) %>% 
  do.call(cbind, .) %>% 
  as_tibble() %>% 
  mutate(Class_X1st = "Class_X1st", Class_X2nd = "Class_X2nd", Class_X3rd = "Class_X3rd", Class_Crew = "Class_Crew") %>% 
  as.matrix() %>% 
  t() %>% 
  as_tibble()
  


cv_log_reg %>% debugonce()
cv_log_reg(.vars = c("Class_X1st", "Class_X2nd"), .mydata = mydata) 

plan(multiprocess)

model_metrics <- future_map_dfr(grid, ~cv_log_reg(.vars = stri_remove_empty(.), .mydata = mydata), .id = "fold", .progress = TRUE)

h <- mydata %>% colnames %>% .[1:5]

mydata %>% select(0,1, 0, 0, 1)

expand.grid(c("Class_X1st", "Class_X2nd", ))
cv_log_reg(.vars = c("Class_X1st", "Class_X2nd"), .mydata = mydata)

vars_list = list(f = c("Class_X1st", "Class_X2nd"), d = c("Class_X1st"))

vcs <- vars_list %>% map(~cv_log_reg(.vars = ., .mydata = mydata))
cvs <- vcs %>% 
  nest() %>% 
  bind_cols()

vars_list %>% map(paste, collapse = ", ")

h <- model_metrics %>% 
  map_dfr(pluck, "cv_res", .id = "model") %>% 
  pivot_wider(names_from = ".metric", values_from = "mean") %>% 
  arrange(desc(accuracy, kap))

library(pander)
model_metrics$V73$folds$glm_fits %>% pander
h$accuracy %>% unique
