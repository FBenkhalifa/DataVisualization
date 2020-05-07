
# The CV_log_reg function is constructed to make the cross validation. Inputs are
# are .v for the number of folds, .repeats for the number of repeats (in case one
# wants to perform repeated cv) and the .mydata stands for the dummy encoded 
# data frame. .vars stands for a subset of all available variables in the .mydata
# framework. This is the parameter which will be tuned in the further progress.
# The output is a tibble which contains the following information:
# 
cv_log_reg <- function(.v = 10, .repeats = 1, .vars, .mydata){
  
  # 1 Create subset of original dataframe
  mydata_reduced <- .mydata %>% select(Survived, all_of(.vars))
  
  # 2  Generate cv with .v folds and .repeats repeats
  folds <- vfold_cv(mydata_reduced, v = .v, repeats = .repeats)
  
  # 3 Fit the logistic model on each of the CV folds and receive .repeats * .v logistic regressions
  
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
  
  # 9 Add the number of variables used in this model for better overview later
  cv_res$n_var <- cv_res$fit  %>% map_int(~coef(.) %>% length(.))
  
  # 10 Return the folds object
  return(bind_cols(folds = tibble(lst(folds)), cv_res) %>% rename(folds = `lst(folds)`))
}
