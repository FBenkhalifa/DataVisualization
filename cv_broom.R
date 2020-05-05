


library(datasets)
# Data wrangling
library(tidyverse)
library(lubridate)
library(reshape)
library(stringi)

# Data viz
library(plotly)
library(scales)
library(ggthemes)
library(cowplot)
library(stargazer)
library(skimr)
library(ggmosaic)
library(plot3D)

# Data presentation
library(knitr)
library(stargazer)

# Data analysis
library(tidymodels)
library(broom)
library(glmnet)
library(rsample)
library(furrr)

# I Data preparation -------------------------------------------------------------------------

# Convert array to 2-dimensional format
mydata <- Titanic %>% as_tibble() %>% print(n=50)

# Convert all except n to factors which are easier to handle later on
mydata <- mydata %>% mutate_at(vars(-n), as.factor) 

# Untable the dbl which makes it easier to use with summarize functions
mydata <- untable(df = mydata %>% select(-n), 
                  num = mydata %>% pull(n))

# Retable for a quick check if n stays the same
mydata %>% count(Class, Sex, Age, Survived, .drop = FALSE)
ls(package:rsample)


mydata_rec <-
  recipe(Survived ~ ., data = mydata) %>% 
  step_dummy(all_predictors()) %>% 
  step_interact(terms = ~ starts_with("Sex"):starts_with("Age")) %>% 
  step_zv(all_predictors()) %>% 
  prep() %>% 
  juice()

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

# Debug the function if needed
cv_log_reg %>% debugonce()
cv_log_reg(.vars = c("Class_X3rd", "Class_X2nd", "Class_Crew"), .mydata = mydata_rec)

# Create the tuning grid
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
  
# Pass all the combinations through the CV function, 
plan(multiprocess) # Set up parallelization for faster results

model_metrics <- future_map_dfr(grid, ~cv_log_reg(.vars = stri_remove_empty(.), .mydata = mydata_rec), .id = "combination", .progress = TRUE) %>% 
  arrange(n_var)


# Create Table ------------------------------------------------------------

# Create table via stargazer, tehr
line_list <-  model_metrics %>% select( c(kap, accuracy)) %>% imap(~c(paste0("cv_",.y), round(.x, digits = 3))) %>% unlist

# Create the table
model_metrics %>% 
  pull(fit) %>% 
  exec(stargazer, ., title = "Results", align = TRUE, type = "text", table.placement="H",
       omit.stat=c("f", "ser"), order=c("Constant",
                                        "Class_X2nd",
                                        "Class_X3rd",
                                        "Class_Crew",
                                        "Sex_Male",
                                        "Age_Child",
                                        "Sex_Male_x_Age_Child"),
       add.lines= line_list)

  