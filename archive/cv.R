library(parsnip)
library(tidymodels)
set.seed(100)

# Define test and training sample
split <- initial_split(mydata)
train_data <- training(split)
test_data <- testing(split)

titanic_cv <- vfold_cv(train_data, v = 10, repeats = 1)

# Preprocess the data
train_rec <-
  recipe(Survived ~ ., data = train_data) %>% 
  step_dummy(all_predictors(), one_hot = TRUE) %>% 
  # step_interact(-all_outcomes()) %>% 
  prep(training = train_data)

train <- train_rec %>% juice()

# Specify model
lm_titanic_model <- 
  # logistic_reg() %>% 
  logistic_reg(mode = "classification", penalty = tune()) %>%
  set_engine(
    "glmnet",
    family = "binomial",
    alpha = 1,
    penalty.factor = c(rep(0, 4), rep(1, ncol(x) - 4))
  )# %>%
# set_engine(formula = tune(), family = tune(), weights = tune()) %>% 
# fit(tune_grid[[2]], data = train)

# Create cross validation splits
 
# Define metric set with metrics used
metrics_vals <- metric_set(roc_auc, accuracy, kap)

# Define verbosity of the calls
ctrl <- control_grid(verbose = TRUE)


set.seed(35)
grid_form <-
  tune_grid(
    Survived ~ ., 
    train_rec %>% juice,
    model = lm_titanic_model,
    resamples = titanic_cv,
    metrics = metrics_vals,
    control = ctrl
    # grid = tibble(lambda = c(1:100))
  )

grid_form

show_best(grid_form, metric = "accuracy", maximize = FALSE)

autoplot(grid_form, metric = "accuracy") 


tune_vars <-
  list(
  c(Class + Sex + Age),
  c(Class + Age),
  c(Class),
  c(Class + Class*Sex + Class*Age)
  ) 

lm_titanic_model <- 
  logistic_reg(mode = "classification", penalty = tune(), mixture = tune()) %>% 
  set_engine("glm")# %>% 
  # set_engine(formula = tune(), family = tune(), weights = tune()) %>% 
  # fit(tune_grid[[2]], data = train)

predict(lm_titanic_model, test, type = "conf_int") %>% print(n = 1000)





