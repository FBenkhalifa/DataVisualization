# Specify model
lm_titanic_model <- 
  logistic_reg(mode = "classification") %>%
  set_engine(
    "glm",
    family = "binomial"
  ) %>%
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
    train_rec,
    model = lm_titanic_model,
    resamples = titanic_cv,
    metrics = metrics_vals,
    control = ctrl
  )

grid_form

show_best(grid_form, metric = "accuracy", maximize = FALSE)

autoplot(grid_form, metric = "accuracy") 


tune_vars <-
  list(
    c(Class + Sex + Age"),
  c(Class + Age"),
    c(Class"),
  c(Class + Class*Sex + Class*Age")
  ) 

lm_titanic_model <- 
  logistic_reg(mode = "classification", penalty = tune(), mixture = tune()) %>% 
  set_engine("glm")# %>% 
# set_engine(formula = tune(), family = tune(), weights = tune()) %>% 
# fit(tune_grid[[2]], data = train)

predict(lm_titanic_model, test, type = "conf_int") %>% print(n = 1000)

y = train %>% select(Survived) %>% data.matrix() 
x = train %>% select(-Survived) %>% data.matrix()

lasso <- cv.glmnet(y = y, 
          x = x, 
          family = "binomial", 
          alpha = 1,
          type.measure = "class",
          type.logistic = "Newton",
          nfolds = 14,
          penalty.factor=c(rep(0, 4), rep(1, ncol(x) - 4)))

lasso %>% tidy
lasso$glmnet.fit %>% tidy %>% print(n = 2000)
plot(lasso)


lambda <- lasso$glmnet.fit %>% tidy %>% filter(dev.ratio== max(dev.ratio)) %>% pull(lambda)
coefficients <- coef(lasso, s = lambda) 
coefficients %>% tidy
# 3 Extract the lambda fpr which lasso has chosen two predictors
lambda_var <- lasso$glmnet.fit$lambda[lasso$glmnet.fit$df == 2]

# 4 Get the coefficients chosen for the corresponding lasso
  as_tibble(rownames = "websites") %>% filter(`1` > 0)
coefficients

          