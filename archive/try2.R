library(mlbench)
data(Ionosphere)

Ionosphere <- Ionosphere %>% select(-V2)
svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")


iono_rec <-
  recipe(Class ~ ., data = Ionosphere)  %>%
  # In case V1 is has a single value sampled
  step_zv(all_predictors()) %>% 
  # convert it to a dummy variable
  step_dummy(V1) %>%
  # Scale it the same as the others
  step_range(matches("V1_"))

set.seed(4943)
iono_rs <- bootstraps(Ionosphere, times = 30)

roc_vals <- metric_set(roc_auc)

ctrl <- control_grid(verbose = FALSE)

set.seed(35)
grid_form <-
  
  tune::tune_grid(
    svm_mod, 
    Class ~ V1,
    resamples = iono_rs,
    metrics = roc_vals,
    control = ctrl
  )

grid_form



library(recipes)
library(rsample)
library(parsnip)

# ------------------------------------------------------------------------------

set.seed(6735)
folds <- vfold_cv(mtcars, v = 5)

# ------------------------------------------------------------------------------

# tuning recipe parameters:

spline_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_ns(disp, deg_free = tune("disp")) %>%
  step_ns(wt, deg_free = tune("wt"))

lin_mod <-
  linear_reg() %>%
  set_engine("lm")

# manually create a grid
spline_grid <- expand.grid(disp = 2:5, wt = 2:5)

# Warnings will occur from making spline terms on the holdout data that are
# extrapolations.
spline_res <-
  tune_grid(spline_rec, model = lin_mod, resamples = folds)
spline_res


show_best(spline_res, metric = "rmse", maximize = FALSE)

# ------------------------------------------------------------------------------

# tune model parameters only (example requires the `kernlab` package)

car_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors())

svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# Use a space-filling design with 7 points
set.seed(3254)
svm_res <- tune_grid(car_rec, model = svm_mod, resamples = folds, grid = 7)
svm_res

show_best(svm_res, metric = "rmse", maximize = FALSE)

autoplot(svm_res, metric = "rmse") +
  scale_x_log10()

