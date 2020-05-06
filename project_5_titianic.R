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
library(ggpubr)

# Data presentation
library(knitr)
library(stargazer)

# Data analysis
library(tidymodels)
library(broom)
library(glmnet)
library(rsample)
library(furrr)

library(pander)


# I Data preparation -------------------------------------------------------------------------

# Convert array to 2-dimensional format
mydata <- Titanic %>% as_tibble() 

# Convert all except n to factors which are easier to handle later on
mydata <- mydata %>% mutate_at(vars(-n), as.factor) 

# Untable the dbl which makes it easier to use with summarize functions
mydata <- untable(df = mydata %>% select(-n), 
                  num = mydata %>% pull(n))

# Retable for a quick check if n stays the same
mydata %>% count(Class, Sex, Age, Survived, .drop = FALSE)


# II Prepare some descriptive statistics ----------------------------------

# 1 Convert binary outcome to 0 and 1 in order to create means
mydata_mean <- mydata %>% mutate_at("Survived", ~ifelse(. == "No", 0, 1))

# 2 Create category means
mydata_mean %>% 
  pivot_longer(-Survived, names_to = "Variable") %>% 
  group_by(value) %>% 
  summarize(`Survival Probability` = format(mean(Survived), digits = 3)) %>% 
  rename(Category = value) %>% 
  as.matrix() %>% 
  stargazer(., type = "text", digits = 3)

# 3 Produce a table which helps to grasp differences within groups more easily
p1 <- mydata %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "attribute",
    names_repair = "universal"
  ) %>% ggplot(aes(x = attribute, group = variable)) +
  geom_bar(aes(y = (..count..)))+
  ylab("n") +
  labs(title = "Histograms per variable") +
  facet_wrap(~variable, scales = "free_x") +
  theme_bw() +
  theme(text = element_text(size=10))

# 4 Compute survival rate which will be included in next plot
survival_rate <- mydata %>% 
  count(Survived, .drop = FALSE) %>% 
  mutate(freq_per_class = n/sum(n))
p1
# 5 Convert data to long format for facet plotting in ggplot
p2 <- mydata %>% 
  pivot_longer(-Survived, 
               names_to = "variable", 
               values_to = "attribute", 
               names_repair = "universal") %>% 
  mutate_if(is.character, as.factor) %>% 
  ggplot(aes(x = attribute, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  ylab("Survival Rate") +
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  scale_fill_manual(values = c("gray25", "gray")) +
  geom_hline(yintercept = survival_rate$freq_per_class[2], col = "white", lty = 2, size = 1) +
  facet_wrap(~variable, scales = "free_x") +
  labs(title = "Survival outcome per category") +
  theme_bw()+
  theme(legend.position="bottom", text = element_text(size=10))

p2
## !!!!More descriptive stats can be found in the appendix!!!!

ggarrange(p1, p2, nrow = 1, widths = c(1, 1.3))
# II Prepare dataset with dummy variables and interaction terms -----------

# This will dummy encode the variables. The base group women, adult,1 class is chosen
mydata_rec <-
  recipe(Survived ~ ., data = mydata) %>% 
  step_dummy(all_predictors()) %>% # This will create dummy variables
  step_interact(terms = ~ starts_with("Sex"):starts_with("Age")) %>% # This will create the interaction terms (class is left out)
  prep() %>% 
  juice() 

mydata_rec
# III Create tuning grid and run cv ---------------------------------------
# In this part we create the tuning grid. The classes are forced to stay as regressors and are not 
# tuned in order to be able to estimate their effect on the dependent variable

# 1 Create vector which will be used to create tuning grid
all_vars <- mydata_rec %>% colnames %>% .[-c(1:4)]

# 2 The grid contains all possible combinations of all_vars and each combination will be
# sent through the cv

grid <- expand.grid(rep(list(0:1), length(all_vars))) %>% 
  set_names(all_vars) %>% 
  imap(~ifelse(.x == 1, .y, "")) %>% 
  do.call(cbind, .) %>% 
  as_tibble() %>% 
  mutate(Class_X2nd = "Class_X2nd", Class_X3rd = "Class_X3rd", Class_Crew = "Class_Crew") %>% 
  as.matrix() %>% 
  t() %>% 
  as_tibble()
grid

# 3 Prepare the tuning grid to be sent throug cv by setting up parallelization
plan(multiprocess)

# 4 Tune the model via a map call. A 15 fold cv is repeated 10 times for better estimates of the out of sample performance
# Mapping instead of for-looping allows for parallelization and therefor faster results
model_metrics <- future_map_dfr(grid, ~cv_log_reg(.vars = stri_remove_empty(.), .mydata = mydata_rec, .v = 15, .repeats = 10), .id = "combination", .progress = TRUE) %>% 
  arrange(n_var)

# 5 Check out some results
model_metrics
model_metrics$folds[[1]] # Here all necessary information about each cv run is captured  
                         # such as each fitted glm, the predictions and the metrics for each cv fold
model_metrics$fit[[1]] # Here the fit on the whole dataset (not only the training subset of a fold)
                       # is saved. This fit can then directly used to interpret the betas 
model_metrics %>% select(combination, accuracy) # The mean accuracy over the different out of sample performances for a full cv run
model_metrics %>% select(combination, kap)  # THe mean kappa over a full cv run

# 6 Save the model for reproducibility
save(model_metrics, file = "./cv_model/cv_results.rda")

# IV Debugging  ----------------------------------------------------------
# In order to check how the function works, run the following chunk and step through
# each line of the function code. Exit debug mode when finished.
cv_log_reg %>% debugonce()
cv_log_reg(.vars = c("Class_X3rd", "Class_X2nd", "Class_Crew"), .mydata = mydata_rec)


# V Create result Table ------------------------------------------------------------

# Create a vector which inputs the results from the CV into the stargazer table
cv_results <-  model_metrics %>% select( c(accuracy, kap)) %>% imap(~c(paste0("cv.",.y), round(.x, digits = 3)))

# Create the table
model_metrics %>% 
  pull(fit) %>% 
  exec(stargazer, ., title = "Results", align = TRUE, type = "text", table.placement="H",
       omit.stat=c("f", "ser"), order=c("Constant",    # This simply sets the vertical order of the variables in the table
                                        "Class_X2nd",
                                        "Class_X3rd",
                                        "Class_Crew",
                                        "Sex_Male",
                                        "Age_Child",
                                        "Sex_Male_x_Age_Child"),
       add.lines= cv_results) # This adds the cv list to the table

# VI Run model tests -------------------------------------------------------

# 1 Run a one sided anova on the best and the worst models
model_metrics$fit[[1]] %>% anova(test = "Chisq")%>% as.matrix %>% stargazer(., type = "text")
model_metrics$fit[[8]] %>% anova(test = "Chisq") %>% as.matrix %>% stargazer(., type = "text")

# 2 Run likelihood ratio test on both models
anova(model_metrics$fit[[1]], model_metrics$fit[[8]],  test = "Chisq") %>% as.matrix %>% stargazer(., type = "text")


# VII Appendix ----------------------------------------------------------------
# Here we have some chunks we did not include in the paper because of problems with
# space. But in a longer paper they could be useful.

## A Univariate distributions

# P(Sex)
mydata %>% 
  count(Sex, .drop = FALSE) %>% 
  mutate(freq = n/sum(n)) %>% 
  as.matrix() %>% 
  stargazer(type = "text", title = "P(Sex)")

# P(Age)
mydata %>% 
  count(Age, .drop = FALSE) %>% 
  mutate(freq = n/sum(n)) %>% 
  as.matrix() %>% 
  stargazer(type = "text", title = "P(Age)")

mydata %>% 
  count(Class, .drop = FALSE) %>% 
  mutate(freq = n/sum(n)) %>% 
  as.matrix() %>% 
  stargazer(type = "text", title = "P(Class)")

# P(Survived)
mydata %>% 
  count(Survived, .drop = FALSE) %>% 
  mutate(freq = n/sum(n)) %>% 
  as.matrix() %>% 
  stargazer(type = "text")


## B Bivariate Distributions

# P(Class, Sex)
mydata %>% 
  count(Class, Sex, .drop = FALSE) %>% 
  mutate(freq= n/sum(n)) %>% 
  as.matrix() %>% 
  stargazer(type = "text", title = "(Class, Sex)")

# P(Class, Age)
mydata %>% 
  count(Class, Age, .drop = FALSE) %>% 
  mutate(freq= n/sum(n)) %>% 
  as.matrix() %>% 
  stargazer(type = "text", title = "P(Class, Age)")

## C Conditinale Distributions

# P(Sex|Class)
mydata %>% 
  count(Class, Sex, .drop = FALSE) %>% 
  group_by(Class) %>% 
  mutate(freq_per_class = n/sum(n)) %>% 
  as.matrix() %>% 
  stargazer(type = "text", title = "P(Sex|Class)")

# P(Class|Sex)
mydata %>% 
  count(Class, Sex, .drop = FALSE) %>% 
  group_by(Sex) %>% 
  mutate(freq_per_Sex = n/sum(n)) %>% 
  as.matrix() %>% 
  stargazer(type = "text", title = "P(Class|Sex)")


