library(datasets)
# Data wrangling
library(tidyverse)
library(lubridate)
library(reshape)

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

# Data analysis
library(tidymodels)
library(broom)
library(glmnet)

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

# II Data exploration --------------------------------------------------------
# Now distributions can easily be computed by choosing the variables of interest

## A Univariate distributions

# P(Sex)
mydata %>% 
  count(Sex, .drop = FALSE) %>% 
  mutate(freq = n/sum(n))

# P(Age)
mydata %>% 
  count(Age, .drop = FALSE) %>% 
  mutate(freq = n/sum(n))

# P(Class)
mydata %>% 
  count(Class, .drop = FALSE) %>% 
  mutate(freq = n/sum(n))

# P(Survived)
mydata %>% 
  count(Survived, .drop = FALSE) %>% 
  mutate(freq = n/sum(n))


## B Bivariate Distributions

# P(Class, Sex)
mydata %>% 
  count(Class, Sex, .drop = FALSE) %>% 
  mutate(freq= n/sum(n))

# P(Class, Age)
mydata %>% 
  count(Class, Age, .drop = FALSE) %>% 
  mutate(freq= n/sum(n))

## C Conditinale Distributions

# P(Sex|Class)
mydata %>% 
  count(Class, Sex, .drop = FALSE) %>% 
  group_by(Class) %>% 
  mutate(freq_per_class = n/sum(n))

# P(Class|Sex)
mydata %>% 
  count(Class, Sex, .drop = FALSE) %>% 
  group_by(Sex) %>% 
  mutate(freq_per_Sex = n/sum(n))


# III Data visualization ------------------------------------------------------


data(Titanic)
titanic <- as.data.frame(Titanic)
titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))


ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class), fill=Survived))
# good practice: use the 'dependent' variable (or most important variable)
# as fill variable

ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class, Age), fill=Survived))

ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class), conds=product(Age), fill=Survived))
## B Plot marginal distributions
mydata_long <- mydata %>% pivot_longer(everything(), names_to = "variable", values_to = "attribute", names_repair = "universal")

mydata_long %>% ggplot(aes(x = attribute, group = variable)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = percent) +
  ylab("Survival Rate") +
  facet_wrap(~variable, scales = "free_x") +
  theme_hc()


## A Plot data with target variable
survival_rate <- mydata %>% 
  count(Survived, .drop = FALSE) %>% 
  mutate(freq_per_class = n/sum(n))

# Convert data to long format for facet plotting
mydata_semi_long <- mydata %>% 
  pivot_longer(-Survived, 
               names_to = "variable", 
               values_to = "attribute", 
               names_repair = "universal") %>% 
  mutate_if(is.character, as.factor)

fct_counts <- mydata_semi_long %>%
  group_by(variable) %>%
  do(fct_count(.$attribute, prop = TRUE))

mydata_semi_long %>% ggplot(aes(x = attribute, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  ylab("Survival Rate") +
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  scale_fill_brewer(palette = "Set1") +
  geom_hline(yintercept = survival_rate$freq_per_class[2], col = "white", lty = 2, size = 1) +
  # geom_label(label = fct_counts$n, nudge_y = 1500) +
  facet_wrap(~variable, scales = "free_x") +
  theme_hc()

ggplot(mpg, aes(x=class, y=drv)) + 
  geom_count(aes(size=..prop..), colour="lightgrey") +
  geom_count(aes(size=..prop.., group=class), colour="cornflowerblue")  +
  scale_size(range = c(0,10), breaks=seq(0,1,by=0.2)) +
  coord_fixed() +
  theme_minimal()

ggplot(mydata, aes(x=Sex, y=Class)) + 
  geom_count(aes(size=..prop..), colour="lightgrey") +
  geom_count(aes(size=..prop.., group=Class), colour="cornflowerblue")  +
  scale_size(range = c(0,10), breaks=seq(0,1,by=0.2)) +
  coord_fixed() +
  theme_minimal()

ggplot(mydata, aes(x=Sex, y=Age)) + 
  geom_count(aes(size=..prop..), colour="lightgrey") +
  geom_count(aes(size=..prop.., group=Age), colour="cornflowerblue")  +
  scale_size(range = c(0,10), breaks=seq(0,1,by=0.2)) +
  coord_fixed() +
  theme_minimal()

ggplot(mydata, aes(x=Class, y=Age)) + 
  geom_count(aes(size=..prop..), colour="lightgrey") +
  geom_count(aes(size=..prop.., group=Age), colour="cornflowerblue")  +
  scale_size(range = c(0,10), breaks=seq(0,1,by=0.2)) +
  coord_fixed() +
  theme_minimal()

## C Heatmaps
library(ggplot2)

set.seed(1)
dat <- data.frame(x = rnorm(1000), y = rnorm(1000))

# plot
p <- ggplot(dat, aes(x = x, y = y)) + geom_bin2d() 
p
# Get data - this includes counts and x,y coordinates 
newdat <- ggplot_build(p)$data[[1]]

# add in text labels
p + geom_text(data=newdat, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                               label=count), col="white")
mydata %>% ggplot(aes(x = Class, y = Class, fill = Survived)) +
  geom_tile()

# IV Model Fitting and evaluation ------------------------------------------------------

# A Fit linear regression
linear_reg <- mydata %>% 
  mutate_at("Survived", ~case_when(Survived == "No" ~ 0,
                                   Survived == "Yes" ~ 1)) %>%  # Convert target variable to numeric for linear regression
  lm(Survived ~ ., .) # Run regression

# Display coefficients and informations and summary statistics
tidy(linear_reg)
glance(linear_reg)

# B Fit log regression
# Preprocess the data
mydata <-
  recipe(Survived ~ ., data = mydata) %>% 
  step_dummy(all_predictors(), one_hot = TRUE) %>% 
  step_interact(terms = ~ starts_with("Sex"):starts_with("Age")) %>% 
  step_zv(all_predictors()) %>% 
  prep() %>% 
  juice() %>% 
  mutate_at("Survived", ~case_when(Survived == "No" ~ 0,
                                   Survived == "Yes" ~ 1))
  

log_reg_simple <- glm(Survived ~ ., mydata %>% select(Survived, starts_with("Class")), family = binomial(link = "logit"))
log_reg_simple %>% pander
# Display coefficients and informations and summary statistics 
tidy(log_reg_simple)
glance(log_reg_simple) %>% pander

log_reg_full <-  glm(Survived ~ ., mydata , family = binomial(link = "logit"))
tidy(log_reg_full)
glance(log_reg_full) %>% pander

log_reg_full %>% pander
# Lasso Regression --------------------------------------------------------

# Define test and training sample
split <- initial_split(mydata)
train <- training(split)
test <- testing(split)

titanic_cv <- vfold_cv(train_data, v = 10, repeats = 1)


y = train %>% select(Survived) %>% data.matrix() 
x = train %>% select(-Survived)  %>% add_column(intercept = 1)  %>% data.matrix()

lasso <- cv.glmnet(y = y, 
                   x = x, 
                   family = "binomial", 
                   alpha = 1,
                   type.measure = "class",
                   type.logistic = "Newton",
                   nfolds = 10,
                   penalty.factor=c(rep(0, 4), rep(1, ncol(x) - 4)))

lasso %>% tidy
plot(lasso)


lambda <- 
  lasso$glmnet.fit %>% 
  tidy %>% 
  filter(dev.ratio== max(dev.ratio)) %>% 
  pull(lambda) %>% 
  unique

coefficients <- coef(lasso, s = lambda) 
coefficients %>% tidy

# 3 Extract the lambda fpr which lasso has chosen two predictors
lambda_var <- lasso$glmnet.fit$lambda[lasso$glmnet.fit$df == 2]

# 4 Get the coefficients chosen for the corresponding lasso


