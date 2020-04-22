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

# Data presentation
library(knitr)

# Data analysis
library(tidymodels)
library(broom)


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
who %>% pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = c("diagnosis", "gender", "age"),
  names_pattern = "new_?(.*)_(.)(.*)",
  values_to = "count"
)
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
mydata_semi_long <- mydata %>% pivot_longer(-Survived, names_to = "variable", values_to = "attribute", names_repair = "universal")

mydata_semi_long %>% ggplot(aes(x = attribute, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  ylab("Survival Rate") +
  scale_fill_brewer(palette = "Set1") +
  geom_hline(yintercept = survival_rate$freq_per_class[2], col = "white", lty = 2, size = 1.5) +
  facet_wrap(~variable, scales = "free_x")



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
log_reg <- glm(Survived ~ ., mydata, family = binomial(link = "logit"))

# Display coefficients and informations and summary statistics 
tidy(log_reg)
glance(log_reg)



# B Compute anova

# Produce linear anova
linear_anova <- anova(linear_reg)
linear_anova %>% tidy

# Produce log anova
log_anova <- anova(log_reg)
log_anova %>% tidy