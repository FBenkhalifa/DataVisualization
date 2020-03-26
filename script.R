library(datasets)
# Data wrangling
library(tidyverse)
library(lubridate)
library(reshape)

# Data viz
library(plotly)
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
mydata <- Titanic %>% as_tibble()

# Convert all except n to factors which are easier to handle later on
mydata <- mydata %>% mutate_at(vars(-n), as.factor) 

# Untable the dbl which makes it easier to use with summarize functions
mydata <- untable(df = mydata %>% select(-n), 
                  num = mydata %>% pull(n))

# Retable for a quick check if n stays the same
mydata %>% count(Class, Sex, Age, Survived, .drop = FALSE)

# II Data exploration --------------------------------------------------------


# Now distributions can easily be computed by choosing the variables of interest
mydata %>% 
  count(Class, Sex, .drop = FALSE) %>% 
  group_by(Class) %>% 
  mutate(freq_per_class = n/sum(n))

mydata %>% 
  count(Class, Sex, .drop = FALSE) %>% 
  group_by(Sex) %>% 
  mutate(freq_per_Sex = n/sum(n))

# III Data visualization ------------------------------------------------------


