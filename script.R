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

# II Data exploration --------------------------------------------------------
mydata %>% group_by(Sex, Survived) %>% summarize(Cases = sum(n)) %>% group_by()
mydata %>% group_by(Sex, Survived) %>% tally()

# III Data visualization ------------------------------------------------------


