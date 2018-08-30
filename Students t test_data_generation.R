
##########################
#### Student's t-test ----
##########################


#### 1. Housekeeoping ----

# install necessary packages

install.packages('readr')
install.packages('tidyr')
install.packages('dplyr')
install.packages('ggplot2')

# call in packages to R Studio

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

#### 2. Data ----

# rounding options

# options(digits = 2) -- this is not use as it prints 2 units rather than decimals
# function for converting data to 2d.p
# specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
# trimws removes any leading white space 

## solution shown in tidyverse code below



#### data generation

# create tibble (type of data table) with 2 vectors
# round vectors to 2 decimals places
# gather so data is in the long view
# animated and non-animated


# add data for participant response time

data <- tibble(
  animated = rnorm(30, mean = 4.7, sd = 1.2),
  non_animated = rnorm(30, mean = 3.1, sd = 1.1)) %>%
  gather(`animated`, `non_animated`, key = "animacy", value = "response_time") %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(id = seq(1,60)) %>%
  select(id, animacy, response_time)

str(data)
summary(data)


##### Generate ordinal likert data ----

# scale of "Very unfriendly" = 0, "Slightly unfriendly" = 2, "Neither friendly or unfriendly" = 3, "Slight friendly" = 4, "Very friendly" = 5

# non animated response data

# a <- c(rep("Very unfriendly", 0.3*10000), rep("Slightly unfriendly", 0.4*10000), rep("Neither friendly or unfriendly", 0.1*10000),rep("Slightly friendly", 0.05*10000), rep("Very friendly", 0.05 *1000))
# 
# a <- sample(a, 30)
# 
# prop.table(summary(as.factor(a)))
# 
# # animated response data
# 
# b <- c(rep("Very unfriendly", 0.03*10000), rep("Slightly unfriendly", 0.07*10000), rep("Neither friendly or unfriendly", 0.2*10000),rep("Slightly friendly", 0.3*10000), rep("Very friendly", 0.4 *1000))
# 
# b <- sample(b, 30)
# 
# prop.table(summary(as.factor(b)))
# 
# y <- tibble(
#   a, b) %>%
#   gather(`a`, `b`, key = "animacy", value = "likert_resp") %>%
#   mutate(id = seq(1,60)) %>%
#   select(id, likert_resp) 


## also add as numbers

# non animated response

c <- c(rep("5", 0.3*10000), rep("4", 0.4*10000), rep("3", 0.1*10000),rep("2", 0.05*10000), rep("1", 0.05 *1000))

c <- sample(c, 30)

prop.table(summary(as.factor(c)))

# animated response data

d <- c(rep("5", 0.03*10000), rep("4", 0.07*10000), rep("3", 0.2*10000),rep("2", 0.3*10000), rep("1", 0.4 *1000))

d <- sample(d, 30)

prop.table(summary(as.factor(d)))

z <- tibble(
  c, d) %>%
  gather(`c`, `d`, key = "animacy", value = "likert_num") %>%
  mutate(id = seq(1,60)) %>%
  select(id, likert_num) 

# create tibble with all ordinal values

ord <- y %>%
  right_join(z, key = id
)


# merge ordinal data with existing dataset
# change labels to animated and non_animated

data1 <- data %>%
  right_join(z, key = id) %>%
  select(id, animacy, likert_num, response_time) %>%
  mutate("animacy" = recode(animacy,
                          "neutral" = "non-animated",
                          "friendly" = "animated" ))
  

# write data as csv for students

# transfer this data to csv as students will have to deal with 
# reading in data types


write_csv(data1, "t-test-data.csv")






 