
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

a <- c(rep("Very unfriendly", 0.3*10000), rep("Slightly unfriendly", 0.4*10000), rep("Neither friendly or unfriendly", 0.1*10000),rep("Slightly friendly", 0.05*10000), rep("Very friendly", 0.05 *1000))

a <- sample(a, 30)

prop.table(summary(as.factor(a)))

# animated response data

b <- c(rep("Very unfriendly", 0.03*10000), rep("Slightly unfriendly", 0.07*10000), rep("Neither friendly or unfriendly", 0.2*10000),rep("Slightly friendly", 0.3*10000), rep("Very friendly", 0.4 *1000))

b <- sample(b, 30)

prop.table(summary(as.factor(b)))

y <- tibble(
  a, b) %>%
  gather(`a`, `b`, key = "robot_emo_1", value = "evaluation") %>%
  mutate(id = seq(1,60)) %>%
  select(id, evaluation) 


## also add as numbers

# non animated response

c <- c(rep("1", 0.3*10000), rep("2", 0.4*10000), rep("3", 0.1*10000),rep("4", 0.05*10000), rep("5", 0.05 *1000))

c <- sample(c, 30)

prop.table(summary(as.factor(c)))

# animated response data

d <- c(rep("1", 0.03*10000), rep("2", 0.07*10000), rep("3", 0.2*10000),rep("4", 0.3*10000), rep("5", 0.4 *1000))

d <- sample(d, 30)

prop.table(summary(as.factor(d)))

z <- tibble(
  c, d) %>%
  gather(`c`, `d`, key = "robot_emo_2", value = "eval_num") %>%
  mutate(id = seq(1,60)) %>%
  select(id, eval_num) 

# create tibble with all ordinal values

ord <- y %>%
  right_join(z, key = id
)


# merge ordinal data with existing dataset
# change labels to animated and non_animated

data1 <- data %>%
  right_join(ord, key = id) %>%
  select(id, animacy, eval_num, evaluation, perf) %>%
  rename(response_time = perf,
         likert_resp = eval_num,
         likert_num = evaluation) %>%
  mutate("animacy" = recode(animacy,
                          "neutral" = "non-animated",
                          "friendly" = "animated" ))
  

# write data as csv for students

# transfer this data to csv as students will have to deal with 
# reading in data types


write_csv(data1, "t-test-data.csv")


#### Reading in data ----

# read in the data using `readr` package

t_test_data <- read_csv("t-test-data.csv")


# examine the raw data

View(t_test_data)

# have a look at the top six rows only

head(t_test_data)

# have a look at bottom six rows only

tail(t_test_data)


summary(t_test_data)

# Data examines the participants (N = 60) rating of friendly v neutral robot
# robot_emo = robot emotion
# experiment is between-subjects
# perf = perceved performance by the user - responses provided by clicking 
# a line marked 0 at one end and 10 at the other


# 2.1. Calculating the critical value ----


# In this example, each sample has 29 degrees of freedom (n-1);
# so we have 58 degrees of freedom in total

# Another way to think about this is that the sample size is 60, 
# and we have estimated paramters from the data 
# (e.g. sample friendly + sample neutral), so we have 60 - 2 = 58.

# We typically use 5% as the chance of rejecting the null hypothesis
# when it is true (what is known as the type 1 error rate; a false positive)


# Let's pretend we do not know a-priori which of the conditions
# students mean evalution was lowest/highest. Without this a priori
# assumption we perform a two-tailed test; so the critical value of
# Student's t is:

qt(0.975, 58)

# [1] 2.001717

# This means that our test statistic needs to be bigger than 2.00 to 
# reject the null hypothesis, and to conclude the two means are 
# significantly different at alpha = 0.05. 


# 2.2. Plotting the data ---- 

# 2.2.1. Checking for normality

# First examine the distribution of each vector: bell-shaped is parametic,
# non-bell shaped (skewed) data is non-parametric.
# For continuous data (e.g. interval, ratio scale) we plot a hisogram to
# check shape of distribution

# distribution of neutral robot data

t_test_data %>%
  filter(robot_emo == "neutral") %>%
  ggplot(.) +
  geom_histogram(mapping = aes(x = perf),
                 bins = 20)


# distribution of friendly robot data

t_test_data %>%
  filter(robot_emo == "friendly") %>%
  ggplot(.) +
  geom_histogram(mapping = aes(x = perf),
                 bins = 20)


# Most of the observations fall around the mean so we can conclude that the
# data is normally distributed


# 2.2.2. Checking for differnce


# A usful way to test your two smaples is to examine a boxplot
# with notches - notches show whether the medians are significantly
# different at the 5% level

# We use the ggpot2 package to generate graphics
# for a boxplot, the syntax is geom_boxplot

t_test_data %>%
  ggplot(.) +
  geom_boxplot(mapping = aes(x = robot_emo, y = perf, fill = robot_emo, notch = TRUE)) + 
  theme(legend.position = "none")

# Notches of the two plots do not overlap, we can conclude
# that the medians are significantly different at the 5% level. 
 
# Note that the variability is similar in both emotion conditions both in terms
# of the range (whiskers) and the IQR (boxes)
 

 
 
# 2.3. Perform a t-test by hand ----

# To carry out a t test long-hand, we begin by calculating the variances of the two
# samples
 
# neutral condition

neutral_resp <- t_test_data %>%
  filter(robot_emo == "neutral") %>%
  summarise(mean = mean(perf),
            sd = sd(perf),
            var = var(perf)) %>%
  print

# mean = 3.22
# sd = 0.99
# var = 0.986

# neutral condition

friendly_resp <- t_test_data %>%
  filter(robot_emo == "friendly") %>%
  summarise(mean = mean(perf),
            sd = sd(perf),
            var = var(perf)) %>%
  print

# mean = 4.90
# sd = 1.44
# var = 1.209


# The value of the test statistic for Student's t test is the difference
# divided by the standard error of the difference.

# So, the numerator is the difference between the two means, and the denominator
# is the sqaure root of the two variances divided by their sample size (i.e. 30 for each)
 
# mean(neutral) - mean(friendly) / square root(neutral_var/30 + friendly_var/30)

(3.22 - 4.90) / sqrt(0.986/30 + 1.209/30)

#  -6.211

# You can ignore the minus sign as we are only concerned with the absolute difference. 
# Thus, the calculated value of the test statistic is 6.211 and the critical value is
# 2.00. (qt, 0.975, 58). 
# As the calaulted value is of the test statistic is larger than the critical value
# we reject the null hypothesis. A good way to remember this rule is 
# "larger rejcet, smaller accept". 

# The null hypothesis was that the two populaion means are not significantly different
# so we reject this, and accept the alternative hypothesis that the two means are
# significantly different.
# Instead of just rejecting the null hypothesis, it is better to state the porbability
# that data as extreme as this ( or more extreme) would be observed if the population 
# mean values really were the same.
# For this we use pt rather than qt , and in this instance 2*pt beacuse we are performing
# a two-tailed test

2*pt(-6.211, 58)

# [1] 6.102005 x 10^8

# So, we can conclude that p < 0.001.



# 2.4. Perform a t test using R's built in function ----

#### START HERE 20180821 ----

# There is of course a function in R that does all of this.
# t.test will perform the above once passed two vectors which the test is to be 
# carried out

# needto remove id vector to perform grouping, then add back in

t_test <- t_test_data %>%
  select(-id) %>%  
  group_by_at(vars(-perf)) %>%
  mutate(id=1:n()) %>% 
  ungroup() %>% 
  spread(key = robot_emo, value = perf) 
  
  
# now the data is ready to perform a t-test
 
t.test(t_test$neutral, t_test$friendly)

# if want paired add paired=FALSE
## add data for non-sig effect

## long RT and favourable 

# note, we use the $ to specify a vector from a dataset

# t = --4.2977, df = 57.862, p-value = 6.696e-05

# Values are almost the same, be sure to include 3 d.p. for value equivalence


# 2.5. Interpreting the output


# Welch Two Sample t-test
# 
# data:  t_test$neutral and t_test$friendly
# t = -4.8065, df = 57.857, p-value = 1.131e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.245036 -0.924831
# sample estimates:
#   mean of x mean of y 
# 3.271733  4.856667 


# First, compare your t statistic to the critical value. As 
# 4.807 is larger than 2.002. That means that participants mean rating of their
# performance was significantly higher when the robot was friendly to when it was
# neutral. 

# Note, that beacuse the means are significantly differnt, the confidence interval
# on the difference does not include 0: it goes from -2.245036 to -0.924831.


# 2.6. Reporting your findings

# Participant's rating of their performance in the task was signifcantly higher when
# the robot was friendly (Mean = 4.857, SD = 1.308) compared to when it was neutral
# (Mean = 3.271, SD = 1.245; t(58) = 4.807, p < 0.001 (two-tailed)). 


##### Points to add ----

# visualising categorical data
# barplot v pie chart
# non-parametric example
# 3rd  exmaple requires more input from student
# Frank start making material in August
# have materials ready by end of August
# sned on RMarkdown slides to Frank


 