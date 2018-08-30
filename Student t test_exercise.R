#### Students t Test exercise ----

#### Housekeeping----

# call in packages to R Studio

library(readr) # reading in the data
library(tidyr) # data wrangling
library(dplyr) # data wrangling
library(ggplot2) # plotting data

#### Reading in data ----

# read in the data using `readr` package

t_test_data <- read_csv("t-test-data.csv")

# examine the raw data

View(t_test_data)

#### Description of the data

#############

# The data examines 60 participants Likert rating and response time of animated v non-animated robot
# Each row represent a different participant responses, so there are 60 rows 
# "id" vector = participant number
# "Likert rating" vector = participants evaluation of the inteaction from 1 (Not very friendly) to 5 (very friendly)
# "animacy" vector = condition with two levels (animated, non-animated)
# in the animated conditon the robot is tracks the users head and uses gestures in communication
# in the non-animated condition the robot is static
# "response time" vector = the time (seconds) it took for participants to indicate how friendly they thought the robot was on the Likert scale
# the experiment (user study) is between-subjects; 
# i.e., one set of paticipants interact with the animated robot, and a separate group interact with the non-animated robot

#############

# Here are a few ways to check the data is ready for manipulation

# have a look at the top six rows only

head(t_test_data)

# have a look at bottom six rows only

tail(t_test_data)

# examine the structure of the data

str(t_test_data)


#### Generating descriptve statistics ----

# for data manipulation we require the "dplyr" and "tidyr" packages

# summarise the data quickly with the "summary" function

summary(t_test_data)

# as you are interested in group differnces, also summarise the by group 

# Likert responses

t_test_data %>%
  group_by(animacy) %>%                     # group_by groups the data by the specified vector
  summarise(avg_rating = mean(likert_num),  # summarise summarises the data into a single row of values
            standev = sd(likert_num))       # you specify what type of summary you would like with simple syntax (e.g. mean)
  


# as you can see, particpants in the animated robot group rated the robot as more friednly (M = 4.13, SD = 0.86) 
# compared to participants in the non-animated robot group (M = 2.77, 1.01).   


# Response time 

t_test_data %>%
  group_by(animacy) %>%                     # group_by groups the data by the specified vector
  summarise(avg_rt = mean(response_time),  # summarise summarises the data into a single row of values
            standev_rt = sd(response_time)) 

# as you can see, particpants in the animated robot group took longer to respond to the Likert question on Friendliness (M = 5.06, SD = 1.06) 
# compared to participants in the non-animated robot group (M = 3.46, 1.03). 

# you can do generate means for both vectors (animacy, response-time) with the following code

t_test_data %>%
  group_by(animacy) %>%                     
  summarise(avg_rating = mean(likert_num),  
            standev = sd(likert_num),
            avg_rt = mean(response_time),
            standev_rt = sd(response_time))




#### 2.2. Plotting the data ---- 

# plotting requires the "ggplot2" package

# We check the distribution of ordinal data using a barplot

# Likert data barplot

# distribution of non-animated robot data

t_test_data %>%
  filter(animacy == "non_animated") %>%
  ggplot(.) +
  geom_bar(mapping = aes(x = likert_num))

# distribution of non-animated robot data

t_test_data %>%
  filter(animacy == "animated") %>%
  ggplot(.) +
  geom_bar(mapping = aes(x = likert_num))


# 2.2.1. Checking for normality

# For continous (e.g. interval, ratio) data, we examine the distribution of the value in a vector: a bell-shape is parametic,
# non-bell shaped (skewed) data is non-parametric.
# For continuous data we plot a histogram to check shape of distribution

# Reponse time distribution

# distribution of non-animated robot data

t_test_data %>%
  filter(animacy == "non_animated") %>%
  ggplot(.) +
  geom_histogram(mapping = aes(x = response_time),
                 bins = 20)


# distribution of animated robot data

t_test_data %>%
  filter(animacy == "animated") %>%
  ggplot(.) +
  geom_histogram(mapping = aes(x = response_time),
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
  geom_boxplot(mapping = aes(x = animacy, y = response_time, fill = animacy, notch = TRUE)) + 
  theme(legend.position = "none")

# Notches of the two plots do not overlap, we can conclude
# that the medians are significantly different at the 5% level. 

# Note that the variability is similar in both emotion conditions both in terms
# of the range (whiskers) and the IQR (boxes)


# 2.4. Perform a t test using R's built in function ----

# t-test of Likert responses

# There is of course a function in R that does all of this.
# t.test will perform the above once passed two vectors which the test is to be 
# carried out

# need to remove id vector to perform grouping, then add back in

t_test <- t_test_data %>%
  select(-id, -response_time) %>%  
  group_by_at(vars(animacy)) %>%
  mutate(id=1:n()) %>% 
  ungroup() %>% 
  spread(key = animacy, value = likert_num) 


# now the data is ready to perform a t-test

t.test(t_test$non_animated, t_test$animated)

# if want paired add paired=FALSE
## add data for non-sig effect

## long RT and favourable 

# note, we use the $ to specify a vector from a dataset

# t = -5.6539, df = 56.632, p-value = 5.357e-07

# Values are almost the same, be sure to include 3 d.p. for value equivalence


# t-test of response time

t_test1 <- t_test_data %>%
  select(-id, -likert_num) %>%  
  group_by_at(vars(animacy)) %>%
  mutate(id=1:n()) %>% 
  ungroup() %>% 
  spread(key = animacy, value = response_time) 

# now the data is ready to perform a t-test

t.test(t_test1$non_animated, t_test1$animated)

#t = -5.9198, df = 57.955, p-value = 1.852e-07


##### Points to add ----

# visualising categorical data
# barplot v pie chart
# non-parametric example
# 3rd  exmaple requires more input from student
# Frank start making material in August
# have materials ready by end of August
# sned on RMarkdown slides to Frank