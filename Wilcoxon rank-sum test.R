
################################
#### Wilcoxon rank-sum test ----
################################



#### 1. Housekeeoping ----

library(tidyverse)

#### 2. Data ----

# Same experiment only number of participants in each group is now 10
# N = 20

# data creation

numcases.a <- 10
min.a <- 7.534
max.a <- 9.252

a <- as.numeric(runif(numcases.a, min.a, max.a + 1))

numcases.b <- 10
min.b <- 1.581
max.b <- 2.366

b <- as.numeric(runif(numcases.b, min.b, max.b + 1))

ID <- seq(1,20,1)

data1 <- tibble(a, b) %>%
  rename(neutral = a,
         friendly = b) %>%
  gather("robot_emo", "perf", neutral:friendly) %>%
  mutate(ID) %>%
  select(ID, robot_emo, perf) %>%
  data.frame

write.csv(data1, "wilcoxon-data.csv")

# transfer this data to csv as students will have to deal with 
# reading in data types


# Wilcoxon rank-sum test

# This is a non-parametric alternative to Student's t-test, to
# be used if the errors in the data are not non-normal. 

# The Wilcoxon rank-sum test statistic, W, is calculated as follows:
# both samples are entered into a single array with their sample
# names clearly attached (i.e. "neutral", "friendly"). Then, the 
# aggregate list is sorted, with care taken to ensure the sample 
# labels are kept with their respective values.
# A rank is assigned to each value, with ties getting the appropriate
# average rank (e.g. rank i + (rank i + 1)/2 ...etc) 
# Finally, the ranks are added up for each of the two samples, and
# significance is assessed on the size of the smaller sum of ranks.

# Frist make a vector of the two samples

robot_emo <- c(a,b)
robot_emo

# then make a list of the sample names

label <- c(rep("neutral", 10), rep("friendly", 10)) 
label

# then use the built-in function 'rank' to get a vector containing
# all of the ranks, smallest to largest with the combined vector

combined.ranks <- rank(robot_emo)
combined.ranks

# Ties would have been dealt with by averaging the appropriate
# ranks (e.g. 13.5 as opposed to 13). 
# We use 'tapply' and 'sum' as the required operation

tapply(combined.ranks, label, sum)

# friendly  neutral 
# 55        155

# Finally, we compare the smaller of the two values (219) with values
# in the tables of Wilcoxon rank sums (Snedecor & Cochran, 1980, p.555) 
# and reject the null hypothesis if our value of 66 is smaller than the
# value in tables. 
# For two samples (n = 10) the Wilcoxon rank sum is 27 at p < 0.05.
# So, in this instance, we do not reject the null hypothesis as the 
# smallest combined rank is larger than this value (55 v 27). 
# The two smaple means are not significantly different

# Step 1. Conduct analysis

wilcox.test(a, b)

