#--------------------------------------------------
# Week 1: In-class assignment
#--------------------------------------------------


# There is no one correct way to write the code to answer the questions
# But your code needs to 
# a. answer the question
# b. be fully reproducible

# For this assignment, we will use 
# the `yrbss` data 
# in the `openintro` package 

install.packages("openintro")
library(openintro)

# Other useful packages
install.packages("tidyverse")
library(tidyverse)

# Read the documentation for `yrbss` to learn about all the variables.
?yrbss

# The code below uses the `flextable` package to create a table of summary characteristics of
# Grade and Gender
# Modify the code below such that the grade shows in increasing order
# and all category labels start with a capital letter


install.packages("flextable")
library(flextable)

install.packages("dplyr")
library(dplyr)

yrbss$Grade <- yrbss$grade
yrbss$Gender <- yrbss$gender 

yrbss <- yrbss %>% mutate(Grade = fct_relevel(Grade, c("9", "10", "11", "12", "other")))
yrbss <- yrbss %>% mutate(Gender = str_to_title(Gender)) %>%
  mutate(Grade = str_to_title(Grade))

z <- summarizor(
  yrbss[c("Grade", "Gender")],
  overall_label = NULL
)
ft_1 <- as_flextable(z) 
ft_1


# To understand the pattern of physical activity by grade and gender,
# 1) aggregate  `physically_active_7d` by calculating its mean within each grade and gender
# 2) create a plot showing the average number of physically active days
#      x-axis: grade
#      y-axis: Mean of `physcially_active_7d`
#      Distinguish gender using different colors, symbols, or lines
# *** I would use the following functions: aggregate(), ggplot(), geom_line() but there is 
# no one correct way to do this
# Ensure that the figure is clearly labeled and includes an appropriate legend

aggregate(xxx) |>
  ggplot(aes(xxx)) + 
  geom_line()
...


aggregate(physically_active_7d ~ Grade + Gender, data = yrbss, FUN = mean, na.rm=TRUE) %>%
  ggplot(yrbss, 
  mapping = aes(x = Grade,
  y = physically_active_7d,
color = Gender,
group = Gender)) +
  geom_point() +
  geom_line() +
  labs(x = "Grade",
y = "Number of Days Physically Active",
title = "Number of Days Physically Active by Grade") +
  scale_y_continuous(limits = c(0, 7))


# Create a plot that shows the relationship betwen physical activity and bmi
# among female students in grade 12 
# Ensure that the figure is clearly labeled and includes an appropriate legend

yrbss$bmi <- yrbss$weight/(yrbss$height^2)
library(ggplot2)
data(yrbss)

yrbss %>% 
  dplyr::filter(Grade == 12, Gender == "Female") %>%
  ggplot( 
  mapping = aes(x = physically_active_7d,
  y = bmi)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x)
  labs(x = "Days of Physical Activity",
y = "BMI",
title = "Physical Activity and BMI Among Grade 12 Girls") +
  scale_y_continuous(limits = c(0, NA))

# Push your completed code to your GitHub repository
