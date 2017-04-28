#PS-09

library(tidyverse)
library(broom)
library(tidyr)
library(dplyr)
library(class)
library(lubridate)
library(base)
library(class)

train <- read.csv("/Users/Tina/Desktop/MATH218/train.csv", header = TRUE, sep = ",")
test <- read.csv("/Users/Tina/Desktop/MATH218/test.csv", header = TRUE, sep = ",")

#Compute hour of the day
train$hour <-hour(train$datetime)
test$hour <- hour(test$datetime)

#Compute day of the week
train$dow <- wday(train$datetime)
test$dow <- wday(test$datetime)

test$count<-0

test2 <- test
#define number of neighbors
num_neigh <- 3

classifications <- train$count

train_input <- train %>%
  select(datetime,count) %>% 
  mutate(
    count = as.numeric(count),
    datetime = as.integer(datetime)
  ) %>% 
    na.omit()
test_input <- test %>%
  select(datetime,count) %>% 
  mutate(
    count = as.numeric(count),
    datetime = as.integer(datetime)
  ) %>% 
    na.omit()

#fit a model 
model_knn <-knn(train=train_input, test=test_input, cl=classifications, k = num_neigh, prob=TRUE)

# Add predictions and probabilities to test set
submission <- test %>%
  mutate(
    count = model_knn
  ) %>% 
  select(datetime,count)

submission %>% readr::write_csv("Submission.csv")


