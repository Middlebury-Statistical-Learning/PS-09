# Author: David Valentin
# Professor Albert Kim
# Homework: PS-09

#Load Necessary Libraries:
library(tidyverse)
library(broom)
library(class)

# General Notes:
# Tried to get it to work to the best of my abillity. Its hard to debug certain things
# because this is not really a class on learning R, but the general idea is there! 
# Cleaning the data set took the most time, honestly filtered out a lot of the unknown data. 
# This was probably unncessary but it simplified the data frame a lot for me and I was able to understand a lot better


setwd("/Users/dvalentin/Documents/Code/CS_Homework/MATH218_Stat_Learning/PS-09")

#Initializing Test and Train and setting seed
#____________________________________________
train <- read.csv2("train_users_2.csv", sep=',', header=TRUE)

test <- read.csv2("test_users.csv", sep=',', header=TRUE)

#Clean the data sets
airbnb_train <- train %>%
  filter(gender != "-unknown-") %>% 
  select(-date_first_booking)

#Clean the data sets
airbnb_test <- test %>%
  filter(gender != "-unknown-") %>% 
  select(-date_first_booking)


#setting the random seed
set.seed(1234)

#Build Samples:
sample_airbnb_train <- sample_frac(airbnb_train, 0.2)

#setting the classifications
classifications <- airbnb_train$country_destination

# Choosing my Input data sets

train_input <- airbnb_train %>%
  select(gender, age)

test_input <- airbnb_train %>%
  select(gender, age)


model_knn <-class::knn(train=train_input, test=test_input, cl=classifications, k = 3)

# Add predictions and probabilities to test set
test <- test %>%
  mutate(
    Species = model_knn,
    Probability = attr(model_knn, "prob")
  )

#Build the train and test sets
airbnb_knn_model <- knn(train = airbnb_train, test = airbnb_test, cl = classifications, k=3, prob=TRUE)

n_folds = 100

airbnb_train <- airbnb_train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

airbnb_test <- airbnb_test %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)


for(i in 1:n_folds){
  # 1. Create pseudo train/test based on folding scheme
  pseudo_train <- airbnb_train %>%
    filter(fold != i)
  
  pseudo_test <- airbnb_test %>%
    filter(fold == i)

  # Needed for glmnet() methods:
  pseudo_train_X <- model.matrix(model_formula, data = pseudo_train)[, -1]

  # 2. Fit model to pseudo-train
  airbnb_knn_model <- knn(train = pseudo_train, test = airbnb_test, cl = classifications, k=3, prob=TRUE)

  # 3. Compute K for each method and save them (by appending/binding to
  # results_all_folds)
  results_all_folds <- 
    # Create data frame of y=SalePrice and predictions from all three methods
    data_frame(
      country_pred = pseudo_train$county_destination,
      k = as.vector(airbnb_knn_model$county_destination)
    ) 
  
}








