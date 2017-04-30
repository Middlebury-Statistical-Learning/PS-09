library(tidyverse)
library(broom)
library(ROCR)
library(class)
library(caret)
library(jsonlite)


setwd("~/PS09")
# Read train and test data
train <- fromJSON("train.json")
train <- bind_rows(train, .id = 'play')
test <- fromJSON("test.json")
test <- bind_rows(test, .id = 'play')

names(train)
#For some weird reason variables come out as lists, must change that:
train$interest_level <- as.character(train$interest_level)
train$bathrooms <- as.numeric(train$bathrooms)
train$bedrooms <- as.numeric(train$bedrooms)
train$price <- as.numeric(train$price)

test$bathrooms <- as.numeric(test$bathrooms)
test$bedrooms <- as.numeric(test$bedrooms)
test$price <- as.numeric(test$price)



#train[is.na(train)] <- 0

########################################
#Start with simple model. Do CV later
########################################


# Note the training data and test data inputs to knn() have to be the same size
# and just be the predictor variables
train_input <- train %>%
  select(interest_level,bathrooms,bedrooms,price)

test_input <- test %>%
  select(bathrooms,bedrooms,price)


model_formula <- as.formula(interest_level~bathrooms+bedrooms+price)
knn_model<- knn3(model_formula, train_input, k = 1)
predictions <- predict(knn_model, test_input, type = "prob")

###########################################
#5-fold CV - Testing k from 1 to 10
#################################################
# Vector of different neighbors to try:
knn_vector <- c(1:20)

# Assign folds at random
n_folds <- 5
train_input <- train_input %>%
  mutate(fold = sample(1:n_folds, replace=TRUE, size=nrow(train)))

# Store  scores here:
score_eachfold <- rep(0, n_folds)
scores <- rep(0, length(knn_vector))

for(j in 1:length(knn_vector)) {

for(i in 1:n_folds){
  # 1. Create disjoint pseudo-train and pseudo-test sets based on folding
  # scheme. Now the pseudo_test has more than one observation, it has 178 = ~1/5
  # of data
  pseudo_train <- train_input %>%
    filter(fold != i)
  pseudo_test <- train_input %>%
    filter(fold == i)
  
  # 2. Train the model using (i.e. fit the model to) the pseudo_train data.
  knn_model<- knn3(model_formula, pseudo_train, k = knn_vector[j])
  

  # 3. Get fitted/predicted values y-hat for the pseudo_test data using the
  # trained model
  predictions <- predict(knn_model, pseudo_test, type = "prob")
  predictions <- data.frame(predictions)
  # 4. Compute your score on the pseudo_test data
  #add real values to predictions
  predictions <- predictions %>%
    mutate(outcome = pseudo_test$interest_level) %>%
    mutate(high_real = ifelse(outcome=="high",1,0)) %>%
    mutate(medium_real = ifelse(outcome=="medium",1,0)) %>%
    mutate(low_real = ifelse(outcome=="low",1,0))
  # Compute my own pseudo_MSE for these outcomes
  predictions <- predictions %>%
    mutate(sqerror= (high-high_real)^2 + (medium-medium_real)^2 + (low-low_real)^2) %>%
    summarise(mse = mean(sqerror))
  # 5. Save your score for this fold
  score_eachfold[i] <- predictions$mse
}

#save result for current number of nearest neighbors:
scores[j] <- mean(score_eachfold)
print(knn_vector[j])
}

#############################################
#FITTING MODEL - OUTPUT TO KAGGLE COMPETITION
#############################################
# k=20 was the best value i found in this cv

model_formula <- as.formula(interest_level~bathrooms+bedrooms+price)
knn_model<- knn3(model_formula, train_input, k = 20)
kaggle_predictions <- predict(knn_model, test_input, type = "prob")

kaggle_predictions <- kaggle_predictions %>%
  data.frame() %>%
  mutate(listing_id = test$listing_id)

#reorder rows
kaggle_predictions <- kaggle_predictions[c("listing_id", "high", "medium","low")]
#weird thing for handling lists
kaggle_predictions$listing_id <- as.numeric(kaggle_predictions$listing_id)

#output to csv
kaggle_predictions %>% write.csv(file = "sigma_submission.csv")



