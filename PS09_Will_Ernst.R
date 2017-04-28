library(tidyverse)
library(class)
library(jsonlite)
#library(MLmetrics)

set.seed(76)
#load data
train <- bind_rows(fromJSON("train.json"))
test <- bind_rows(fromJSON("test.json"))
sample_submission <- read_csv('sample_submission.csv')

#clean train data
train <- train %>%
  mutate(
    #use jitter function to prevent too many knn ties in predictors
    bathrooms = jitter(unlist(bathrooms)),
    bedrooms = jitter(unlist(bedrooms)),
    longitude = unlist(longitude),
    latitude = unlist(latitude),
    interest_level = unlist(interest_level)
  )
train <- train %>%
  select(interest_level, bathrooms, bedrooms, longitude, latitude)


#===== Cross Validation =====#
#set number of folds
n_folds <- 5

#define ks to test
max_k <- 50
k_vector <- seq(1, max_k, by = 1)

#keep track of scores
avg_scores <- rep(0, max_k)

for(k_index in 1:length(k_vector)){
  print(k_index)
  #shuffle train
  train <- train %>%
    sample_frac(1) %>%
    mutate(fold = rep(1:n_folds, length=n())) %>%
    arrange(fold)
  
  #reset total score
  total_score <- 0
  k_index
  
  for(curr_fold in 1:n_folds){
    #generate pseudo_test and pseudo_train
    pseudo_test <- train %>%
      filter(fold == curr_fold)
    pseudo_train <- train %>%
      filter(fold != curr_fold)
    
    #define your outcome variable
    classifications_cv <- pseudo_train$interest_level
    
    #define predictor variables
    train_input_cv <- pseudo_train %>%
      select(bathrooms, bedrooms, longitude, latitude)
    test_input_cv <- pseudo_test %>%
      select(bathrooms, bedrooms, longitude, latitude)
    
    #train model
    model_knn_cv <-
      class::knn(train=train_input_cv, test=test_input_cv, cl=classifications_cv, k = k_vector[k_index], prob=TRUE)
    
    #calculate score
    pseudo_test <- pseudo_test %>%
      mutate(
        interest_predict = model_knn_cv,
        probability = attr(model_knn_cv, "prob"),
        correct = ifelse(interest_level != interest_predict, 1, 0)
      )
    score <- sum(pseudo_test$correct)
    
    #add score to total
    total_score <- total_score + score
  }
  
  #calculate avg score during cross validation
  avg_score <- total_score/n_folds
  avg_scores[k_index] = avg_score
  
}

#results of cv
cv_results <- data_frame(
  avg_score = avg_scores,
  k = k_vector
)

  
#calculate optimal k
optimal_k <- cv_results %>%
  arrange(avg_score) %>%
  slice(1) %>%
  .[['k']]

#clean test data
test <- test %>%
  mutate(
    bathrooms = jitter(unlist(bathrooms)),
    bedrooms = jitter(unlist(bedrooms)),
    longitude = unlist(longitude),
    latitude = unlist(latitude)
  )

#predictor data
input_train <- train %>%
  select(bathrooms, bedrooms, longitude, latitude)
input_test <- test %>%
  select(bathrooms, bedrooms, longitude, latitude)

#outcome data
classifications <- train$interest_level

#train model with optimal k
model_knn <-
  class::knn(train=input_train, test=input_test, cl=classifications, k = optimal_k, prob=TRUE, use.all = TRUE)

#class::knn() only outputs the 'winning' probability of knn. The Kaggle competition 
#submission asks for the probability of the losing values as well. To get around this,
#this model assumes that the probability of the two losing values were equal.
submission <- data.frame(
  listing_id = rep(0, nrow(test)),
  high = rep(0, nrow(test)),
  medium = rep(0, nrow(test)),
  low = rep(0, nrow(test)),
  prediction = model_knn,
  probability = attr(model_knn, "prob")
) %>%
  mutate(
    high = ifelse(prediction == 'high', probability, (1-probability)/2),
    medium = ifelse(prediction == 'medium', probability, (1-probability)/2),
    low = ifelse(prediction == 'low', probability, (1-probability)/2),
    listing_id = unlist(test$listing_id)
  ) %>%
  select(listing_id, high, medium, low) %>%
  write_csv("Rental_Listing_submission.csv")

