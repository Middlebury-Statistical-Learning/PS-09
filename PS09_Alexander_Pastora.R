library(tidyverse)
library(class)

# This leaderboard uses "Categorization Accuracy", which is literally just
# the percentage of correct guesses. 

# Preperation of the train and test data for K-Nearest Neighbors Model using the 
# Class R Package.

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train <- train %>% mutate(bone_length= as.numeric(bone_length)) %>% 
  mutate(rotting_flesh=as.numeric(rotting_flesh)) %>% 
  mutate(hair_length = as.numeric(hair_length)) %>% 
  mutate(has_soul = as.numeric(has_soul))

test <- test %>% mutate(bone_length= as.numeric(bone_length)) %>% 
  mutate(rotting_flesh=as.numeric(rotting_flesh)) %>% 
  mutate(hair_length = as.numeric(hair_length)) %>% 
  mutate(has_soul = as.numeric(has_soul))

classifications <- train$type

train_input <- train %>% 
  tbl_df() %>% 
  select(bone_length, rotting_flesh, hair_length, has_soul)

test_input <- test %>% 
  tbl_df %>% 
  select(bone_length, rotting_flesh, hair_length, has_soul)

# Determine the optimal K using the knn.cv function in the class package.
k_values <- as.vector(1:371)
n_folds <-10
train <- train %>% mutate(fold = rep(1:n_folds, length=n()))

for(i in 1:371)
  {
  scores <- as.vector(1:n_folds)
  for(j in 1:n_folds)
  {
    
    
    pseudo_train <- train %>%
      filter(fold != j)
    pseudo_test <- train %>%
      filter(fold == j)
    
    pseudo_train_input <- pseudo_train %>% 
      tbl_df() %>% 
      select(bone_length, rotting_flesh, hair_length, has_soul)
    
    pseudo_test_input <- pseudo_test %>% 
      tbl_df %>% 
      select(bone_length, rotting_flesh, hair_length, has_soul)
    
    model_knn <-knn(train=pseudo_train_input, test=pseudo_test_input, cl=pseudo_train$type, k=i)
    
    pseudo_test <- pseudo_test %>%
      mutate(
        guess = model_knn)
    pseudo_test <- pseudo_test %>% mutate(correct=ifelse(type==guess,1,0))
    
    scores[j] <- mean(pseudo_test$correct)
  }
  k_values[i] <-mean(scores)
}

max(k_values)
# This function finds the vector index associated with the max value, which
# is the optimal k value.
k_star <- match(max(k_values),k_values)


# Time to train the real model
model_knn <-knn(train=train_input, test=test_input, cl=classifications, k=k_star)
test <- test %>%
  mutate(
    type = model_knn
  )

predictions <- test %>% select(id,type )
write.csv(predictions, "predictions.csv")

# For some reason, write.csv generates and extra place-holder column that I
# manually erase.