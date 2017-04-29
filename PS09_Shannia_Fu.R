# Packages
library(dplyr)
library(tidyverse)
library(broom)
library(plotly)
library(glmnet)
library(ggplot2)
library(knncat)
library(knn)
library(class)

# Load training data
train <- read_csv("crime-train.csv")
# Load test data
test <- read_csv("crime-test.csv")
# Changes var types to factor not char
cols <- sapply(train, is.character)
chrCols <- names(train[, cols])
train[cols] <- lapply(train[chrCols], factor)
cols <- sapply(test, is.character)
chrCols <- names(test[, cols])
test[cols] <- lapply(test[chrCols], factor)

# This should theoretically work but it's crashing my computer so I
# don't know if it works
# model <- knncat(train, test, classcol = 2)

# So I just used numerical vars instead
# i.e. just lat/long (X, Y)

# neighbors to test for cv
neighbors <- seq.int(1, 200)
# set min error to inf first so that you can compare for min error later
min_error = Inf
# set k to be 0 but use min error to set k within cv loop
final_k = 0

# for all the kneighbors
for (i in neighbors) {
  # for all the sections of cv
  cv <- seq.int(0, 4)
  # record the error in each cv
  error <- c()
  for (j in cv) {
    # set pseudo test and train
    pseudo_test <- train[(j*nrow(train)/5 + 1):((j+1)*(nrow(train)))/5,]
    pseudo_train <- anti_join(train, pseudo_test)
    
    # select only lat/long
    train_input <- pseudo_train %>% 
      select(X, Y)
    test_input <- pseudo_test %>%
      select(X, Y)
    
    # set outcome var
    classifications <- pseudo_train$Category
    
    # train model
    model_knn <-
      class::knn(train=train_input, test=test_input, cl=classifications, k = i, prob=TRUE)
    
    # add prediction to pseudo test
    pseudo_test <- pseudo_test %>%
      mutate(
        predCategory = model_knn,
        Probability = attr(model_knn, "prob"),
        is_equal = ifelse(predCategory != Category, 1, 0)
      )
    
    # add the error to this cv
    error <- c(error, mean(pseudo_test$predCategory != pseudo_test$Category))
  }
  
  # average the errors in this cv
  # if it's less than the min error, then choose this as current final k
  if (mean(error) < min_error) {
    min_error = mean(error)
    final_k = i
  }
  
}

# I keep getting the error "too many ties in knn"
# but this should theoretically work sans any syntax errors
# So I didn't submit anything to Kaggle
# hope that's ok
# esp cause I already got the extension
# also I'm writing this from outside the concert hall


# commenting for this should be the same as above
classifications <- train$Category
model_knn <-
  class::knn(train=train, test=test, cl=classifications, k = final_k, prob=TRUE)
test <- test %>%
  mutate(
    predCategory = model_knn,
    Probability = attr(model_knn, "prob")
  )

# this would theoretically be the submission to Kaggle
output <- spread(test, predCategory, Probability)
write.csv(output, file = "crime_submission.csv", sep = ",")

