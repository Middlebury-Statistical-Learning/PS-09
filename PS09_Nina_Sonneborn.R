# Problem Set 9
# Kaggle: Shelter Animal Outcomes
# https://www.kaggle.com/c/shelter-animal-outcomes/

# Load data and setup ------------
library(tidyverse)
library(broom)
library(caret)
library(MLmetrics)

train <- read_csv('PS09_train.csv')
test <- read_csv('PS09_test.csv')


# Data Cleaning -----------------------------------------

# While caret::knn3() allows missing values, it is easier
# to make variables numeric if NA values are gone
train[is.na(train)] <- "Unknown"
test[is.na(test)] <- "Unknown"

# knn doesn't allow for categorical predictors
# `knncat()` exists for categorical vars but seems sort of silly, 
# as this method is based on notion of distance.

# First make predictor variables numerical for training set:

# Fixing the age to be numerical (ugh)
age_nums <- ifelse(train$AgeuponOutcome == "Unknown", NA,
                   as.integer(substr(train$AgeuponOutcome, 0, 1)))

age_units <- ifelse(grepl("year", train$AgeuponOutcome), 365, 
                    ifelse(grepl("month",train$AgeuponOutcome), 30,
                           ifelse(grepl("weeks", train$AgeuponOutcome), 7, 1)))
days_old <- age_nums*age_units


# Binary vars are okay to make numerical. I code missing values
# as 0.5 because that is halfway between the two possibilities - 
# as if not putting emphasis on that variable.
train <- train %>%
  mutate(
    # Fixed: 0 = No, 1 = Yes, 0.5 = Unknown
    fixed = ifelse(SexuponOutcome == "Unknown", 0.5, 
                   ifelse(SexuponOutcome %in% c("Spayed Female", "Neutered Male"), 1, 0)),
    # Gender: 0 = male, 1 = female, 0.5 = Unknown
    gender = ifelse(SexuponOutcome %in% c("Intact Female", "Spayed Female"), 1, 
                    ifelse(SexuponOutcome == "Unknown", 0.5, 0)),
    # AnimalType: 0 = Cat, 1 = Dog
    AnimalType = match(AnimalType, c("Dog", "Cat")) - 1,
    # Black: 0 = No, 1 = Yes
    black = ifelse(Color == "Black", 1, 0),
    # Days old
    days_old = as.numeric(days_old)
  )

train[is.na(train)] <- mean(train$days_old, na.rm = TRUE)


# View data/EDA
train %>%
  group_by(OutcomeType) %>%
  summarise(mean(black), mean(days_old), mean(AnimalType), mean(gender), mean(fixed))

# Based on this, black and gender are not great predictors, so I wont use 
# them in my model


# Now for the test set:

# Fixing the age to be numerical...
age_nums_test <- ifelse(test$AgeuponOutcome == "Unknown", NA,
                        as.integer(substr(test$AgeuponOutcome, 0, 1)))

age_units_test <- ifelse(grepl("year", test$AgeuponOutcome), 365, 
                         ifelse(grepl("month",test$AgeuponOutcome), 30,
                                ifelse(grepl("weeks", test$AgeuponOutcome), 7, 1)))
days_old_test <- age_nums_test*age_units_test

test <- test %>%
  mutate(
    # Fixed: 0 = No, 1 = Yes, 0.5 = Unknown
    fixed = ifelse(SexuponOutcome == "Unknown", 0.5, 
                   ifelse(SexuponOutcome %in% c("Spayed Female", "Neutered Male"), 1, 0)),
    # Gender: 0 = male, 1 = female, 0.5 = Unknown
    gender = ifelse(SexuponOutcome %in% c("Intact Female", "Spayed Female"), 1, 
                    ifelse(SexuponOutcome == "Unknown", 0.5, 0)),
    # AnimalType: 0 = Cat, 1 = Dog
    AnimalType = match(AnimalType, c("Dog", "Cat")) - 1,
    # Black: 0 = No, 1 = Yes
    black = ifelse(Color == "Black", 1, 0),
    # Days old
    days_old = as.numeric(days_old_test)
  )


test[is.na(test)] <- mean(test$days_old, na.rm = TRUE)

# Fit dummy model: -----------------------------

# To resolve "too many ties" error
train <- train %>%
  mutate(days_old = jitter(days_old))

# Define number of neighbors:
num_neigh <- 10

model_knn <-
  knn3(OutcomeType ~ days_old + AnimalType + fixed, data=train, k = num_neigh)

# k-fold Cross validation to find optimal k in knn model ---------------

k_neighbors <- seq(10, 300, by = 10)
k_scores <- rep(0, length(k_neighbors))

# Assign folds at random
n_folds <- 10
train <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

for(j in 1:length(k_neighbors)){
  
  # Store your scores here:
  scores <- rep(0, n_folds)
  
  for(i in 1:n_folds){
    # 1. Create disjoint pseudo-train and pseudo-test sets based on folding
    # scheme.
    pseudo_train <- train %>%
      filter(fold != i)
    pseudo_test <- train %>%
      filter(fold == i)
    
    # 2. Train the model using (i.e. fit the model to) the pseudo_train data.
    # set up the input for the model
    model_knn_loop <-
      knn3(OutcomeType ~ days_old + AnimalType + fixed,
           data=pseudo_train, k =  k_neighbors[j])
    
    # 3. Get prediction for the pseudo_test data using the trained model
    probabilities <- predict(model_knn_loop, newdata=pseudo_test, type=c("prob", "class"))
    
    # 4. Compute your score on the pseudo_test data
    # Using Multi
    score <- MultiLogLoss(y_true = as.factor(pseudo_test$OutcomeType),
                          y_pred = probabilities)
    
    # 5. Save your score for this fold
    scores[i] <- score
  }
  # Store mean cross-validated score for this value of k neighbors
  k_scores[j] <- mean(scores)
  
  # Print statement to view progress of loop
  if (j%%1 == 0){
    print(j)
  }
}

# Create data frame with all test values of k and
# corresponding MultiLogLoss score
scores_per_k <- k_neighbors %>% 
  tbl_df() %>% 
  mutate(score = k_scores) %>%
  rename(k = value)

scores_per_k %>%
  ggplot(aes(x = k, y = score)) + geom_point()

# First tried k_neighbors <- seq(10, 300, by = 10) and found it was best
# between 200 and 250
# That's why I narrowed search range

# Optimal k turn out to be 280

optimal_k <- 280

# Add predictions and probabilities to test set -------------------

knn_optimal <- knn3(OutcomeType ~ days_old + AnimalType + fixed, train, k = optimal_k)
probabilities <- 
  predict(knn_optimal, newdata=test, type=c("prob", "class")) %>% 
  tbl_df()
submission <- 
  bind_cols(tbl_df(test$ID), probabilities) %>% 
  rename(ID = value)

write_csv(submission, "PS09_submission_caret.csv")

# Your submission scored 0.95968.