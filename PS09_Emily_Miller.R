# Emily Miller
# PS09
# Accelerometer Competition

# Load Packages ----------------------------------------------------------------
library(tidyverse)
library(broom)
library(class)
library(caret)
library(ISLR)


setwd("C:/Users/Emily Miller/Documents/Stat_learning")
train <- readr::read_csv("CSVs/crime_train.csv")


# Data exploration and cleaning
head(train)
train <- select(train, Category, Dates, DayOfWeek, PdDistrict, X, Y)


# Lets plot X vs Y with Category
color_palette <- "Dark2"
ggplot(train, aes(x=train$X, y=train$Y, col= train$Category)) +
  geom_point() +
  labs(title="Training Data")

# There is an outlier that makes this plot awful
train <- filter(train, train$X < -121)
ggplot(train, aes(x=train$X, y=train$Y, col= train$Category)) +
  geom_point() +
  labs(title="Training Data")

# plot usoing geom_jitter to see all points
ggplot(train, aes(x=train$X, y=train$Y, col=Category)) +
  geom_jitter(width = 0.05, height = 0.05) +
  labs(title="Training Data with Random Noise Added")

# Define test set ---------------------------------------------------------
# We'll define the test set to be a grid of values that encompasses the range
# of both sepal length and width

# Actual range of values:
range(train$X)
# Desired grid width:
range(train$X) + c(-0.05, 0.05)

# Actual range of values:
range(train$Y)
# Desired grid width:
range(train$Y) + c(-0.05, 0.05)

# We define our grid using expand.grid(). We will make predictions for each of
# these points
test <- expand.grid(
  X = seq(-122.56, -122.31, by=0.01),
  Y = seq(37.66, 37.87, by=0.01)
) %>%
  tbl_df()

ggplot(test, aes(x=X, y=Y)) +
  geom_point(size=0.5) +
  labs(title="Test Data (Grid)")

# Fit model & get predictions --------------------------------------------------
# Define number of neighbors:
num_neigh <- 3

# Assign categorical outcome variable and make train much smaller to prevent ties:
train_input <- train %>%
  sample_frac(0.05, replace = TRUE)
classifications <- train_input$Category

# Note the training data and test data inputs to knn() have to be the same size
# and just be the predictor variables
train_input <- train_input %>%
  select(X, Y)
test_input <- test %>%
  select(X, Y)

# Fit model:
model_knn <-
  class::knn(train=train_input, test=test_input, cl=classifications, k = num_neigh, prob=TRUE)

# Add predictions and probabilities to test set
test <- test %>%
  mutate(
    Category = model_knn,
    Probability = attr(model_knn, "prob")
  )

# Basic plots ------------------------------------------------------------
# The larger points represent the training data, the smaller points the
# test data (i.e. the grid)
ggplot(test, aes(x=X, y=Y, col=Category)) +
  geom_point(size=1) +
  geom_jitter(data=train, size=3) +
  labs(title=paste("k =", num_neigh, "Nearest Neighbors with Prediction Points"))

# The following is the same plot as above, but using geom_tile() instead of
# geom_point() to denote the prediction for each grid point:
ggplot(test, aes(x=X, y=Y)) +
  geom_tile(aes(fill=Category), alpha=0.5) +
  geom_jitter(data=train, aes(col=Category), size=3) +
  labs(title=paste("k =", num_neigh, "Nearest Neighbors with Prediction Tiles"))

# Find best K value through cross validation

# Make smaller train set to prevent issues with ties
sample_train <- sample_frac(train, 0.01, replace = TRUE)

# Create list of k values
klist = seq(1,33,2)

# Create folds
n_folds <- 5
sample_train <- sample_train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

# Create place to store wrong classification percentage
Percent_correct_for_K <- 0
list_correct <- 0


for (j in klist){
  num_neigh <- j
  
    for(i in 1:n_folds){
      # 1. Create disjoint pseudo-train and pseudo-test sets based on folding
      # scheme
      pseudo_train <- sample_train %>%
        filter(fold != i)
      pseudo_test <- sample_train %>%
        filter(fold == i)
      
      # 2. Train the model using (i.e. fit the model to) the pseudo_train data.
      classifications <- pseudo_train$Category
      
      # Make training data and test data inputs to knn() have to be the same size
      # and just be the predictor variables
      pseudo_train_input <- pseudo_train %>%
        select(X, Y)
      pseudo_test_input <- pseudo_test %>%
        select(X, Y)
      
      # Fit model:
      model_knn <-
        class::knn(train=pseudo_train_input, test=pseudo_test_input, cl=classifications, k = num_neigh, prob=TRUE)
      
      # 4. Compute your score on the pseudo_test data and save your score for this
      # fold.
      pseudo_test <- pseudo_test %>%
        mutate(
          Predict_Category = model_knn,
          Probability = attr(model_knn, "prob"),
          Correct_Match = charmatch(Category, Predict_Category, nomatch = 0)
        )
      
      percent_correct = sum(pseudo_test$Correct_Match)/ length(pseudo_test$Correct_Match)
      list_correct[i] <- percent_correct
    }
  
  index <- (j/2) + .5
  Percent_correct_for_K[index] <- mean(list_correct)
  
}

plot(klist, Percent_correct_for_K, "b")

# This isn't a very good model, but the best K value is at 17

# Crate predictions for test data set
rm(list=setdiff(ls(), "train"))
test <- readr::read_csv("CSVs/crime_test.csv")
test <- select(test, Id, Dates, DayOfWeek, PdDistrict, X, Y)
num_neigh <- 17

# Assign categorical outcome variable and make train much smaller to prevent ties:
train_input <- train %>%
  sample_frac(0.01, replace = TRUE)
classifications <- train_input$Category

# Note the training data and test data inputs to knn() have to be the same size
# and just be the predictor variables
train_input <- train_input %>%
  select(X, Y)
test_input <- test %>%
  select(X, Y)

# Fit model:
model_knn <-
  class::knn(train=train_input, test=test_input, cl=classifications, k = num_neigh, prob=TRUE)

# Add predictions and probabilities to test set
test <- test %>%
  mutate(Category = model_knn) %>%
  select(Id, Category)

# Formatt Data in the way Kaggle Wants
Kaggle_CSV <- data.frame(test$Id, test$Category)
namevector <- sort(unique(train$Category))
for(i in namevector) {
  Kaggle_CSV[,i] <- 0
}
Kaggle_CSV <- Kaggle_CSV[ , order(names(Kaggle_CSV))]
Kaggle_CSV <- Kaggle_CSV %>%
  select(test.Id, test.Category, everything())

nrows = as.numeric(length(Kaggle_CSV$test.Id))

for (i in 1:nrows){
  row_name <- as.character(Kaggle_CSV$test.Category[i])
  index <- as.numeric(which(names(Kaggle_CSV)==row_name))
  Kaggle_CSV[i, index] <- 1
  X <- as.numeric(unlist(strsplit(as.character(i), "")))
  X <- tail(X, 3)
  if (sum(X) == 0) {
    print(i)
  }
}

Kaggle_CSV = subset(Kaggle_CSV, select = -c(test.Category) )
Kaggle_CSV <- Kaggle_CSV %>%
  rename(Id = test.Id)

write.csv(Kaggle_CSV, file = "PS09_Predictions.csv")
