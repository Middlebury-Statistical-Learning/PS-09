# Load Packages ----------------------------------------------------------------
library(tidyverse)
library(broom)
library(plotly)
library(glmnet)
# Package for string manipulation:
library(stringr)
library(lubridate)
library(ggplot2)
library(class)


train <- read_csv("train crime.csv")
finaltest <- read_csv("test crime.csv")
sample <- read_csv("sampleSubmission Crime.csv")


train <- train %>%
  select(X,Y, Category) %>%
  mutate(Id = seq_len(n())) %>%
  #needed to add some noise
  mutate(X = jitter(X)) %>%
  mutate(Y = jitter(Y))

finaltest <- finaltest %>%
  select(X,Y,Id) %>%
  #needed to add some noise
  mutate(X = jitter(X)) %>%
  mutate(Y = jitter(Y))



# Fit model & get predictions --------------------------------------------------
# Define number of neighbors:
num_neigh <- seq(from = 2, to = 100)
k_score <- rep(0,99)

# Assign categorical outcome variable:
classifications <- train$Category


#best knn, couldn't figure this out
cvfit <- knn.cv(train, cl = classifications, k=num_neigh, prob = TRUE)


# Fit model:
model_knn <- class::knn(train=train, test=finaltest, cl=classifications, k = 3, prob=TRUE)


# Add predictions and probabilities to test set
final <- finaltest %>%
  mutate(
    Category = model_knn,
    Probability = attr(model_knn, "prob")
  )

sub <- test %>%
  spread(ID, Category)



