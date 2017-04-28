#Set working directory
setwd("~/Dropbox/Middlebury/Semesters/9. Spring 2017/Statistical Learning/Assignments/#9")

# Code based on: http://stackoverflow.com/questions/31234621/
library(tidyverse)
library(class)

# Color palette used for plotting points chosen from:
# http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
color_palette <- "Dark2"

# We set the random number generator seed value to get replicable randomness
set.seed(76)

# Load the iris data:
train <- readr::read_csv("train.csv") 

time_in_weeks_vector <- rep(0, dim(train)[1])
for(i in 1:26729) {
  str <- train$AgeuponOutcome[i]
  double <- strsplit(str, " ")
  num <- strtoi(double[[1]][1])
  time <- double[[1]][2]
  time_in_num_weeks <- NULL
  if(!is.na(time)) {
  if(time == "day" || time == "days") {
    time_in_num_weeks <- (1/7)
  }
  if(time == "week" || time == "weeks") {
    time_in_num_weeks <- 1
  }
  if(time == "month" || time == "months") {
    time_in_num_weeks <- 4
  }
  if(time == "year" || time == "years") {
    time_in_num_weeks <- 52
  }
  }
  else {
    time_in_num_weeks <- 0
  }
  result <- num * time_in_num_weeks
  time_in_weeks_vector[i] = result

}

SexuponOutcome <- train$SexuponOutcome
OutcomeType <- train$OutcomeType

train <- data.frame(time_in_weeks_vector, SexuponOutcome, OutcomeType) %>% 
  tbl_df() %>%
  mutate(
    SexuponOutcome = match(SexuponOutcome, c("NA", "Unknown","Spayed Female", "Neutered Male", "Intact Male", "Intact Female")) - 1
  )
View(train)
dim(train)
train <- subset(train, !is.na(time_in_weeks_vector))
train <- subset(train, !is.na(SexuponOutcome))
dim(train)
# Define training set -----------------------------------------------------

ggplot(train, aes(x=time_in_weeks_vector, y=SexuponOutcome, col=OutcomeType)) +
  geom_point() +
  scale_color_brewer(palette = color_palette) +
  labs(title="Training Data")

# Unfortunately there is a little overplotting. So we use geom_jitter to add a
# little random noise so we can see situations when points are plotted on top
# of each other:
ggplot(train, aes(x=time_in_weeks_vector, y=SexuponOutcome, col=OutcomeType)) +
  geom_jitter(width = 0.05, height = 0.05) +
  scale_color_brewer(palette = color_palette)  +
  labs(title="Training Data with Random Noise Added")



# We define our grid using expand.grid(). We will make predictions for each of
# these points
test <- expand.grid(
  time_in_weeks_vector = seq(0, 1100, by=20),
  SexuponOutcome = seq(1, 7, by=1)
) %>%
  tbl_df()

ggplot(test, aes(x=time_in_weeks_vector, y=SexuponOutcome)) +
  geom_point(size=0.5) +
  labs(title="Test Data (Grid)")




# Fit model & get predictions --------------------------------------------------
# Define number of neighbors:
num_neigh <- 3

# Assign categorical outcome variable:
classifications <- train$OutcomeType

# Note the training data and test data inputs to knn() have to be the same size
# and just be the predictor variables
train_input <- train %>%
  select(time_in_weeks_vector, SexuponOutcome)
test_input <- test %>%
  select(time_in_weeks_vector, SexuponOutcome)

# Fit model:
model_knn <-
  class::knn(train=train_input, test=test_input, cl=classifications, k = num_neigh, prob=TRUE)

# Add predictions and probabilities to test set
test <- test %>%
  mutate(
    Species = model_knn,
    Probability = attr(model_knn, "prob")
)




# Basic plots ------------------------------------------------------------
# The larger points represent the training data, the smaller points the
# test data (i.e. the grid)
ggplot(test, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) +
  geom_point(size=1) +
  geom_jitter(data=train, size=3) +
  scale_color_brewer(palette = color_palette)  +
  labs(title=paste("k =", num_neigh, "Nearest Neighbors with Prediction Points"))

# The following is the same plot as above, but using geom_tile() instead of
# geom_point() to denote the prediction for each grid point:
ggplot(test, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_tile(aes(fill=Species), alpha=0.5) +
  geom_jitter(data=train, aes(col=Species), size=3) +
  scale_fill_brewer(palette = color_palette)  +
  scale_color_brewer(palette = color_palette)  +
  labs(title=paste("k =", num_neigh, "Nearest Neighbors with Prediction Tiles"))



# Plots that incorporate uncertainty --------------------------------------
# The above isn't great. I like the following plot most. Note the training set
# data are denoted with points with:
# -shape=21 are points with an outline color and a fill color
# -stroke controls the size of the outline
ggplot(test, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_tile(aes(fill=Species, alpha=Probability)) +
  geom_jitter(data=train, aes(fill=Species), size=3, shape=21, stroke=0.5) +
  scale_fill_brewer(palette = color_palette)  +
  labs(title=paste("k =", num_neigh, "Nearest Neighbors with Prediction Tiles"))




# Exercise ---------------------------------------------------------------
# Play around with k as reflected in num_neigh paying attention to the
# probabilities!
# https://beta.rstudioconnect.com/connect/#/apps/2683/
# Try to explain any "weird" behavior

