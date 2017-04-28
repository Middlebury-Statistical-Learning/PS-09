
library(tidyverse)
library(broom)
library(class)
library(kknn)

train <- dplyr::select(train_3, -Dates, -PdDistrict, -Descript, -Address)

n1 <- nrow(train)

train <- train %>%
  mutate(ID = 1:n1) %>%
  mutate(Resolution = ifelse(Resolution == "NONE", 0, 1)) %>%
  mutate(DayOfWeek = ifelse(DayOfWeek == "Monday", 1, ifelse(DayOfWeek == "Tuesday", 2, ifelse(DayOfWeek == "Wednesday", 3, ifelse(DayOfWeek == "Thursday", 4, ifelse(DayOfWeek == "Friday", 5, ifelse(DayOfWeek == "Saturday", 6, ifelse(DayOfWeek == "Sunday", 7, 8)))))))) %>%
  sample_frac(1)

test <- dplyr::select(test_2, -Dates, -PdDistrict, -Address)

n2 <- nrow(test)

test <- test %>%
  mutate(DayOfWeek = ifelse(DayOfWeek == "Monday", 1, ifelse(DayOfWeek == "Tuesday", 2, ifelse(DayOfWeek == "Wednesday", 3, ifelse(DayOfWeek == "Thursday", 4, ifelse(DayOfWeek == "Friday", 5, ifelse(DayOfWeek == "Saturday", 6, ifelse(DayOfWeek == "Sunday", 7, 8)))))))) %>%
  sample_frac(1)


View(test)
View(train)
train_2 <- dplyr::sample_n(train, 10000, replace = FALSE)
test_2 <- dplyr::sample_n(test, 10000, replace = FALSE)

# Fit model & get predictions --------------------------------------------------
# Define number of neighbors:
num_neigh <- 8

# Assign categorical outcome variable:
classifications <- train_2$Category

# Note the training data and test data inputs to knn() have to be the same size
# and just be the predictor variables
train_input <- train_2 %>%
  select(DayOfWeek, X, Y)
test_input <- test_2 %>%
  select(DayOfWeek, X, Y)

# Fit model:
model_knn <-
  class::knn(train=train_input, test=test_input, cl=classifications, k = num_neigh, prob=TRUE, use.all = FALSE)

# Add predictions and probabilities to test set
test_2 <- test_2 %>%
  mutate(
    Category = model_knn,
    Probability = attr(model_knn, "prob")
  )

View(test_2)

#below this is a few more things I tried, none worked.
# I was unable to get  my above model to work for more than a few tens of thousands of 
#of rows, it kept crashing or having too many ties (despite using a low k or use.all=FALSE)
#So i was unable to submit a test (over 800,000 rows) to kaggle







i <- 100000

pseudo_train <- dplyr::slice(train, 100001:878049)
pseudo_test <- dplyr::slice(train, 1:100000)

pre_train <- dplyr::sample_n(train, 1000, replace=FALSE)
View(pre_train)
suppressWarnings(suppressMessages(library(kknn)))

model <- train.kknn(Category ~ DayOfWeek + Resolution + X + Y, data = pre_train, kmax = 5)

knn(pseudo_train, pseudo_test, pseudo_train$Category, k = 9)

model




prediction <- knn(pseudo_train, pseudo_test, pseudo_train$Category, k = 1)


test <- expand.grid(DayOfWeek = seq(1, 7), Resolution = seq(0, 1), X = seq(-122.5136, -120.5000, by=0.0001), Y = seq(37.70788, 90.0000, by=0.00001)) %>%
  tbl_df()


#predictors <- cbind(lag(stocks$Apple, default = 210.73), lag(stocks$Google, default = 619.98), lag(stocks$MSFT, default = 30.48))



data(train)
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
knn3Train(train, test, cl, k = 5, prob = TRUE) 

