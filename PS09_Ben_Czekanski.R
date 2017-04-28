setwd("~/Documents/Math218/PS09 Leaf Classification")

library(tidyverse)
library(class)
library(caret)
library(plotly)
train <- read.csv("train.csv")
test <- read.csv("test.csv")
sample_submission <- read.csv("sample_submission.csv")

set.seed(24)

folds <- 10

num_neighbors <- 300

k_scores <- rep(0, (num_neighbors))

for (j in 1:num_neighbors){

fold_scores <- rep(0, folds)

train_ <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:folds, length=n())) %>%
  arrange(fold) 
# %>%
#   select(id, species, fold)

for(i in 1:folds){
  pseudo_test <- train_ %>%
    filter(fold == i)
  pseudo_train <- train_ %>%
    filter(fold != i)
  

c <- knn3Train(pseudo_train[, -c(1,2, 195)], pseudo_test[, -c(1,2, 195)], pseudo_train$species, 
               k = j, prob = TRUE)

fold_scores[i] <- attr(c, "prob") %>%
  tbl_df() %>%
  mutate(correct = as.character(pseudo_test$species),
         id = as.character(pseudo_test$id)) %>%
  gather(prediction, prob, Acer_Capillipes:Zelkova_Serrata) %>%
  filter(correct == prediction) %>%
  mutate(prob = ifelse(prob <= 10^(-15), 10^(-15), prob),
         prob = ifelse(prob >= (1 - 10^(-15)), 1 - 10^(-15), prob),
         log_prob = log(prob)) %>%
  summarize(log_loss = -mean(log_prob))

}

k_scores[j] <- mean(as.numeric(fold_scores))

if(j %% 10 == 0){
  print(j)
}

}

k_scores <- k_scores %>%
  tbl_df() %>%
  mutate(k = 1:num_neighbors) 

ggplot(k_scores, aes(x = k, y = value)) +
  geom_point()



a <- knn3Train(train[, -c(1,2, 195)], test[, -c(1)], train$species, k = 10, prob = TRUE) 

b <- attr(a, "prob") %>%
  tbl_df()

b$id <-  test$id

write_csv(b,"Submission_fifteen")


