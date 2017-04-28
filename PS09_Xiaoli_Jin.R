library(tidyverse)
library(class)
color_palette <- "Dark2"

set.seed(12)

#due to the size of the dataset, my laptop is unable to run cv on the exntire dataset(It took hours and still couldn't give a result)
#Therefore, I select one-tenth of the original dataset as cv sample
train <- train %>% 
  sample_frac(0.1)

train_sf <- train %>%
  tbl_df() %>%
  select(Category,X,Y) 

train_sf <- train_sf %>% 
  dplyr::rename(Longitude = `X`,Latitude = `Y`)

#add some noise to eliminate ties 
train_sf <- train_sf %>% 
  dplyr::mutate(
    Longitude = Longitude + rnorm(n = nrow(train_sf), mean = 0, sd = 0.001),
    Latitude = Latitude + rnorm(n = nrow(train_sf), mean = 0, sd = 0.001)
  ) 

num_neigh <- 3
num_k <- 30
n_folds <- 5
train_sf <- train_sf %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

#classifications <- train$Category

Score_per_k <- rep(0, num_k)

for(k in 3:num_k +2) {

  Score_per_fold <- rep(0, n_folds) 
  
  for(j in 1:n_folds) {
      pseudo_train <- train_sf %>%
        filter(fold != j)
      
      classifications <- pseudo_train$Category 
      
      pseudo_train <- pseudo_train %>% 
        select(Longitude,Latitude)
      
      pseudo_test <- train_sf %>%
        filter(fold == j) %>% 
        select(Longitude,Latitude)
      
      print(j)
      
      model_knn <-
        class::knn(train=pseudo_train, test=pseudo_test, cl=classifications, k = k)
      
      print(j)
      
      Original <- train_sf %>%
        filter(fold == j) %>% 
        select(Category)
      
      print(j)
      
      pseudo_test <- pseudo_test %>%
        mutate(Prediction = model_knn) %>% 
        mutate(Category = Original$Category) 
      
      print("here")
      
      pseudo_test <- pseudo_test %>%
        mutate(correctness = ifelse(as.character(pseudo_test$Prediction)==pseudo_test$Category,1,0))
      
      print(j)
        
      Score_per_fold[j] <- mean(pseudo_test$correctness)
      print(j)
  }
  
  Score_per_k[k] <- mean(Score_per_fold)
  
}

#from the result of cross validation, the best k value is 32 
#Note: 32 is never the ideal k, due to the incredibly low speed of my laptop, however, 
#I decided to use 32 in this case 
#below is the prediction process

entire_train <-read.csv(file="train.csv", header=TRUE, sep=",")
real_test <- read.csv(file="test.csv", header=TRUE, sep=",")

classifications <- entire_train$Category

train_input <- entire_train %>%
  tbl_df() %>%
  select(X,Y) %>% 
  dplyr::rename(Longitude = `X`,Latitude = `Y`) %>% 
  dplyr::mutate(
    Longitude = Longitude + rnorm(n = nrow(entire_train), mean = 0, sd = 0.001),
    Latitude = Latitude + rnorm(n = nrow(entire_train), mean = 0, sd = 0.001)
  ) 

test_input <- real_test %>%
  select(X,Y) %>% 
  dplyr::rename(Longitude = `X`,Latitude = `Y`)

model_knn_final <-
  class::knn(train=train_input, test=test_input, cl=classifications, k = 32)

submission <- real_test %>%
  select(Id) %>% 
  mutate(Category = model_knn_final)

#sample_submission <- read.csv(file="sampleSubmission.csv", header=TRUE, sep=",")

#import sampleSubmission
#when I imported this dataset from one direct line of code, the column name changed.
#so I imported it by clicking the button (I know ideally, we should write reproducible code)

sampleSubmission <- sampleSubmission %>% 
  mutate(Category = submission$Category) 
  
sampleSubmission$WARRANTS = 0

submission_copy <- sampleSubmission
for(i in 2:(ncol(sampleSubmission)-1)) {
  submission_copy[i] <- ifelse(as.character(sampleSubmission$Category) == colnames(sampleSubmission)[i],1,0)
}


Xiaoli_Jin_submission <- submission_copy %>% 
  select(-Category)

write_csv(Xiaoli_Jin_submission,"PS09_Xiaoli_Jin_submission.csv")

