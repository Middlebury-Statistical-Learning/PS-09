library(rpart)
library(tidyverse)
library(broom)
# I got a lot of help from Aayam on this one because I missed class on Monday
#i also got a lot of code from knn.r
#I know this wasn't finished- see note at end

#---load data -- 
crime_train<- read_csv("~/Google Drive/Math/crimetrain.csv")
crime_test<- read_csv("~/Google Drive/Math/crimetest.csv")
sample_sub <- read_csv("~/Google Drive/Math/sampleSubmission.csv") %>% mutate ("WARRANTS" = 0)


View(crime_train)
glimpse(crime_test)
glimpse(sample_sub)

crime_train <- crime_train %>%
  tbl_df() %>%
  #i also did a simple model like aayam
  select(Category, X, Y) %>%
  #add an ID field since theres only one on test
  mutate(Id = seq_len(n()))  %>%
  mutate(X = jitter(X), Y = jitter(Y))

crime_test <- crime_test %>%
  mutate(X = jitter(X), Y = jitter(Y)) %>%
  select(X,Y,Id)
  
#--CV--
k_val <- seq(from = 1, to = 50)
k_score <- numeric(50)



for (i in k_val) {
  n_folds <- 10 
  train <- crime_train %>%
    #this was to make it go faster
  sample_frac(0.5)
  train <- train %>%
  mutate(fold = sample(1:n_folds, replace=TRUE, size=nrow(train)))
  
  score<- numeric(length(n_folds))
  
  for(j in n_folds){
    pseudo_train <- train %>%
      filter(fold != i)
    pseudo_test <- train %>%
      filter(fold == i)
    
    #make inputs (this came from knn.r)
    classifications <- pseudo_train$Category
    pseudo_train_input <- pseudo_train %>%
      select(X, Y)
    pseudo_test_input <- pseudo_test  %>%
      select(X, Y) 
    
    #fit the model 
    model_knn <-
      class::knn(train=pseudo_train_input, test=pseudo_test_input, cl=classifications, k = i, prob=TRUE)
    
    #Add predictions and probabilities to test set
    pseudo_test <- pseudo_test %>%
      mutate(
        Prediction = model_knn,
        Probability = attr(model_knn, "prob")
      )
    score[i] <- pseudo_test %>%
      summarize(score = mean(Category != Prediction))
  }
  
  k_score[i] <- as.numeric(score) %>% mean()  
  if(i %% 10 == 0){
    print(j) }
}

#find optimal
k_star <- 0
prev_score = 100000
for (i in 1:length(k_score)){
  if(k_score[i] < prev_score){
    k_star = i
    prev_score = k_score[i]
  }
  
}
k_star

#make real predictions!

classifications <- train$Category
train_input <- train %>%
  select(X, Y)
test_input <- crime_test  %>%
  select(X, Y) 

#fit the model 
model_knn <-
  class::knn(train=train_input, test=test_input, cl=classifications, k = k_star, prob=TRUE)

#Add predictions and probabilities to test set
pseudo_test <- pseudo_test %>%
  mutate(
    Prediction = model_knn,
    Probability = attr(model_knn, "prob")
  )

#This was taking super long and R kept crashing on me so I'm stopping here and submitting what I have
#I know it's incomplete but I have too many other things to focus on
#I talked to Aayam about his strategy for submission. I understand what I should have done. 