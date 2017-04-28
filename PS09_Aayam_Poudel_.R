# Code based on: http://stackoverflow.com/questions/31234621/
library(tidyverse)
library(class)

setwd("~/Desktop/Junior Spring/Statistical Learning/Homework/HW9")

#Grab the data

#For testing:
#train <- read_csv("train.csv") %>% 
#  sample_n(1000)

train <- read_csv("train.csv") 
test <- read_csv("test.csv")
#All the warrants were labeled as 1, so I changed them to 0

sample_submission <- read_csv("sampleSubmission.csv") %>% mutate ("WARRANTS" = 0)


#We are going to build a simple model that predicts what kind of crime it is 
#based only on latitude and longitude
#I added a little bit of noise to X and Y, because I was getting errors, and that solved it

train <- train %>%
  select(X,Y, Category) %>%
  mutate(Id = seq_len(n())) %>%
  mutate(X = jitter(X), Y = jitter(Y))
View(train) 

test <- test %>%
  select(X,Y,Id) %>%
  mutate(X = jitter(X), Y = jitter(Y))
View(test)


#CROSS VALIDATION TO SEE THE OPTIMAL K

#Potential k values stores here
k_values <- seq(from=1 , to = 100)

#To store scores for each k_values
k_scores <- rep(0,100)


for(i in k_values){
  
  # Assign folds at random
  n_folds <- 10
  
  train <- train %>%
    # Equivalent to shuffling the rows:
    sample_frac(1) %>%
    mutate(fold = rep(1:n_folds, length=n()))
  
  #score for each iteration of the inner for loop
  instance_score <- rep(0, n_folds)
  
  #for each fold
  for(j in 1:n_folds){
    #disjoint train and test sets
    pseudo_train <- train %>%
      filter(fold != j)
    pseudo_test <- train %>%
      filter(fold == j)
    
    #classifications
    classifications <- pseudo_train$Category
    
    #pseudo train and test sets inputs
    pseudo_train_input <- pseudo_train %>%
      select(X, Y)
    
    pseudo_test_input <- pseudo_test  %>%
      select(X, Y) 
    
    #fit the model
    model_knn <-
      class::knn(train=pseudo_train_input, test=pseudo_test_input, cl=classifications, k = i, prob=TRUE)
    
    #Predict on the model
    pseudo_test <- pseudo_test %>%
      mutate(
        Prediction = model_knn,
        Probability = attr(model_knn, "prob")
      )
    
    #get score for each iteration of the for loop
    instance_score[j] <- pseudo_test %>%
      summarize(score = mean(Category != Prediction))
    
  }
  
  #Store the scores for each k values in a vector
  k_scores[i] <- as.numeric(instance_score) %>% mean()
  
}

#find the optimal k
k_star <- 0
prev_score = 100000
for (i in 1:length(k_scores)){
  if(k_scores[i] < prev_score){
    k_star = i
    prev_score = k_scores[i]
  }
  
}


#Fit model
# Define number of neighbors:
num_neigh <- k_star

# Assign categorical outcome variable:
classifications <- train$Category

# Note the training data and test data inputs to knn() have to be the same size
# and just be the predictor variables
train_input <- train %>%
  select(X, Y)

test_input <- test %>%
  select(X,Y) 

#Fit the model
Sys.time()
model_knn <-
  class::knn(train=train_input, test=test_input, cl=classifications, k = num_neigh, prob=TRUE)
Sys.time()


#Predict on the model
test <- test %>%
  mutate(
    Category = model_knn,
    Probability = attr(model_knn, "prob")
)

test <- test %>% select (Id, Category, Probability)

#I think I could have done this problem better. I could have used a model that gives me
#the probabilities for each of the different crimes. I could have even just gotten the
#probabilities for the models and stopped doing the knn there.
#But, the whole point of this assignment was to come up with the optimal k for knn
#and that is why I am putting the expected crime as its probability and all the crimes as 1 - probability 
#divided up equally among the rest of the variables here instead of putting their probabilities. 


#Convert this to the accepted format now 
#Could not find a better way of doing this, and I didn't have too much time
#I took the DONE APPROACH

final <- mutate(test, "ARSON"=ifelse(test$Category=="ARSON", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "ASSAULT"=ifelse(test$Category=="ASSAULT", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "BAD CHECKS"=ifelse(test$Category=="BAD CHECKS",test$Probability, (1-test$Probability)/38))
final <- mutate(final, "BRIBERY"=ifelse(test$Category=="BRIBERY", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "BURGLARY"=ifelse(test$Category=="BURGLARY", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "DISORDERLY CONDUCT"=ifelse(test$Category=="DISORDERLY CONDUCT", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "DRIVING UNDER THE INFLUENCE"=ifelse(test$Category=="DRIVING UNDER THE INFLUENCE", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "DRUG/NARCOTIC"=ifelse(test$Category=="DRUG/NARCOTIC", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "DRUNKENNESS"=ifelse(test$Category=="DRUNKENNESS", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "EMBEZZLEMENT"=ifelse(test$Category=="EMBEZZLEMENT", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "EXTORTION"=ifelse(test$Category=="EXTORTION", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "FAMILY OFFENSES"=ifelse(test$Category=="FAMILY OFFENSES", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "FORGERY/COUNTERFEITING"=ifelse(test$Category=="FORGERY/COUNTERFEITING", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "FRAUD"=ifelse(test$Category=="FRAUD", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "GAMBLING"=ifelse(test$Category=="GAMBLING", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "KIDNAPPING"=ifelse(test$Category=="KIDNAPPING", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "LARCENY/THEFT"=ifelse(test$Category=="LARCENY/THEFT", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "LIQUOR LAWS"=ifelse(test$Category=="LIQUOR LAWS", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "LOITERING"=ifelse(test$Category=="LOITERING", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "MISSING PERSON"=ifelse(test$Category=="MISSING PERSON", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "NON-CRIMINAL"=ifelse(test$Category=="NON-CRIMINAL", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "OTHER OFFENSES"=ifelse(test$Category=="OTHER OFFENSES", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "PORNOGRAPHY/OBSCENE MAT"=ifelse(test$Category=="PORNOGRAPHY/OBSCENE MAT", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "PROSTITUTION"=ifelse(test$Category=="PROSTITUTION", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "RECOVERED VEHICLE"=ifelse(test$Category=="RECOVERED VEHICLE", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "ROBBERY"=ifelse(test$Category=="ROBBERY", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "RUNAWAY"=ifelse(test$Category=="RUNAWAY", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "SECONDARY CODES"=ifelse(test$Category=="SECONDARY CODES", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "SEX OFFENSES FORCIBLE"=ifelse(test$Category=="SEX OFFENSES FORCIBLE", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "SEX OFFENSES NON FORCIBLE"=ifelse(test$Category=="SEX OFFENSES NON FORCIBLE", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "STOLEN PROPERTY"=ifelse(test$Category=="STOLEN PROPERTY", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "SUICIDE"=ifelse(test$Category=="SUICIDE", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "SUSPICIOUS OCC"=ifelse(test$Category=="SUSPICIOUS OCC", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "TREA"=ifelse(test$Category=="TREA", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "TRESPASS"=ifelse(test$Category=="TRESPASS", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "VANDALISM"=ifelse(test$Category=="VANDALISM", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "VEHICLE THEFT"=ifelse(test$Category=="VEHICLE THEFT", test$Probability, (1-test$Probability)/38))
final <- mutate(final, "WARRANTS"=ifelse(test$Category=="WARRANTS",test$Probability, (1-test$Probability)/38))
final <- mutate(final, "WEAPON LAWS"=ifelse(test$Category=="WEAPON LAWS", test$Probability, (1-test$Probability)/38))


final <- final %>% select(-Category,-Probability) 

final%>% write_csv("PS09_Aayam_Poudel_submission.csv")


