
#Brenda Li
#Problem Set 09
#KNN Neightbors: Shelter Outcomes


# Loading all libraries
library(dplyr)
library(caret)
library(knncat)
library(e1071)
library(stringr)
library(MLmetrics)

# Loading the train and test datasets
shelter_train<-read.csv("Shelter_train.csv.gz")
shelter_test<-read.csv("shelter_test.csv.gz")


# PART 1: CLEANING DATASETS

# Selecting only relevant variables and filtering out NA's
clean_test<-shelter_test %>% select(AnimalType,SexuponOutcome,AgeuponOutcome) %>% na.omit()
clean_train<-shelter_train %>% select(OutcomeType,AnimalType,SexuponOutcome,AgeuponOutcome) %>% na.omit()


# Mutating variables to be numerical rather than categorical for KNN (haven't figured out how to use knncat yet)
clean_train$AgeuponOutcome<-as.character(clean_train$AgeuponOutcome)
clean_test$AgeuponOutcome<-as.character(clean_test$AgeuponOutcome)

clean_train<-clean_train %>% 
  mutate(AnimalTypeInt=ifelse(AnimalType=="Dog",1,0),
         SexuponOutcomeInt=ifelse(SexuponOutcome=="Intact Female",1,SexuponOutcome),
         SexuponOutcomeInt=ifelse(SexuponOutcome=="Intact Male",2,SexuponOutcomeInt),
         SexuponOutcomeInt=ifelse(SexuponOutcome=="Spayed Female",3,SexuponOutcomeInt),
         SexuponOutcomeInt=ifelse(SexuponOutcome=="Neutered Male",4,SexuponOutcomeInt),
         SexuponOutcomeInt=ifelse(SexuponOutcome=="Unknown",0,SexuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="months", as.integer(word(AgeuponOutcome, 1))*4, 0),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="month", as.integer(word(AgeuponOutcome, 1))*4, AgeuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="weeks", as.integer(word(AgeuponOutcome, 1)), AgeuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="week", as.integer(word(AgeuponOutcome, 1)), AgeuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="years", as.integer(word(AgeuponOutcome, 1))*52, AgeuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="days", as.integer(word(AgeuponOutcome, 1))/7, AgeuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="year", as.integer(word(AgeuponOutcome, 1))*52, AgeuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="day", as.integer(word(AgeuponOutcome, 1))/7, AgeuponOutcomeInt)) %>% 
  select(OutcomeType,AnimalTypeInt,SexuponOutcomeInt,AgeuponOutcomeInt)

clean_test<-clean_test %>% 
  mutate(AnimalTypeInt=ifelse(AnimalType=="Dog",1,0),
         SexuponOutcomeInt=ifelse(SexuponOutcome=="Intact Female",1,SexuponOutcome),
         SexuponOutcomeInt=ifelse(SexuponOutcome=="Intact Male",2,SexuponOutcomeInt),
         SexuponOutcomeInt=ifelse(SexuponOutcome=="Spayed Female",3,SexuponOutcomeInt),
         SexuponOutcomeInt=ifelse(SexuponOutcome=="Neutered Male",4,SexuponOutcomeInt),
         SexuponOutcomeInt=ifelse(SexuponOutcome=="Unknown",0,SexuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="months", as.integer(word(AgeuponOutcome, 1))*4, 0),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="month", as.integer(word(AgeuponOutcome, 1))*4, AgeuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="weeks", as.integer(word(AgeuponOutcome, 1)), AgeuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="week", as.integer(word(AgeuponOutcome, 1)), AgeuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="years", as.integer(word(AgeuponOutcome, 1))*52, AgeuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="days", as.integer(word(AgeuponOutcome, 1))/7, AgeuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="year", as.integer(word(AgeuponOutcome, 1))*52, AgeuponOutcomeInt),
         AgeuponOutcomeInt=ifelse(word(AgeuponOutcome,-1)=="day", as.integer(word(AgeuponOutcome, 1))/7, AgeuponOutcomeInt)) %>% 
  select(AnimalTypeInt,SexuponOutcomeInt,AgeuponOutcomeInt)


# STEP 2: USING CV TO FIND OPTIMAL K

# making a smaller training dataset since using too large of a training set results in too many ties
mini_train<-sample_frac(clean_train,0.75) %>% select(OutcomeType, AnimalTypeInt,SexuponOutcomeInt,AgeuponOutcomeInt)

# setting up CV
mini_train <-mini_train[sample(nrow(mini_train)),]
n_folds <- 5
mini_train_cv <- mini_train%>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

# testing out different k_values using CV 
k_values<-seq(1,300,20)
best_k<-rep(0,n_folds)
for (i in 1:n_folds){
  best_error<-Inf
  pseudo_train<-mini_train_cv %>% filter(fold!=i)
  pseudo_test<-mini_train_cv %>% filter(fold==i)
  for (k in k_values){
    knn_fit_CV<-knn3Train(train=select(pseudo_train,-OutcomeType),test=select(pseudo_test,-OutcomeType),cl=pseudo_train$OutcomeType,k)
    if (MultiLogLoss((attr(knn_fit_CV,"prob")),pseudo_test$OutcomeType)<best_error){
      best_error<-MultiLogLoss((attr(knn_fit_CV,"prob")),pseudo_test$OutcomeType)
      best_k[i]<-k
    }
  }
}

# There doesn't seem to be a consistent value for the best k value of each fold
optimal_k<-mean(best_k) #let's choose the optimal k to be 165


# PART 3: MAKING PREDICTIONS ON THE TEST SET
predictions<-knn3Train(train=select(mini_train,-OutcomeType),test=clean_test,cl=mini_train$OutcomeType,k=120)

submission<-as.data.frame(attr(predictions,"prob"))

submission %>% 
  write.csv("Shelter_submission_v1.csv") 
