# Submission doesn't work on Kaggle

library(dplyr)
library(caret)

train<-read.csv("train.csv")
test<-read.csv("test.csv")


# Testing out the model (I get an error of there being too many ties when I use the whole train dataset)
set.seed(17)
mini_train<-sample_frac(train,0.01)
mini_test<-sample_frac(test,1)
classifications<-mini_train$Category

train_input<-mini_train %>% select(X,Y)
test_input<-mini_test%>% select(X,Y)


num_neigh<-3

model_knn <-
  class::knn(train=train_input, test=test_input, cl=classifications,k = num_neigh, prob=TRUE)

#probs <-
#  knn3Train(train=train_input,test=test_input, cl=classifications,k = num_neigh, prob=TRUE)

mini_test <- mini_test %>%
  mutate(
    Category_hat = model_knn,
    Probability = attr(model_knn, "prob")
  )


#a<-knn.cv(train=train_input,cl=classifications,k=k_values,prob=TRUE)

# Cross Validation for Different Values of K

mini_train <-mini_train[sample(nrow(mini_train)),]

n_folds <- 5
mini_train_cv <- mini_train%>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

k_values<-seq(1,200,1)
best_k<-rep(0,n_folds)
for (i in 1:n_folds){
  min_error<-1
  pseudo_train<-mini_train_cv %>% filter(fold!=i)
  pseudo_test<-mini_train_cv %>% filter(fold==i)
  for (k in k_values){
    classifications_CV<-pseudo_train$Category
    pseudo_input_train<-pseudo_train %>% select(X,Y)
    pseudo_input_test<-pseudo_test %>% select(X,Y)
    model_knn_CV <-
      class::knn(train=pseudo_input_train, test=pseudo_input_test, cl=classifications_CV,k, prob=TRUE)
    pseudo_test <- pseudo_test %>%
      mutate(
        Category_hat = model_knn_CV,
        Probability = attr(model_knn_CV, "prob"),
        wrong=ifelse(Category_hat!=Category,1,0)
      )
    if (mean(pseudo_test$wrong)<min_error){
      min_error<-mean(pseudo_test$wrong)
      best_k[i]<-k
    }
  }
}

optimal_k<-mean(best_k)


# Make Predictions

set.seed(17)
mini_train<-sample_frac(train,0.01)
classifications<-mini_train$Category

train_input<-mini_train %>% select(X,Y)
test_input<-test%>% select(X,Y)

model_knn <-
  class::knn(train=train_input, test=test_input, cl=classifications,k = optimal_k, prob=TRUE)

test <- test %>%
  mutate(
    Category_hat = model_knn,
    Probability = attr(model_knn, "prob")
  )

submission<-test %>% select(Id,Category_hat) %>% 
  mutate(ARSON=ifelse(Category_hat=="ARSON",1,0),
         ASSAULT=ifelse(Category_hat=="ASSAULT",1,0),
         BAD_CHECKS=ifelse(Category_hat=="BAD CHECKS",1,0),
         BRIBERY=ifelse(Category_hat=="BRIBERY",1,0),
         BURGLARY=ifelse(Category_hat=="BURGLARY",1,0),
         DISORDERLY_CONDUCT=ifelse(Category_hat=="DISORDERLY CONDUCT",1,0),
         DRIVING_UNDER_THE_INFLUENCE=ifelse(Category_hat=="DRIVING UNDER THE INFLUENCE",1,0),
         DRUG_NARCOTIC=ifelse(Category_hat=="DRUG/NARCOTIC",1,0),
         DRUNKENNESS=ifelse(Category_hat=="DRUNKENNESS",1,0),
         EMBEZZLEMENT=ifelse(Category_hat=="EMBEZZLEMENT",1,0),
         EXTORTION=ifelse(Category_hat=="EXTORTION",1,0),
         FAMILY_OFFENSES=ifelse(Category_hat=="FAMILY OFFENSES",1,0),
         FORGERY_COUNTERFEITING=ifelse(Category_hat=="FORGERY/COUNTERFEITING",1,0),
         FRAUD=ifelse(Category_hat=="FRAUD",1,0),
         GAMBLING=ifelse(Category_hat=="GAMBLING",1,0),
         KIDNAPPING=ifelse(Category_hat=="KIDNAPPING",1,0),
         LARCENY_THEFT=ifelse(Category_hat=="LARCENY/THEFT",1,0),
         LIQUOR_LAWS=ifelse(Category_hat=="LIQUOR LAWS",1,0),
         LOITERING=ifelse(Category_hat=="LOITERING",1,0),
         MISSING_PERSON=ifelse(Category_hat=="MISSING PERSON",1,0),
         NON_CRIMINAL=ifelse(Category_hat=="NON-CRIMINAL",1,0),
         OTHER_OFFENSES=ifelse(Category_hat=="OTHER OFFENSES",1,0),
         PORNOGRAPHY_OBSCENE_MAT=ifelse(Category_hat=="PORNOGRAPHY/OBSCENE MAT",1,0),
         PROSTITUTION=ifelse(Category_hat=="PROSTITUTION",1,0),
         RECOVERED_VEHICLE=ifelse(Category_hat=="RECOVERED VEHICLE",1,0),
         ROBBERY=ifelse(Category_hat=="ROBBERY",1,0),
         RUNAWAY=ifelse(Category_hat=="RUNAWAY",1,0),
         SECONDARY_CODES=ifelse(Category_hat=="SECONDARY CODES",1,0),
         SEX_OFFENSES_FORCIBLE=ifelse(Category_hat=="SEX OFFENSES FORCIBLE",1,0),
         SEX_OFFENSES_NON_FORCIBLE=ifelse(Category_hat=="SEX OFFENSES NON FORCIBLE",1,0),
         STOLEN_PROPERTY=ifelse(Category_hat=="STOLEN PROPERTY",1,0),
         SUICIDE=ifelse(Category_hat=="SUICIDE",1,0),
         SUSPICIOUS_OCC=ifelse(Category_hat=="SUSPICIOUS_OCC",1,0),
         TREA=ifelse(Category_hat=="TREA",1,0),
         TRESPASS=ifelse(Category_hat=="TRESPASS",1,0),
         VANDALISM=ifelse(Category_hat=="VANDALISM",1,0),
         VEHICLE_THEFT=ifelse(Category_hat=="VEHICLE THEFT",1,0),
         WARRANTS=ifelse(Category_hat=="WARRANTS",1,0),
         WEAPON_LAWS=ifelse(Category_hat=="WEAPON LAWS",1,0))

submission %>% 
  select(-Category_hat) %>% 
  write.csv("SF_Crime_submission.csv") 
