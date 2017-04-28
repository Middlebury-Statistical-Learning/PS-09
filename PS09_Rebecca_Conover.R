library(tidyverse)
library(broom)
library(readr)
library(jsonlite)
library(caret)

train <-
  bind_rows(
    fromJSON(
      "~/Documents/Year 2/Sem 2.2/Statistical Learning/Problem Sets/train.json"
    )
  )


num_neigh <- seq(1, 200, 5)
n_folds <- 5


train <- train %>%
  select(bathrooms, bedrooms, latitude, longitude, price, interest_level) %>%
  mutate(
    bathrooms = unlist(bathrooms),
    bedrooms = unlist(bedrooms),
    price = unlist(price),
    interest_level = unlist(interest_level)
  ) %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length = n())) %>%
  arrange(fold) %>%
  sample_frac(.25)

k_scores <- c()

for (i in num_neigh) {
  fold_score <- c()
  
  for (j in (1:n_folds)) {
    pseudo_train <- train %>%
      filter(fold != j)
    pseudo_test <- train %>%
      filter(fold == j)
    
    classifications <- pseudo_train$interest_level
    actual_outcome <- pseudo_test$interest_level
    
    train_input <- pseudo_train %>%
      select(bathrooms, bedrooms, price)
    
    test_input <- pseudo_test %>%
      select(bathrooms, bedrooms, price)
    
    model_knn <-
      class::knn(
        train = train_input,
        test = test_input,
        cl = classifications,
        k = i,
        prob = TRUE
      )
    
    
    pseudo_test <- pseudo_test %>%
      mutate(prediction = model_knn,
             actual = actual_outcome)
    
    
    j_score <- pseudo_test %>%
      mutate(correct = ifelse(prediction == actual, 1, 0)) %>%
      summarize(prop_correct = sum(correct) / n())
    
    fold_score <- fold_score %>%
      rbind(j_score$prop_correct) %>%
      as.data.frame() %>%
      mutate(fold = j)
    
  }
  
  overall <- fold_score %>%
    summarize(mean_score = mean(V1)) %>%
    as.data.frame() %>%
    mutate(k = i)
  
  k_scores <- k_scores %>%
    rbind(overall)
  
  
  if (i %% 1 == 0) {
    print(i)
    
  }
  
  
}

ggplot(k_scores, aes(x = k, y = mean_score)) + geom_point()

k <- 95
test <-
  bind_rows(fromJSON(
    "~/Documents/Year 2/Sem 2.2/Statistical Learning/Problem Sets/test.json"
  ))

train <-
  bind_rows(
    fromJSON(
      "~/Documents/Year 2/Sem 2.2/Statistical Learning/Problem Sets/train.json"
    )
  ) %>%
  select(bathrooms, bedrooms, price, interest_level) %>%
  mutate(
    bathrooms = unlist(bathrooms),
    bedrooms = unlist(bedrooms),
    price = unlist(price),
    interest_level = unlist(interest_level)
  )


test <- test %>%
  select(bathrooms, bedrooms, price, listing_id) %>%
  mutate(
    bathrooms = unlist(bathrooms),
    bedrooms = unlist(bedrooms),
    price = unlist(price),
    listing_id = unlist(listing_id)
  )

classifications <- train$interest_level


train_input <- train %>%
  select(bathrooms, bedrooms, price)

test_input <- test %>%
  select(bathrooms, bedrooms, price)

model_knn <-
  knn3Train(train_input,
            test_input,
            cl = classifications,
            k = k,
            prob = TRUE)

test <- test %>%
  cbind(attr(model_knn, "prob"))

test <- test %>%
  select(listing_id, high, medium, low) %>%
  write_csv("PS09_predictions.csv")






