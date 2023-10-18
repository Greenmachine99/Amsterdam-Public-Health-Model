### Importing Libraries
library(rio)
library(ggplot2)
library(broom)
library(car)
library(dplyr)
library(nnet)
library(tree)
library(randomForest)
library(e1071)
library(tidyr)

### Importing & Mutating Datasets
  
  ## Importing Datasets
health = import('Datasets/amsterdam_health.csv', stringsAsFactors = TRUE)
income = import('Datasets/amsterdam_income.csv', stringsAsFactors = TRUE)
population = import('Datasets/amsterdam_population.csv', stringsAsFactors = TRUE)
public_space = import('Datasets/amsterdam_publicspace.csv', stringsAsFactors = TRUE)
social = import('Datasets/amsterdam_social.csv', stringsAsFactors = TRUE)
housing = import('Datasets/amsterdam_housing.csv', stringsAsFactors = TRUE)

  ## Joining Datasets
all_data = inner_join(health, income)
all_data = inner_join(all_data, population)
all_data = inner_join(all_data, public_space)
all_data = inner_join(all_data, social)
all_data = inner_join(all_data, housing)

  ## Mutating Datasets
all_data = all_data %>%
  mutate(avg_greenery = pub_area_green / (pub_area_land + pub_area_water))
all_data = all_data %>%
  mutate(tot_facilities = soc_fac_cultural + soc_fac_sports)
all_data = all_data %>%
  mutate(pop_elder = (pop_60_69 + pop_70_79 + pop_80_89 + pop_90) / pop_total)

all_data$district = factor(all_data$district)

  ## Oversampling Dataset
table(all_data$district)

n_max = all_data %>%
  count(district) %>%
  pull(n) %>% max()
all_data_os <- all_data %>%
  group_by(district) %>%
  sample_n(size = n_max, replace = TRUE)

all_data_os$district = factor(all_data_os$district)

### Functions for Classifier Evaluation
  
  # Accuracy
accuracy <- function(y, yhat) {
  sum(yhat == y) / length(y)
}

  # Precision
precision <- function(y, yhat, class = NULL) {
  if(missing(class))
    p <- sapply(levels(y), precision, y = y, yhat = yhat)
  else
    p <- sum(yhat == class & yhat == y) / sum(yhat == class)
  
  ifelse(is.finite(p), p, 0)
}

  # Recall
recall <- function(y, yhat, class = NULL) {
  if(missing(class))
    r <- sapply(levels(y), recall, y = y, yhat = yhat)
  else
    r <- sum(yhat == class & yhat == y) / sum(y == class)
  
  ifelse(is.finite(r), r, 0)
}
  
  # F
f <- function(y, yhat, class = NULL) {
  if(missing(class))
    ff <- sapply(levels(y), f, y = y, yhat = yhat)
  else {
    p <- precision(y, yhat, class)
    r <- recall(y, yhat, class)
    ff <- 2*p*r/(p+r)
  }
  
  ifelse(is.finite(ff), ff, 0)
}

  # Confusion Matrix
confusion_matrix <- function(y, yhat) {
  table(truth = y, predicted = yhat) %>%
    as.data.frame() %>%
    ggplot(aes(predicted, truth)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "lightblue")
}

### Functions for Cross Validation
kfold_cv <- function(data, response, k, H, FUN, ...) {
  
  # randomly assign instances to folds in a column called .fold, stratified by class
  data <- data %>%
    group_by({{response}}) %>%
    mutate(.fold = sample(rep(1:k, length.out = n()))) %>%
    ungroup()
  
  # for each value h in H to explore, do CV
  all_folds <- lapply(H, function(h) {
    # for each fold kk = 1...k
    per_fold <- lapply(1:k, function(kk) {
      # partition the data in training and validation
      training <- data %>% filter(.fold != kk) # everything except fold kk
      validation <- data %>% filter(.fold == kk) # only fold kk
      # call the FUNction to train the model and compute performance
      l <- FUN(training, validation, h, ...)
      if(!is.list(l)) stop("The cross-validation function must return a list of metric scores")
      
      l %>%  
        as.data.frame() %>% 
        mutate(.h = h, .fold = kk, .before = 1)
    }) %>% bind_rows()
  }) %>% bind_rows()
  
  all_folds
}

### Multinomial Regression Classifier

  # Make Model
mrc = multinom(district ~ (hea_good + hea_psych + hea_overweight + hea_lonely)^2 + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder, all_data, trace = FALSE)
summary(mrc)
  # Create Classifier
p_mrc = predict(mrc)
  # Assess Classifier
accuracy(all_data$district, p_mrc)
precision(all_data$district, p_mrc)
recall(all_data$district, p_mrc)
f(all_data$district, p_mrc)

confusion_matrix(all_data$district, p_mrc)

### Tree Classifier

  ## Normal Tree
  # Make Model
m_fulltree = tree(district ~ hea_good + hea_psych + hea_overweight + hea_lonely + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder, all_data)
summary(m_fulltree)

  # Plot Model
plot(m_fulltree)
text(m_fulltree, ex = .6)
  # Make Classifier
p_fulltree = predict(m_fulltree, type = 'class')
  # Assess Classifier
accuracy(all_data$district, p_fulltree)
precision(all_data$district, p_fulltree)
recall(all_data$district, p_fulltree)
f(all_data$district, p_fulltree)

confusion_matrix(all_data$district, p_fulltree)

  ## Bagged Tree
  # Make Model
m_bt = randomForest(district ~ hea_good + hea_psych + hea_overweight + hea_lonely + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder, all_data_os, mtry = 10, ntree = 150)
summary(m_bt)
  # Make Classifier
p_bt = predict(m_bt, type = 'class')
  # Assess Classifier
accuracy(all_data$district, p_bt)
precision(all_data$district, p_bt)
recall(all_data$district, p_bt)
f(all_data$district, p_bt)

confusion_matrix(all_data$district, p_bt)

  ## Random Forrest
  # Make Model
m_rf = randomForest(district ~ hea_good + hea_psych + hea_overweight + hea_lonely + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder, all_data, mtry = 4, ntree = 500)
summary(m_rf)
  # Make Classifier
p_rf = predict(m_rf, type = 'class')
  # Asses Classifier
accuracy(all_data$district, p_rf)
precision(all_data$district, p_rf)
recall(all_data$district, p_rf)
f(all_data$district, p_rf)

confusion_matrix(all_data$district, p_rf)

### Support Vector Machine Classifier

  ## Polynomial Kernel
  # Make Model
m_svm_p = svm(district ~ hea_good + hea_psych + hea_overweight + hea_lonely + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder, all_data, kernel = "poly", degree = 2)
summary(m_svm_p)
  # Make Classifier
p_svm_p = predict(m_svm_p)
  # Assess Model
accuracy(all_data$district, p_svm_p)
precision(all_data$district, p_svm_p)
recall(all_data$district, p_svm_p)
f(all_data$district, p_svm_p)

confusion_matrix(all_data$district, p_svm_p)

  ## Radial Kernel
  # Make Model
m_svm_r = svm(district ~ hea_good + hea_psych + hea_overweight + hea_lonely + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder, all_data)
summary(m_svm_r)
  # Make Classifier
p_svm_r = predict(m_svm_r)
  # Assess Classifier
accuracy(all_data$district, p_svm_r)
precision(all_data$district, p_svm_r)
recall(all_data$district, p_svm_r)
f(all_data$district, p_svm_r)

confusion_matrix(all_data$district, p_svm_r)

### Cross Validation

  ## Tree Pruning
  # Use Pruning Function
fit_with_prune <- function(training, validation, h, ...) {
  # fit the full model with the training set
  m <- tree(district ~ hea_good + hea_psych + hea_overweight + hea_lonely + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder, data = training)
  # prune to h
  m <- prune.tree(m, best = h)
  # predict on the validation set
  p <- predict(m, newdata = validation, type = "class")
  
  # Compute evaluation metrics
  # Note how for precision, recall and F we compute the mean score across classes
  list(a = accuracy(validation$district, p),
       p = mean(precision(validation$district, p)),
       r = mean(recall(validation$district, p)),
       f = mean(f(validation$district, p)))
}
  # Create Validator
set.seed(123)
pr = kfold_cv(all_data, district, 10, 5:25, fit_with_prune)
  # Assess Validator
pr
  # Plot Validator
pivot_longer(pr, cols = -(.h:.fold), names_to = "metric") %>%
  ggplot(aes(.h, value, color = metric, fill = metric)) + 
  stat_summary(fun = "mean", geom = "point") +
  geom_smooth(se = TRUE, alpha = .15) +
  labs(x = "leaf nodes", y = "value")

  ## Random Forest

  # Number of Trees
  # Use Validator Function
fit_with_ntree <- function(training, validation, h, ...) {
  # fit the forest with h trees
  m <- randomForest(district ~ hea_good + hea_psych + hea_overweight + hea_lonely + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder,
                    data = training, ntree = h)
  # predict on validation set
  p <- predict(m, newdata = validation)
  
  # Compute evaluation metrics
  # Note how for precision, recall and F we compute the mean score across classes
  list(a = accuracy(validation$district, p),
       p = mean(precision(validation$district, p)),
       r = mean(recall(validation$district, p)),
       f = mean(f(validation$district, p)))
}
  # Create Validator
set.seed(321)

p_rf_cv <- kfold_cv(all_data, district, 10, seq(10, 300, 20), fit_with_ntree)
  # Plot Validator
pivot_longer(p_rf_cv, cols = -(.h:.fold), names_to = "metric") %>%
  ggplot(aes(.h, value, color = metric, fill = metric)) + 
  stat_summary(fun = "mean", geom = "point") +
  geom_smooth(se = TRUE, alpha = .15) +
  labs(x = "number of trees", y = "value")

  # MTRY
  # Use Validator Function
fit_with_mtry = function(training, validation, h, ...) {
  m = randomForest(district ~ hea_good + hea_psych + hea_overweight + hea_lonely + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder,
                   data = training, mtry = h, ntree = 150)
  # prediction on validation set
  p = predict(m, validation)
  
  # Compute Evaluation Metrics
  list(a = accuracy(validation$district, p),
       p = mean(precision(validation$district, p)),
       r = mean(recall(validation$district, p)),
       f = mean(f(validation$district, p)))
}
  # Create Validator
set.seed(0)

p_rf_cv_mtry = kfold_cv(all_data, district, 10, c(1, 2, 3, 4, 5, 6, 7, 8, 9), fit_with_mtry)
  # Plot Validator
pivot_longer(p_rf_cv_mtry, cols = -(.h:.fold), names_to = "metric") %>%
  ggplot(aes(.h, value, color = metric, fill = metric)) + 
  stat_summary(fun = "mean", geom = "point") +
  geom_smooth(se = TRUE, alpha = .15) +
  labs(x = "mtry", y = "value") 

  ## Support Vector Machine

  # Costs
  # Use Validator Function
fit_with_cost <- function(training, validation, h, ...) {
  m <- svm(district ~ hea_good + hea_psych + hea_overweight + hea_lonely + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder,
           data = training, cost = h)
  # predict on validation set
  p <- predict(m, validation)
  
  # Compute evaluation metrics
  # Note how for precision, recall and F we compute the mean score across classes
  list(a = accuracy(validation$district, p),
       p = mean(precision(validation$district, p)),
       r = mean(recall(validation$district, p)),
       f = mean(f(validation$district, p)))
}
  # Create Validator
set.seed(0)

p_svm_cv <- kfold_cv(all_data, district, 10, c(.1, .25, .5, .75, 1, 2.5, 5, 7.5, 10, 15, 25, 50, 100), fit_with_cost)
  # Plot Validator
pivot_longer(p_svm_cv, cols = -(.h:.fold), names_to = "metric") %>%
  ggplot(aes(.h, value, color = metric, fill = metric)) + 
  stat_summary(fun = "mean", geom = "point") +
  geom_smooth(se = TRUE, alpha = .15) +
  labs(x = "cost", y = "value") +
  scale_x_log10()

  # Gamma
  # Use Validator Function
fit_with_gamma <- function(training, validation, h, ...) {
  m <- svm(district ~ hea_good + hea_psych + hea_overweight + hea_lonely + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder,
           data = training, gamma = h)
  # predict on validation set
  p <- predict(m, validation)
  
  # Compute evaluation metrics
  # Note how for precision, recall and F we compute the mean score across classes
  list(a = accuracy(validation$district, p),
       p = mean(precision(validation$district, p)),
       r = mean(recall(validation$district, p)),
       f = mean(f(validation$district, p)))
}
# Create Validator
set.seed(12345)

p_svm_cv_g <- kfold_cv(all_data, district, 10, seq(.01, 2, .1), fit_with_gamma)

  # Plot Validator
pivot_longer(p_svm_cv_g, cols = -(.h:.fold), names_to = "metric") %>%
  ggplot(aes(.h, value, color = metric, fill = metric)) + 
  stat_summary(fun = "mean", geom = "point") +
  geom_smooth(se = TRUE, alpha = .15) +
  labs(x = "gamma", y = "value") +
  scale_x_log10()

### Random Forest (Cross Validated)

  ## Random Forest
  # Make Model
m_rf_cv = randomForest(district ~ hea_good + hea_psych + hea_overweight + hea_lonely + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder, all_data, mtry = 3, ntree = 150)
summary(m_rf_cv)
  # Make Classifier
p_rf_cv = predict(m_rf_cv, type = 'class')
  # Asses Classifier
accuracy(all_data$district, p_rf_cv)
precision(all_data$district, p_rf_cv)
recall(all_data$district, p_rf_cv)
f(all_data$district, p_rf_cv)

confusion_matrix(all_data$district, p_rf_cv)

### Support Vector Machine (Cross Validated)

  ## Radial Kernel
  # Make Model
m_svm_r = svm(district ~ hea_good + hea_psych + hea_overweight + hea_lonely + inc_disposable + hou_value + tot_facilities + pub_setup + avg_greenery + pop_elder, all_data, cost = 10, gamma = 0.95)
summary(m_svm_r)
  # Make Classifier
p_svm_r = predict(m_svm_r)
  # Assess Classifier
accuracy(all_data$district, p_svm_r)
precision(all_data$district, p_svm_r)
recall(all_data$district, p_svm_r)
f(all_data$district, p_svm_r)

confusion_matrix(all_data$district, p_svm_r)
