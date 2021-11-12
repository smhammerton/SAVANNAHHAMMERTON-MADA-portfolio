# Setup-------------------------------------------------------------
#load packages 
library(tidymodels) #for model fitting 
library(tidyverse) #for data processing 
library(rsample) #for data splitting 

#load data 
mydata <- readRDS('data/processed_data/processeddata.rds')


# Data Splitting------------------------------------------------------
#set seed to fix random numbers 
set.seed(124)

#put 3/4 of data into training set 
data_split <- initial_split(mydata, prop = 3/4)

#create data frames for training and testing sets 
train_data <- training(data_split)
test_data <- testing(data_split)

# Workflow Creation and Model Fitting--------------------------------
#create recipe fitting nausea to all predictors 
nausea <- recipes::recipe(Nausea ~ ., data = train_data)

#logistic regression model specification, computational engine glm
log_mod <- parsnip::logistic_reg() %>% 
  set_engine('glm')

#create simple workflow fitting logistic model to all predictors using glm function
nausea_workflow <- workflow() %>% 
  add_model(log_mod) %>% 
  add_recipe(nausea)

#view workflow
nausea_workflow

#prepare recipe and train model from resulting predictors 
nausea_fit <- nausea_workflow %>% 
  fit(data = train_data)

#pull fitted model object and get tibble of model coefficients 
nausea_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

# Model 1 Evaluation-------------------------------------------------
#use trained workflow to predict with unseen test data 
predict(nausea_fit, test_data)

#predict probabilities for each case in test and train data 
#test
nausea_aug_test <- augment(nausea_fit, test_data)
#view the data 
nausea_aug_test %>% 
  select(Nausea, .pred_class, .pred_Yes)

#train
nausea_aug_train <- augment(nausea_fit, train_data)
#view the data 
nausea_aug_train %>% 
  select(Nausea, .pred_class, .pred_Yes)

#create ROC curve; setting event level to 'second' because event levels will be alphabetized
#test
nausea_aug_test %>% roc_curve(truth = Nausea, .pred_Yes, event_level = "second") %>% 
  autoplot()
#calculate AUC 
nausea_aug_test %>% 
  roc_auc(truth = Nausea, .pred_Yes, event_level = 'second')

#train
nausea_aug_train %>% roc_curve(truth = Nausea, .pred_Yes, event_level = 'second') %>% 
  autoplot()
#AUC 
nausea_aug_train %>% 
  roc_auc(truth = Nausea, .pred_Yes, event_level = 'second')

# Alternative Model--------------------------------------------------

# Fitting only main predictor (runny nose) to nausea 

## Workflow Creation and Model Fitting-------------------------------

#create recipe fitting nausea to all predictors 
nausea_run <- recipes::recipe(Nausea ~ RunnyNose, data = train_data)


#create simple workflow fitting logistic model to all predictors using glm function
nausea_run_workflow <- workflow() %>% 
  add_model(log_mod) %>% 
  add_recipe(nausea_run)

#view workflow
nausea_run_workflow

#prepare recipe and train model from resulting predictors 
nausea_run_fit <- nausea_run_workflow %>% 
  fit(data = train_data)

#pull fitted model object and get tibble of model coefficients 
nausea_run_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

## Model 2 Evaluation-------------------------------------------------
#use trained workflow to predict with unseen test data 
predict(nausea_run_fit, test_data)

#predict probabilities for each case 
#test
nausea_run_aug_test <- augment(nausea_run_fit, test_data)
#view the data 
nausea_run_aug_test %>% 
  select(Nausea, .pred_class, .pred_Yes)

#train
nausea_run_aug_train <- augment(nausea_run_fit, train_data)
#view the data 
nausea_run_aug_train %>% 
  select(Nausea, .pred_class, .pred_Yes)

#create ROC curve
#test
nausea_run_aug_test %>% roc_curve(truth = Nausea, .pred_Yes, event_level = "second") %>% 
  autoplot()
#calculate AUC 
nausea_run_aug_test %>% 
  roc_auc(truth = Nausea, .pred_Yes, event_level = 'second')

#train
#ROC curve 
nausea_run_aug_train %>% roc_curve(truth = Nausea, .pred_Yes, event_level = 'second') %>% 
  autoplot()
#AUC 
nausea_run_aug_train %>% 
  roc_auc(truth= Nausea, .pred_Yes, event_level = 'second')

######################################
### Tzu-Chun's contribution starts ###
######################################

# Linear model with all predictors 
# Outcome: body temperature

# create recipe fitting body temperature to all predictors
bodytemp_rec<- 
  recipe(BodyTemp ~ ., data = train_data)

# fit a linear model 
lm_mod <- 
  linear_reg() %>% set_engine("lm")

# create model Workflow
bodytemp_workflow <-
  workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(bodytemp_rec)

bodytemp_workflow

# Preparing the recipe and train the model from the resulting predictors
bodytemp_fit<-
  bodytemp_workflow %>%
  fit(data = train_data)

# Extracting and tidying the model coefficients
bodytemp_fit %>%
  extract_fit_parsnip() %>% 
  tidy()

### Model 1 Evaluation
# we will use the RMSE as a metric to assess model performance

# Use a trained workflow to predict section using test data 
predict(bodytemp_fit, test_data)

# include predicted probabilities
bodytemp_aug <- augment(bodytemp_fit, test_data)

# estimate RMSE for test data
bodytemp_aug %>% 
  rmse(truth = BodyTemp, .pred)

# evaluate prediction in train data
predict(bodytemp_fit, train_data) 

# include predicted probabilities
bodytemp_train_aug <- 
  augment(bodytemp_fit, train_data) 

# estimate RMSE for train data
bodytemp_train_aug %>% 
  rmse(truth = BodyTemp, .pred) 

# The linear model with all predictors shows similar model fit for train (RMSE=1.20) and test (RMSE=1.17) data. 

# Alternative linear model with main predictor
# main predictor: runny nose
# Outcome: body temperature

# create recipe fitting body temperature to all predictors
bodytemp_rnose_rec<- 
  recipe(BodyTemp ~ RunnyNose, data = train_data)

# fit a linear model 
lm_mod <- 
  linear_reg() %>% set_engine("lm")

# create model Workflow
bodytemp_rnose_workflow <-
  workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(bodytemp_rnose_rec)

bodytemp_rnose_workflow

# Preparing the recipe and train the model from the resulting predictors
bodytemp_rnose_fit<-
  bodytemp_rnose_workflow %>%
  fit(data = train_data)

# Extracting and tidying the model coefficients
bodytemp_rnose_fit %>%
  extract_fit_parsnip() %>% 
  tidy()

### Alternative model Evaluation

# Use a trained workflow to predict section using test data 
predict(bodytemp_rnose_fit, test_data)

# include predicted probabilities
bodytemp_rnose_aug <- augment(bodytemp_rnose_fit, test_data)

# estimate RMSE for test data
bodytemp_rnose_aug %>% 
  rmse(truth = BodyTemp, .pred)

# evaluate prediction in train data
predict(bodytemp_rnose_fit, train_data) 

# include predicted probabilities
bodytemp_train_aug <- 
  augment(bodytemp_rnose_fit, train_data) 

# estimate RMSE for train data
bodytemp_train_aug %>% 
  rmse(truth = BodyTemp, .pred) 

# The model with only the runny nose had the same RMSE as the model with all predictors.
