###############################
# analysis script
#

#load needed packages
library(tidyverse) #for plotting 
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidymodels) #for model fitting 
library(dotwhisker) #for visualizing regression results

#path to data
data_location <- here::here("data","processed_data","processeddata.rds")

#load data and set to object 
mydata <- readRDS(data_location)

#linear regression model specification, computational engine lm
lm_mod <- parsnip::linear_reg() %>% 
  set_engine('lm')
  
#fit linear model to temp using runny nose 
temp_run_lm <- lm_mod %>% 
  fit(BodyTemp ~ RunnyNose, data = mydata)

tidy(temp_run_lm)

#fit linear model to temp using all predictors 
temp_all_lm <- lm_mod %>% 
  fit(BodyTemp ~ ., data= mydata)

View(broom::tidy(temp_all_lm))

#visualize regression results 
#create plot of regression coefficients
p1 <- tidy(temp_all_lm) %>% 
  dwplot(dot_args = list(size = 2, color = 'black'),
         whisker_args = list(color = 'black'),
         vline = geom_vline(xintercept = 0, color = 'grey50', linetype = 2))
#view plot
print(p1)

#save figure
figure_file = here("results","resultfigure.png")
ggsave(filename = figure_file, plot=p1) 

#compare model with just runny nose and model with all predictors 
glimpse(glance(temp_run_lm)) #AIC of 2329.346, r squared of 0.0123 when just using runny nose
glimpse(glance(temp_all_lm)) #AIC of 2303.84, r squared of 0.1287 when using all predictors
#the model with all predictors is a better fit than the one with just runny nose - 
#the AIC is lower and the r squared is higher in the model with all predictors


#logistic regression model specification, computational engine glm
log_mod <- parsnip::logistic_reg() %>% 
  set_engine('glm')

#fit logistic model to nausea using only runny nose 
nausea_run_log <- log_mod %>% 
  fit(Nausea ~ RunnyNose, data = mydata)

#fit logistic model to nausea using all predictors 
nausea_all_log <- log_mod %>% 
  fit(Nausea ~ ., data = mydata)

View(tidy(nausea_all_log))

#visualize regression results 
#create plot of regression coefficients 
p2 <- tidy(nausea_all_log) %>% 
  dwplot(dot_args = list(size = 2, color = 'black'),
         whisker_args = list(color = 'black'),
         vline = geom_vline(xintercept = 0, color = 'grey50', linetype = 2))
#view plot
print(p2)

#save figure 
figure_file2 = here("results","resultfigure2.png")
ggsave(filename = figure_file2, plot=p2) 

#compare model with just runny nose and model with all predictors 
glimpse(glance(nausea_run_log)) #AIC of 948.566 when just using runny nose
glimpse(glance(nausea_all_log)) #AIC of 821.471 when using all predictors 
#the models with all predictors is a better fit - the AIC is much lower 


  