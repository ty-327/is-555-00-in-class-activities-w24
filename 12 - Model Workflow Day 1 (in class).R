library(tidyverse)
library(tidymodels)

# Okay let's work through the (cleaned) titanic data from last week:
titanic <- read_csv('https://www.dropbox.com/s/92funarubgk5rzh/titanic_clean.csv?dl=1')


# First let's make sure factors are factors
leo <- titanic %>% 
  mutate(across(c(survived, had_cabin, sex), ~as.factor(.)))



# Now let's do a train/test split
set.seed(42)
leo_split <- leo %>% 
  initial_split(strata = survived) #strata is the label, helps us with the distribution or smthn...

# train/test
leo_training <- leo_split %>% training()
leo_testing <- leo_split %>% testing()

# Plan the model setup, including the engine and mode
leo_spec <- logistic_reg() %>% # ANNOUNCE TO THE WORLD WHAT MODEL WE ARE USING
  set_engine('glm') %>% #glm is the type of logistic regression algorithm
  set_mode('classification') #tell it to do regression or classification (seems redundant, but necessary sometimes)


# relevant model types: logistic_reg(), linear_reg(), decision_tree(), rand_forest(), boost_tree()
# show_engines('logistic_reg') #this shows you the different engines (or algorithms) available




# Now fit a model, look at output with tidy()
leo_fit <- leo_spec %>% 
  fit(survived ~ ., # tell it the independent variables I think? survived ~ .
      data = leo_training)

leo_fit %>% tidy()



# Calculate predictions, 
# including class predictions _and_ probabilities

leo_preds <- leo_fit %>% 
  predict(new_data = leo_testing)

leo_fit %>% 
  predict(new_data = leo_testing, type = 'prob') # returns probabilities I think

leo_test <- leo_testing %>% #I think this is where leo_test is initialized, I missed it in class
  bind_cols(leo_preds)


  

# Now let's build a confusion matrix and explore a few of the related metrics.
# conf_mat(), sens()
leo_test %>% 
  sens(truth= survived,
       estimate = .pred_class)

leo_test %>% 
  accuracy(truth = survived,
           estimate = .pred_class)

leo_test %>% 
  conf_mat(truth = survived,
           estimate = .pred_class) %>% 
  summary()





# Let's get fancy:
# roc_curve(), roc_auc(), autoplot()
roc_data <- leo_results %>% 
  roc_curve(truth = survived, .pred_0)

roc_data %>% 
  autoplot()

leo_results %>% 
  roc_auc(truth = survived, .pred_0)

# Finalize the model with last_fit()
leo_final <- leo_spec %>% 
  last_fit (
    survived ~ .,
    split = leo_split
  )
# THIS FINALIZED OBJECT ALLOWS YOU TO USE THE SAME WORKFLOW, SAME SPLITS, ETC.!!!!
# COULD USE THIS WITH LAWSON PROJECT
# EASIER TO WORK WITH APIS & STUFF


# finalized object, extract predictions, metrics 
# with dedicated collect_* functions:





