library(tidyverse)
library(tidymodels)
library(vetiver)
library(pins)

#install.packages("ranger")

# Data and Model Setup
rentals <-  read_csv('https://www.dropbox.com/scl/fi/khs0biz27katxeoduhy8c/housing_with_dates.csv?rlkey=2xvu3sr5eqbtxteh28cz1df0r&dl=1')

set.seed(42)
rentals_split <- initial_split(rentals, strata = sale_price)

rentals_training <- rentals_split %>% training()
rentals_testing <- rentals_split %>% testing()

rentals_rec <- recipe(sale_price ~ .,
                      data = rentals_training) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_other(ms_sub_class,ms_zoning,exterior_1st,exterior_2nd,utilities,electrical) %>% 
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors())

rentals_rf_model <- rand_forest() %>% 
  set_engine('ranger') %>% 
  set_mode('regression')

rentals_wkfl <- workflow() %>% 
  add_model(rentals_rf_model) %>% 
  add_recipe(rentals_rec)

# finalize the model
final_model <- rentals_wkfl %>% 
  last_fit(split = rentals_split)

# Prep for deployment:
trained_workflow <- final_model %>% extract_workflow()
deployable_model <- vetiver_model(trained_workflow, 'rental_pricing_model')

# Write the (deployable) vetiver model object to a pin board:
pin_board <- board_folder('C:/Users/tyisb/OneDrive/Documents/School/SEMESTERS/SEM 8/IS 555/pin board stuff')

vetiver_pin_write(pin_board, deployable_model)


# Now let's set ourselves up to do some performance monitoring. First we need 
# some metrics that represent "expected" performance based on our test set used
# during training.

trained_workflow %>% 
  predict(new_data = rentals_testing)

final_model %>% collect_metrics() 


# Let's look at performance over time during the training window. This requires
# generating predictions from the training window. Which data should we use?
performance_metrics_from_training_window %>% 
  ggplot(aes(x = .index, y = .estimate, color = .metric)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~.metric, scales = 'free_y', ncol = 1)


# The metrics are intentionally arranged for easy plotting. Create a line chart
# that summarizes performance over time.
performance_metrics_from_training_window <- final_model %>% collect_predictions() %>% 
  bind_cols(rentals_testing %>% select(date_listed)) %>% 
  vetiver_compute_metrics(date_var = date_listed, 
                          period = 'month',   #period is like how you're going to group by
                          truth = sale_price,
                          estimate = .pred) 
#ORRRRR......


performance_metrics_from_training_window %>% 
  vetiver_plot_metrics()


# Metrics are designed to be pinned alongside models for later referencing:
pin_board %>% pin_write(performance_metrics_from_training_window, 'rental_metrics')


# Now let's roll the clock forward...
# 
# Tick...
# 
#   Tock...
# 
# Tick...
# 
#   Tock...
# 
# Tick...
# 
#   Tock...
# 
# Tick...
# 
#   Tock...

# Read in new data:
new_rental_listings <- read_csv('https://www.dropbox.com/scl/fi/sgehr76gch8tux9pzdgfz/new_housing_with_dates.csv?rlkey=do0s9jbrgb2y53e728zrxl6zo&dl=1')

new_rental_listings %>% select(date_listed) %>% summary()

# Read in model from the pin board:
pin_board
old_model_object <- vetiver_pin_read(pin_board, 'rental_pricing_model')

# Calculate performance metrics on new data
new_metrics <- old_model_object %>% 
  predict(new_data = new_rental_listings) %>% 
  bind_cols(new_rental_listings %>% select(sale_price, date_listed)) %>% 
  vetiver_compute_metrics(date_var = date_listed, 
                          period = 'month',   #period is like how you're going to group by
                          truth = sale_price,
                          estimate = .pred) 

new_metrics %>% 
  vetiver_plot_metrics()

# Update metrics (on the pin_board) so that we can use them next time:
pin_board %>% vetiver_pin_metrics(df_metrics = new_metrics,
                                  metrics_pin_name = 'rental_metrics')


# Reusable plot from the pin_board "stash":
pin_board %>% 
  pin_read('rental_metrics') %>% 
  vetiver_plot_metrics()



