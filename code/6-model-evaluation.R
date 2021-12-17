# load libraries
library(glmnetUtils)
library(tidyverse)

# load test data
grad_test = read_csv("data/clean/grad_test.csv")

# load ridge fit object
load("results/ridge_fit.Rda")

# load lasso fit object
load("results/lasso_fit.Rda")

# evaluate ridge RMSE
ridge_predictions = predict(ridge_fit, 
                            newdata = grad_test, 
                            s = "lambda.1se") %>%
  as.numeric()
ridge_RMSE = sqrt(mean((ridge_predictions-grad_test$grad_rate)^2))

# evaluate lasso RMSE
lasso_predictions = predict(lasso_fit, 
                            newdata = grad_test, 
                            s = "lambda.1se") %>%
  as.numeric()
lasso_RMSE = sqrt(mean((lasso_predictions-grad_test$grad_rate)^2))

# print nice table
tibble(Method = c("Ridge", "Lasso"), `Test RMSE` = c(ridge_RMSE, lasso_RMSE)) %>%
  write_csv("results/model-evaluation.csv")
