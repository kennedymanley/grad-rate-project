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

# load elastic net fit object
load("results/elnet_fit.Rda")

# evaluate elastic net RMSE
elnet_predictions = predict(elnet_fit, 
                            alpha = elnet_fit$alpha,
                            newdata = grad_test, 
                            s = "lambda.1se") %>% as.numeric()

elnet_RMSE = sqrt(mean((elnet_predictions-grad_test$grad_rate)^2))

# load linear fit object
load("results/lm_fit.Rda")

linear_predictions = predict(lm_fit, newdata = grad_test)
linear_RMSE = sqrt(mean((linear_predictions - grad_test$grad_rate)^2))


# print nice table
tibble(Method = c("Linear", "Lasso", "Ridge"), `Test RMSE` = c(linear_RMSE, lasso_RMSE, ridge_RMSE)) %>%
  write_csv("results/model-evaluation.csv")

v = tibble(Method = c("Linear", "Lasso", "Ridge"), `Test RMSE` = c(linear_RMSE, lasso_RMSE, ridge_RMSE))

v %>% kable(format = "latex", row.names = NA, 
            booktabs = TRUE,
            digits = 5,
            col.names = c("Model type", 
                          "Root Mean Squared Error"),
            caption = "These are the test RMSE 
                        of the Linear Regression, Lasso Regression, and Ridge Regression.") %>%
  kable_styling(position = "center") %>%
  kable_styling(latex_options = "HOLD_position")




# load classification fit object
load("results/tree_fit.Rda")

# compute test misclassification error of the classification tree
dt_probabilities = predict(tree_fit1, newdata = grad_test, type = "class")
classification_tree_error = mean(dt_probabilities != grad_test$grad_rate)

# load deepest fit object
load("results/deepest_tree_fit.Rda")

# compute test misclassification error of the deepest tree
dt_probabilities1 = predict(deepest_tree_fit, newdata = grad_test, type = "class")
classification_tree_error1 = mean(dt_probabilities1 != grad_test$grad_rate)

# load random forest object
load("results/rf_fit.Rda")

# compute test misclassification error of the random forest
rf_predictions = predict(rf_fit_tuned,
                         type = "response", 
                         newdata = grad_test)
rf_error = mean(rf_predictions != grad_test$grad_rate)

# print nice table
tibble(Method = c("Classification", "Deepest", "Random Forest"), `Test Misclassification Error` = 
         c(classification_tree_error, classification_tree_error1, rf_error)) %>%
  write_csv("results/tree-model-evaluation.csv")

u = tibble(Method = c("Classification", "Deepest", "Random Forest"), `Test Misclassification Error` = 
         c(classification_tree_error, classification_tree_error1, rf_error))

 u %>% kable(format = "latex", row.names = NA, 
      booktabs = TRUE,
      digits = 5,
      col.names = c("Model type", 
                    "Misclassification error"),
      caption = "These are the test misclassification errors 
                        of the classification tree, deepest tree, and random forest 
                        classifiers.") %>%
  kable_styling(position = "center") %>%
  kable_styling(latex_options = "HOLD_position")

