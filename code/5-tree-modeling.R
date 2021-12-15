# load libraries
library(rpart)         # to train decision trees
library(rpart.plot)    # to plot decision trees
library(randomForest)  # random forests
library(gbm)           # boosting
library(tidyverse)     # tidyverse
library(kableExtra)    # for printing tables
library(cowplot)       # for side by side plots


# read in the training data
grad_train = read_csv("data/clean/grad_train.csv")
grad_train = grad_train %>% na.omit(grad_train)
grad_train_clean = grad_train %>% select(-report_school_year, -aggregation_index, -aggregation_name,
                                         -county_code, -county_name, -membership_code, -membership_key,
                                         -membership_desc, -subgroup_code, -subgroup_name)

# fit classification tree
tree_fit = rpart(grad_rate ~ . ,
                 method = "class", # classification
                 parms = list(split = "gini"), # Gini index for splitting
                 data = grad_train_clean)

rpart.plot(tree_fit) # plot tree

#fit deepest possible tree
set.seed(1) # for reproducibility (DO NOT CHANGE)

deepest_tree_fit = rpart(spam ~ .,
                         method = "class",
                         parms = list(split = "gini"),
                         control = rpart.control(minsplit = 1,
                                                 minbucket = 1,
                                                 cp = 0),
                         data = spam_train)

# print the CP table for this tree
cp_table = deepest_tree_fit$cptable %>% as_tibble()
cp_table %>% kable(format = "latex", row.names = NA, 
                   booktabs = TRUE, digits = 5,
                   col.names = c("CP", 
                                 "Number of splits",
                                 "Relative error",
                                 "CV error",
                                 "CV std"),
                   caption = "This is the CP table for this 
                   deepest tree.") %>%
  kable_styling(position = "center") %>%
  kable_styling(latex_options = "HOLD_position")

# produce CV plot based on info in CP table
cp_table %>%
  filter(nsplit >= 2) %>%
  ggplot(aes(x = nsplit+1, y = xerror,
             ymin = xerror - xstd, ymax = xerror + xstd)) + 
  scale_x_log10() +
  geom_point() + geom_line() +
  geom_errorbar(width = 0.25) +
  xlab("Number of terminal nodes on log scale") + ylab("CV error") + 
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") + 
  theme_bw()

optimal_tree_info = cp_table %>% 
  filter(xerror - xstd < min(xerror)) %>% 
  arrange(nsplit) %>% head(1)
optimal_tree_info$nsplit


optimal_tree = prune(tree = tree_fit, cp = optimal_tree_info$CP)


#random forest
set.seed(1) # for reproducibility (DO NOT CHANGE)

rf_fit = randomForest(factor(spam) ~ ., data = spam_train)


#boosted models
set.seed(1) # for reproducibility (DO NOT CHANGE)
# fit random forest with interaction depth 1
gbm_fit_1 = gbm(spam ~ .,
                distribution = "bernoulli", 
                n.trees = 1000, 
                interaction.depth = 1, 
                shrinkage = 0.1,
                cv.folds = 5,
                data = spam_train)

set.seed(1) # for reproducibility (DO NOT CHANGE)
# fit random forest with interaction depth 2
gbm_fit_2 = gbm(spam ~ .,
                distribution = "bernoulli", 
                n.trees = 1000, 
                interaction.depth = 2, 
                shrinkage = 0.1,
                cv.folds = 5,
                data = spam_train)

set.seed(1) # for reproducibility (DO NOT CHANGE)
# fit random forest with interaction depth 3
gbm_fit_3 = gbm(spam ~ .,
                distribution = "bernoulli", 
                n.trees = 1000, 
                interaction.depth = 3, 
                shrinkage = 0.1,
                cv.folds = 5,
                data = spam_train)