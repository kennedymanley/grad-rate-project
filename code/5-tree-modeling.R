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

set.seed(1)
randomRows = sample(1:nrow(grad_train), 1000, replace=F)
sliced_data = grad_train %>% slice(randomRows) %>% na.omit(grad_train) %>%
  select(-report_school_year, -aggregation_index, -aggregation_name,
         -county_code, -county_name, -membership_code, -membership_key,
         -membership_desc, -subgroup_code, -subgroup_name)

# fit classification tree
tree_fit1 = rpart(grad_rate ~ . ,
                 method = "class", # classification
                 parms = list(split = "gini"), # Gini index for splitting
                 data = sliced_data)


save(tree_fit1, file = "results/tree_fit.Rda")
rpart.plot(tree_fit1, box.palette=0) # plot tree

#fit deepest possible tree
set.seed(1) # for reproducibility (DO NOT CHANGE)
deepest_tree_fit = rpart(grad_rate ~ .,
                         method = "class",
                         parms = list(split = "gini"),
                         control = rpart.control(minsplit = 1,
                                                 minbucket = 1,
                                                 cp = 0),
                         data = sliced_data)

# print the CP table for this tree
cp_table = deepest_tree_fit$cptable %>% as_tibble()
save(cp_table, file = "results/cp_table.Rda")

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


optimal_tree = prune(tree = tree_fit1, cp = optimal_tree_info$CP)
save(optimal_tree, file = "results/optimal_tree.Rda")


#random forest
set.seed(1) # for reproducibility (DO NOT CHANGE)
rf_fit = randomForest(factor(grad_rate) ~ ., data = sliced_data)
save(rf_fit, file = "results/rf_fit.Rda")

num_features = ncol(sliced_data) - 1
mtry = floor(sqrt(num_features))
mtry_verified = rf_fit$mtry

o = tibble(oob_error = rf_fit$err.rate[,"OOB"], trees = 1:500) %>%
  ggplot(aes(x = trees, y = oob_error)) + geom_line() + 
  labs(x = "Number of trees", y = "OOB error") + theme_bw()
ggsave(filename = "results/oob-error-plot.png", 
       plot = o, 
       device = "png", 
       width = 6, 
       height = 4)
