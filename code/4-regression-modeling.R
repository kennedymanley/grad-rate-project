# load libraries
library(glmnetUtils)                    # to run ridge and lasso
source("code/functions/plot_glmnet.R")            # for lasso/ridge trace plots

# read in the training data
grad_train = read_csv("data/clean/grad_train.csv") 
grad_train %>% na.omit()

#ordinary least squares
lm_fit = lm(grad_rate ~ aggregation_index + nrc_code + county_code +membership_code +subgroup_code +local_rate 
            +reg_rate +reg_adv_rate +non_diploma_credential_rate + still_enr_rate +ged_rate, 
              data =grad_train)
summary(lm_fit)

# run ridge regression
set.seed(1)
ridge_fit = cv.glmnet(grad_rate ~ . - report_school_year - aggregation_index - aggregation_name
                      - county_code - county_name - membership_desc - subgroup_name,   
                      alpha = 0,                 
                      nfolds = 10,               
                      data = grad_train)

# save the ridge fit object
save(ridge_fit, file = "results/ridge_fit.Rda")

# create ridge CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/ridge-cv-plot.png")
plot(ridge_fit)
dev.off()

# create ridge trace plot
w = plot_glmnet(ridge_fit, grad_train, features_to_plot = 10)
ggsave(filename = "results/ridge-trace-plot.png", 
       plot = w, 
       device = "png", 
       width = 6, 
       height = 4)

lambda_ridge = ridge_fit$lambda.1se


# run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(grad_rate ~ . - report_school_year - aggregation_index - aggregation_name
                      - county_name - membership_desc - subgroup_name,   
                      alpha = 1,                 
                      nfolds = 10,               
                      data = grad_train)

# save the lasso fit object
save(lasso_fit, file = "results/lasso_fit.Rda")

# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# create lasso trace plot
p = plot_glmnet(lasso_fit, grad_train, features_to_plot = 6)
ggsave(filename = "results/lasso-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

lambda_lasso = lasso_fit$lambda.1se

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(lasso_fit, grad_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_csv("results/lasso-features-table.csv")

#Elastic Net Model
elnet_fit = cva.glmnet(grad_rate ~. - report_school_year - aggregation_index - aggregation_name
                       - county_name - membership_desc - subgroup_name,  # formula notation, as usual
                       nfolds = 10,               # number of folds
                       data = grad_train)   # data to run on

# save the elastic net model object
save(elnet_fit, file = "results/elnet_fit.Rda")

elnet_fit_best = extract_best_elnet(elnet_fit)
elnet_fit_best$alpha

# create elastic net CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/elnet-cv-plot.png")
plot(elnet_fit_best)
dev.off()

# create lasso trace plot
d = plot_glmnet(elnet_fit_best, grad_train, features_to_plot = 6)
ggsave(filename = "results/elnet-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

lambda_elnet = elnet_fit_best$lambda.1se

