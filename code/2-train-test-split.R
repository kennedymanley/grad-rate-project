# read in the cleaned data
grad_data = read_csv("data/clean/grad_data.csv")

set.seed(5) # seed set for reproducibility
n = nrow(grad_data)
train_samples = sample(1:n, round(0.8*n))

# split grad_data into training and test sets
grad_train = grad_data[train_samples,]
grad_test = grad_data[-train_samples,]

# save the train and test data
write_csv(x = grad_train, file = "data/clean/grad_train.csv")
write_csv(x = grad_test, file = "data/clean/grad_test.csv")
