# load libraries
library(tidyverse)
library(readr)

# download raw data into a tibble
# must fully download the data from https://data.nysed.gov/files/gradrate/19-20/gradrate.zip before opening in excel
# this is because the data is a zip file and the csv file must be extracted from the zip file.
grad_data_raw <- read_csv("grad-rate-project/data/raw/grad_data_raw.csv")

