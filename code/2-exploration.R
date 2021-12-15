# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(maps)                           # for creating maps
library(tidyverse)

# read in the cleaned data
grad_data = read_csv("data/clean/grad_data.csv")



# calculate median graduation rate
median_grad_rate = grad_data %>%
  summarise(median(as.numeric(grad_rate))) %>%
  pull()



# create histogram of graduation rate
p = grad_data %>%
  ggplot(aes(x = grad_rate)) + 
  geom_histogram(bins = 15) +
  geom_vline(xintercept = median_grad_rate,
             linetype = "dashed",
             color = "red") +
  labs(x = "Graduation Rate (percent)",
       y = "Frequency") +
  theme_bw()

# save the histogram
ggsave(filename = "results/response-histogram.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)

# examine counties with schools that have 100% graduation rate
grad_data %>% 
  select(county_code, county_name, grad_rate) %>%
  distinct() %>%
  arrange(desc(grad_rate)) %>%
  head(62) %>%
  write_csv("results/top-counties-data.csv")

# calculate median dropout rate
median_dropout_rate = grad_data %>%
  summarise(median(as.numeric(dropout_rate))) %>%
  pull()


# create histogram of dropout rate
z = grad_data %>%
  ggplot(aes(x = dropout_rate)) + 
  geom_histogram(bins = 15) +
  geom_vline(xintercept = median_dropout_rate,
             linetype = "dashed",
             color = "red") +
  labs(x = "Dropout Rate (percent)",
       y= "Frequency") +
  theme_bw()

# save the histogram
ggsave(filename = "results/dropout-rate-histogram.png", 
       plot = z, 
       device = "png", 
       width = 5, 
       height = 3)
