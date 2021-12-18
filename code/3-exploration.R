# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(maps)                           # for creating maps
library(tidyverse)
library(ggplot2)
library(ggcorrplot)

# read in the cleaned data
grad_train = read_csv("data/clean/grad_train.csv")



# calculate median graduation rate
median_grad_rate = grad_data %>%
  summarise(median(as.numeric(grad_rate))) %>%
  pull()


# create histogram of graduation rate
p = grad_train %>%
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

# calculate median dropout rate
median_dropout_rate = grad_train %>%
  summarise(median(as.numeric(dropout_rate))) %>%
  pull()


# create histogram of dropout rate
z = grad_train %>%
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


#graduation rate based on subgroup
subgroup_grad_rate = grad_train %>% group_by(subgroup_code) %>% summarize(avg_grad_rate = mean(grad_rate))

k = subgroup_grad_rate %>% ggplot() +
  geom_boxplot(aes(x = as.factor(subgroup_code), y = avg_grad_rate)) +
  ylab("Average Graduation Rate")+
  xlab("Subgroup Code")

ggsave(filename = "results/subgroup-grad-rate.png", 
       plot = k, 
       device = "png", 
       width = 5, 
       height = 3)
  

#graduation rate based on nrc
nrc_grad_rate = grad_train %>% group_by(nrc_code) %>% summarize(avg_grad_rate = mean(grad_rate))

j = nrc_grad_rate %>% ggplot() +
  geom_boxplot(aes(x = as.factor(nrc_code), y = avg_grad_rate)) +
  ylab("Average Graduation Rate")+
  xlab("NRC Code")

ggsave(filename = "results/nrc-grad-rate.png", 
       plot = j, 
       device = "png", 
       width = 5, 
       height = 3)

#graduation rate based on county
county_grad_rate = grad_train %>% group_by(county_code) %>% summarize(avg_grad_rate = mean(grad_rate))

h = county_grad_rate %>% ggplot() +
  geom_boxplot(aes(x = as.factor(county_code), y = avg_grad_rate)) +
  ylab("Average Graduation Rate")+
  xlab("County Code")

ggsave(filename = "results/county-grad-rate.png", 
       plot = h, 
       device = "png", 
       width = 5, 
       height = 3)

#save the covariance between features
num_variables = grad_train %>% select(-report_school_year, -aggregation_name, -county_name, -membership_desc,
                                      -subgroup_name)
corr <- cor(num_variables)
f= ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white")
ggsave(filename = "results/covariance-plot.png", 
       plot = f, 
       device = "png", 
       width = 5, 
       height = 3)



# examine counties with schools that have 100% graduation rate
top_grad_rate = grad_data %>% 
  select(county_code, county_name, grad_rate) %>%
  distinct() %>%
  arrange(desc(grad_rate)) %>%
  select(county_name, grad_rate) %>%
  head(62)

top_grad_rate %>% kable(format = "latex", row.names = NA,
                        booktabs = TRUE,
                        digits = 2,
                        col.names = c("County Name",
                                      "Graduation Rate"),
                        caption = "These are the counties that have at least one
                        subgroup at a school with a graduation rate of 100%") %>%
  kable_styling(position = "center")



