# load libraries
library(lubridate)
library(tidyverse)

# load raw case data
grad_data_raw = read_csv(file = "data/raw/grad_data_raw.csv",
                         col_types = "fdfdffdfdfdfddfddfdfddddddddddddddddd")

# clean case data
grad_data = grad_data_raw %>%   
  select(-lea_name, -entity_inactive_date, -nrc_desc, -boces_name, -aggregation_type, -aggregation_code, -lea_beds, -nrc_code, -boces_code, -nyc_ind) %>%
  mutate(grad_rate = grad_cnt/enroll_cnt*100) %>%
  mutate(local_rate = local_cnt/enroll_cnt*100) %>%
  mutate(reg_rate = reg_cnt/enroll_cnt*100) %>%
  mutate(reg_adv_rate = reg_adv_cnt/enroll_cnt*100) %>%
  mutate(non_diploma_credential_rate = non_diploma_credential_cnt/enroll_cnt*100) %>%
  mutate(still_enr_rate = still_enr_cnt/enroll_cnt*100) %>%
  mutate(ged_rate = ged_cnt/enroll_cnt*100) %>%
  mutate(dropout_rate = dropout_cnt/enroll_cnt*100) %>%
  select(-grad_pct, -local_pct, -reg_pct, -reg_adv_pct, -non_diploma_credential_pct,
         -still_enr_pct, -ged_pct, -dropout_pct) %>%
  na.omit()
  

# write cleaned data to file
write_csv(grad_data, file = "data/clean/grad_data.csv")
