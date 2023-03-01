rm(list = ls())

library(tidyverse)
library(readxl)
library(openxlsx)
library(unheadr)
library(zoo)
source("R/function_tidy_data.R")

## !!!need to check, env variable not working
CC_RB_LF_SCH_MONTHLY="/Users/mehedi/Library/CloudStorage/GoogleDrive-geocrunchy@gmail.com/My Drive/carter_center/RBLF/Country Monthly Reports"
path <-  paste0(CC_RB_LF_SCH_MONTHLY,"/ETH/new_format_example/202203/202203_r1_dawuro_konta_20230218.xlsx")


### reading the dataset
df_list <- read_data(data_path = path,sheet_contains = "Total",
               cols_words_replace_to_na = c("x_","_for_oncho_lf"))
df_list$dawuro_zone_total$round

## check if the column name accross the dataframes are matching or not, Only for getting the general idea.
check_cols <- df_list %>% check_cols_name()

######### test data
needed_cols <- c("base_name","round","year","month","admin_2","name_of_woredas", "number_of_hd_as", "census_population",
                 "eligible_population", "treated_population_5_14_years_male",
                 "treated_population_5_14_years_female", "treated_population_15_years_above_male",
                 "treated_population_15_years_above_female", "treated_population_total_treated",
                 "tc", "utg", "not_treated_population_absentees", "not_treated_population_refusals",
                 "not_treated_population_s_sick", "not_treated_population_pregnants",
                 "not_treated_population_5_yrs_old_children", "not_treated_population_lactating_mothers_1_week",
                 "not_treated_population_total_not_treated", "ivermectin_balance_received",
                 "ivermectin_balance_distributed", "ivermectin_balance_wasted",
                 "ivermectin_balance_remained", "ivermectin_balance_i_p",
                 "filename", "admin_2","base_name","round","year","month")

output <- bind_data(df_list,needed_cols = needed_cols)


### Summarizing
co <- c("treated_population_5_14_years_male",
        "treated_population_5_14_years_female", "treated_population_15_years_above_male",
        "treated_population_15_years_above_female", "treated_population_total_treated")
summarise_df <- summarise_accross(df = output$binded_df,group_cols = "admin_2",cols = co)

#### checking if the total treated is accurate or not
col <- co[!co %in% "treated_population_total_treated"]

summarise_df <- summarise_df %>% mutate(
  total = rowSums(summarise_df[col],na.rm=T)
) %>% filter(total ==treated_population_total_treated)

