rm(list = ls())

library(tidyverse)
library(readxl)
library(unheadr)
source("R/function_tidy_data.R")

## !!!need to check, env variable not working
CC_RB_LF_SCH_MONTHLY="/Users/mehedi/Library/CloudStorage/GoogleDrive-geocrunchy@gmail.com/My Drive/carter_center/RBLF/Country Monthly Reports"
path <-  paste0(CC_RB_LF_SCH_MONTHLY,"/ETH/new_format_example/202203/202203_r1_dawuro_konta_20230218.xlsx")


### reading the dataset
df_list <- read_data(data_path = path,sheet_contains = "Total",
               cols_words_replace_to_na = c("x_","_for_oncho_lf"))

## check if the column name accross the dataframes are matching
check_cols <- df_list %>% check_cols_name()
