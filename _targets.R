# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(lubridate)
library(tidyverse)
library(janitor)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tidyverse","here","janitor"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)
# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)
# source("other_functions.R") # Source other scripts as needed. # nolint


root_dir <- Sys.getenv("CC_RB_LF_SCH_MONTHLY")
data_dir <- glue::glue("{root_dir}/ETH/data_raw/")
# Replace the target list below with your own:


list(

  # targets::tar_target(f)
  # Load/track Inputs ------------------------------------------------------------
  # compile current format RB rx tabs into list of dfs
  tar_target(RB_post201905_df_ls,
             compile_tab(folder_path = data_dir,which_tabs = "RB_rx")
  ),
  # compile old format  RB rx tabs into list of dfs (different tab name/structure)
  tar_target(RB_pre201905_df_ls,
               compile_tab(folder_path = data_dir, which_tabs = "Active TX UTG2",skip = 2 )
  ),
  tar_target(RB_post_training_data,
               compile_tab(folder_path = data_dir, which_tabs = "RB_training",skip = 0 ) %>%
               map(
                 \(df_temp){
                   df_temp %>%
                     unheadr::mash_colnames(n_name_rows = 4,sliding_headers = T) %>%
                     clean_names() %>%
                     mutate(
                       uid = row_number(),
                       across(everything(),~as.character(.x))
                     ) %>%
                     pivot_longer(-uid) %>%
                     mutate(
                       name= case_when(
                         str_detect(name,"zone")~"Zone",
                         str_detect(name, "region")~"region",
                         str_detect(name,"name_of_woredas")~"name_of_woredas",
                         str_detect(name,"number_of_communities")~"number_of_communities",
                         str_detect(name,"d_ds_actual_trained_in_current_month_ato")~"cdd_ato",
                         str_detect(name,"drug_distributors_cd_ds_new")~"cdd_new",
                         str_detect(name,"drug_distributors_cd_ds_refresher")~"cdd_refresher",
                         str_detect(name,"cs_actual_trained_in_current_month_ato")~"cs_ato",
                         str_detect(name,"cs_new")~"cs_new",
                         str_detect(name,"cs_refresher")~"cs_refresher",
                         str_detect(name,"h_ws_actual_trained_in_current_month_ato")~"hw_ato",
                         str_detect(name,"h_ws_new")~"hw_new",
                         str_detect(name,"h_ws_refresher")~"hw_refresher",
                         TRUE~name
                       )
                     ) %>%
                     pivot_wider(id_cols = uid) %>%
                     select(any_of(c("Zone", "region", "name_of_woredas", "number_of_communities")),
                            matches("_refresher$|_new$|_ato$")) %>%
                     mutate(across(everything(),~as.character(.x)))
                 }

               )

  ),
  # track master file
  tar_target(eth_master_adm_fp,
             here::here("data/eth_adm_master.rds"), format = "file"
  ),
  # track master file object
  tar_target(eth_master_adm,
             readr::read_rds(here::here("data/eth_adm_master.rds")) |>
               adjust_master_admin_file()
  ),
  # file to harmonize old and new col names
  tar_target(
    RB_colname_harmonize_lookup_fp,
    here::here("colname_harmonization_lookup.xlsx"),format= "file"
             ),
  # make file a target -- it will get updated (need to update)
  tar_target(
    RB_colname_harmonize_lookup,
    readxl::read_xlsx(here::here("colname_harmonization_lookup.xlsx"))
             ),

  # Clean RB (Post) ------------------------------------------------------
  # initial cleaning. First do for current format data
  # grab top table, clean admin names, drop summary rows

  tar_target(
    name = RB_post201905_df_ls_clean1,
    command = extract_pre_clean_names_adms_batch(df_list =RB_post201905_df_ls,data_format = "current" )
  ),
  tar_target(
    name = RB_post201905_training_compiled,
    command = extract_pre_clean_names_adms_batch(df_list =RB_post_training_data,data_format = "current" ) %>%
      map(\(df_temp){
        df_temp %>%
          mutate(
            across(everything(), ~as.character(.x))
          )%>%
          mutate(
            year=str_sub(string = file_name,start = 1,end = 4) |> as.numeric(),
            month=str_sub(string = file_name,start = 5,end = 6) |> as.numeric(),
            date= lubridate::ymd(glue::glue("{year}-{month}-01")),
            reporting_level= "admin 3"
          ) |>
          mutate(
            across(everything(), ~as.character(.x))
          )
      }
      ) %>%
      bind_rows() %>%
      readr::type_convert()

  ),
  tar_target(
    name = RB_post201905_adm3_compiled,
    command = bind_rows_add_dates(df_list =RB_post201905_df_ls_clean1,data_format = "current")
  ),
  tar_target(name=RB_post201905_adm3_dedup,
             command=deduplicate(df=RB_post201905_adm3_compiled,adm1_name,adm2_name,adm3_name,date)
               ),

  tar_target(name=RB_post201905_adm3_spillfix,
             command=fix_spillovers(RB_post201905_adm3_dedup,"popn_treated_during_current_month",grp_vars =c("adm1_name","adm2_name","adm3_name","year"))
               ),
  tar_target(name=RB_post201905_adm3,


             # fill in all combos
             command=
               RB_post201905_adm3_spillfix %>%
               complete(nesting(adm1_name,adm2_name,adm3_name),date,
                        fill = list(
                          popn_treated_during_current_month=0,
                          villages_treated_during_current_month=0
                        )
               ) %>%
               mutate(
                 month=month(date),
                 year = year(date),
                 month_fmt= formatC(month,width=2,flag="0"),
                 file_name= glue::glue("{year}{month_fmt}_ETH_CCRBLFSCHI_Monthly.xlsx"),
                 popn_treated_during_current_month=replace_na(popn_treated_during_current_month,0)
               ) %>%
               select(-month_fmt) %>%
               # mode of yearly summary figures so they dont change per year
               mode_yearly_numbers(num_cols=c("utg_2_treatment_target_for_the_whole_year",
                                                                                "utg_treatment_target_for_each_round",
                                                                             "active_villages_for_the_year",'total_population'),
                                         grp_vars=c("year","adm1_name","adm2_name","adm3_name")) %>%



               fix_spill_adm2(adm2_issue="east_hararge",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="west_hararge",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="kefa",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="dawuro",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="dawuro",
                              date_issue = "2020-01-01") %>%
               fix_spill_adm2(adm2_issue="mirab_omo",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="bench_sheko",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="central_gondar",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="gofa",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="majang",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="sheka",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="south_omo",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="agnewak",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="awi",
                              date_issue = "2021-01-01") %>%
               fix_spill_adm2(adm2_issue="west_gondar",
                              date_issue = "2021-01-01") %>%
               group_by(year,adm1_name,adm2_name,adm3_name) %>%
               mutate(
                 cum_treated_yr = cumsum(popn_treated_during_current_month),
                 cum_treated_yr_fix = cumsum(pop_treated_monthly_fix),
                 cum_villages_yr = cumsum(villages_treated_during_current_month)
               ) %>%
               ungroup() %>%
               mutate(
                 pct_utg_treated_yr = cum_treated_yr/utg_2_treatment_target_for_the_whole_year,
                 pct_utg_treated_yr_fix = cum_treated_yr_fix/utg_2_treatment_target_for_the_whole_year,
                 pct_pop_treated_yr = cum_treated_yr/total_population,
                 pct_pop_treated_yr_fix = cum_treated_yr_fix/total_population,
                 pct_villages_yr = cum_villages_yr/active_villages_for_the_year

               )
               ),


  # we need to make two data sets from current: a.) admin 2 level (for binding with old), admin 3 level
  tar_target(
    name = RB_post201905_adm2,
    command= summarise_to_adm2(RB_post201905_adm3)

  ),

  # Clean RB (Pre) --------------------------------------------------
  # initial cleaning
  tar_target(
    name = RB_pre201905_df_ls_clean1a,
    command = extract_pre_clean_names_adms_batch(df_list =RB_pre201905_df_ls,
                                                 data_format = "old" )
  ),
  # old format data only has admin 2 so need to join other admins based on master
  tar_target(
    name = RB_pre201905_df_ls_clean1b,
    command = join_master_admin_to_pre201905_data(df_list =RB_pre201905_df_ls_clean1a,
                                                  master_adm = eth_master_adm )

  ),
  # Harmonize "pre" column names with lookup table (after this they will match "post")
  tar_target(
    name = RB_pre201905_df_compiled,
      command = rename_cols_lookup_add_dates_batch(df_list =RB_pre201905_df_ls_clean1b,
                                                   colname_lookup = RB_colname_harmonize_lookup,
                                                   lookup_fixed = F)
  ),
  # check implications of this, but i think just smoothes out/cleans up some potential duplicate issues?
  # one thing i noted is that metekel dam workers & metekel admin 2 gets summed here... i think that seems okay at adm2 level?
  tar_target(
    name = RB_pre201905_to_adm2,
    command= RB_pre201905_df_compiled %>%
      mode_yearly_numbers(num_cols=c("utg_2_treatment_target_for_the_whole_year","utg_treatment_target_for_each_round",
                                     "active_villages_for_the_year",'total_population'),
                          grp_vars=c("year","adm1_name","adm2_name")) %>%
      mutate(pop_treated_monthly_fix=popn_treated_during_current_month) %>%  # dummy holder for now
      summarise_to_adm2()
  ),
  tar_target(
    name = RB_pre201905_adm2,
    command= fix_spillovers(RB_pre201905_to_adm2,x ="popn_treated_during_current_month",grp_vars = c("adm1_name","adm2_name","year") )
  ),

# RB Merge Pre & Post -----------------------------------------------------
  tar_target(
    name = RB_pre_post_compiled,
      command = dplyr::bind_rows(RB_pre201905_adm2,RB_post201905_adm2) %>%
      mode_yearly_numbers(num_cols=c("utg_2_treatment_target_for_the_whole_year",
                                                                 "active_villages_for_the_year",'total_population'),
                          grp_vars=c("year","adm1_name","adm2_name")) %>%
      group_by(year,adm1_name,adm2_name) %>%
      mutate(
        cum_treated_yr = cumsum(popn_treated_during_current_month),
        cum_treated_yr_fix = cumsum(popn_treated_during_current_month_fix),
        cum_villages_yr = cumsum(villages_treated_during_current_month)
      ) %>%
      ungroup() %>%
      mutate(
        pct_utg_treated_yr = cum_treated_yr/utg_2_treatment_target_for_the_whole_year,
        pct_utg_treated_yr_fix = cum_treated_yr_fix/utg_2_treatment_target_for_the_whole_year,
        pct_pop_treated_yr = cum_treated_yr/total_population,
        pct_pop_treated_yr_fix = cum_treated_yr_fix/total_population,
        pct_villages_yr = cum_villages_yr/active_villages_for_the_year

      )
  ),


  ### basically finished RB_Data_cleaninig chunk ending 381


  # LF Rx: Clean  New Phase 2 Data ------------------------------------------------------
  # initial cleaning. First do for current format data
  # grab top table, clean admin names, drop summary rows

  tar_target(LFrx_post201905_df_ls,
             compile_tab(folder_path = data_dir,which_tabs = "LF_rx")
  ),
  tar_target(
    name = LFrx_post201905_df_ls_clean1,
    command = extract_pre_clean_names_adms_batch(df_list =LFrx_post201905_df_ls,data_format = "current" )
  ),
  tar_target(
    name = LFrx_post201905_adm3,
    command = bind_rows_add_dates(df_list =LFrx_post201905_df_ls_clean1)
  ),
  tar_target(
    name = LFrx_post201905_adm2,
    command= summarise_to_adm2(LFrx_post201905_adm3)
  ),

# LF PRE data -------------------------------------------------------------

tar_target(LFrx_pre201905_df_ls,
           compile_tab(folder_path = data_dir,
                       # should improve regex handling so i don't have to write escape
                       # characters for this tab
                       which_tabs = "Active TX \\(LF_TX\\)",
                       skip = 2 )
),


# LFrx PRE Phase 1 Data --------------------------------------------------
tar_target(
  name = LFrx_pre201905_df_ls_clean1a,
  command = extract_pre_clean_names_adms_batch(df_list =LFrx_pre201905_df_ls,
                                               data_format = "old" )
),
# old format data only has admin 2 so need to join other admins based on master
tar_target(
  name = LFrx_pre201905_df_ls_clean1b,
  command = join_master_admin_to_pre201905_data(df_list =LFrx_pre201905_df_ls_clean1a,
                                                master_adm = eth_master_adm )
),
# secondary cleaning first old, then new.
tar_target(
  name = LFrx_pre201905_df_compiled,
  command = rename_cols_lookup_add_dates_batch(df_list =LFrx_pre201905_df_ls_clean1b,
                                               colname_lookup = RB_colname_harmonize_lookup,
                                               lookup_fixed = F)
),
# check implications of this, but i think just smoothes out cleans up some poential duplicate issues?
tar_target(
  name = LFrx_pre201905_adm2,
  command= summarise_to_adm2(LFrx_pre201905_df_compiled)
),

tar_target(
  name = LFrx_pre_post_compiled,
  command = dplyr::bind_rows(LFrx_pre201905_adm2,LFrx_post201905_adm2)
)

)

# tar_invalidate(RB_post201905_adm3)
# tar_prune()
# tar_make()
# tar_load_everything()
