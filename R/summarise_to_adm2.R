
summarise_to_adm2 <- function(df=RB_post201905_df_compiled

){
    sum_cols = c(
      "popn_treated_during_current_month",
      "utg_treatment_target_for_each_round",
      "utg_2_treatment_target_for_the_whole_year",
      "total_popn_projected",
      "popn_treated_round1",
      "popn_treated_round2",
      "popn_treated_cumulative_all_rounds",
      "active_villages_for_the_year",
      "villages_treated_during_current_month",
      "villages_treated_round_1",
      "villages_treated_round_2",
      "total_popn_census",
      "total_population"
      )
    pct_cols = c("percent_utg_treated_round_1",
                 "percent_utg_2_treated_all_rounds",
                 "percent_popn_treated_round_1",
                 "percent_popn_treated_round_2",
                 "percent_active_villages_treated_for_round_1",
                 "percent_active_villages_treated_for_round_2")

  df |>
    dplyr::group_by(year, month,date, file_name, adm1_name,adm2_name) |>
    dplyr::summarise(
      # old format does not contain any cols we want to summarise not in the list above,
      # but the lists above DO contain some variables not in old format data... therefore: dplyr::any_of
      across(.cols = dplyr::any_of(sum_cols),.fns = ~sum(.x,na.rm = T)),
      across(.cols= dplyr::any_of(pct_cols),.fns = ~mean(.x,na.rm=T)),
      .groups = "drop"
    )

}




