

#' identify spill over events
#'
#' @param df data.frame
#' @param x \code{character} vector name containing numeric col to id spill overs with
#' @param ... variables to group_by
#'
#' @return df of just spill over events
#'
#' @examples \dontrun{
#' library(targers)
#' tar_load(RB_post201905_adm3)
#' id_spillovers(RB_post201905_adm3,"popn_treated_during_current_month",adm1_name,adm2_name,adm3_name,year)
#' }

id_spillovers <- function(df, x,grp_vars){
  grp_vars_sym = syms(grp_vars)
  adm_grp_strings <- grp_vars[grp_vars!='year']
  grp_vars_join <- grp_vars %>% set_names(c(adm_grp_strings,"lag_year"))

  lookup_table_sum <- df %>%
    group_by(!!!grp_vars_sym) %>%
    summarise(
      year_sum = sum(!!sym(x),na.rm=T),
      .groups = "drop"
    )



  df_w_prev_year_cum <- df %>%
    mutate(
      !!x:= replace_na(!!sym(x),0)
    ) %>%
    filter(month %in% c(1,2,3)) %>%
    mutate(
      lag_year = year(date)-1
    ) %>%
    left_join(lookup_table_sum, by=grp_vars_join
                # c("adm1_name"= "adm1_name","adm2_name"="adm2_name", "adm3_name"="adm3_name", "lag_year"="year")
              )

  df_w_prev_year_cum %>%
    mutate(
      diff_measure = ifelse(!!sym(x) ==0, Inf, abs(year_sum - !!sym(x))),
      criteria= diff_measure<5000
    ) %>%
    filter(criteria)


}


#' fix spill overs
#'
#' @param df data.frame
#' @param x \code{character} vector name containing numeric col to id spill overs with
#' @param ... variables to group_by
#' @return original df with spill over events correctly set to 0
#' @examples \dontrun{
#' library(targets)
#' tar_load(RB_post201905_adm3)
#' fixed_example <- fix_spillovers(RB_post201905_adm3,"popn_treated_during_current_month",adm1_name,adm2_name,adm3_name,year)
#' id_spillovers(fixed_example,"popn_treated_during_current_month",adm1_name,adm2_name,adm3_name,year)
#'
#' }

fix_spillovers <- function(df, x,...){
  spill_over_events <- id_spillovers(df,x,...)
  spill_over_events_fixed <- spill_over_events %>%
    mutate(
      !!x:=0
    ) %>%
    select(...,x,date)

  dplyr::rows_update(df, spill_over_events_fixed,by=c("adm1_name","adm2_name","adm3_name","date")) %>%
    mutate(
      !!x:=replace_na(!!sym(x),0)
    )
}
