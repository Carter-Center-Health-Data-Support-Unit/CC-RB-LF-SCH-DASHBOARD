

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
  # grp_vars_join <- c(adm_grp_strings,"lag_year") %>% set_names(grp_vars)

  lookup_table_sum <- df %>%
    filter(month(date)%in% c(10,11,12)) %>%
    group_by(!!!grp_vars_sym) %>%
    filter(!is.na(!!sym(x))) %>%
    summarise(
      year_sum = max(!!sym(x),na.rm=T),
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
      pct_measure = ifelse(!!sym(x) ==0|year_sum==0, -Inf, !!sym(x)/year_sum),
      criteria= pct_measure>0.95 #diff_measure<5000
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

fix_spillovers <- function(df, x,grp_vars){
  spill_over_events <- id_spillovers(df,x,grp_vars = grp_vars)
  fix_colname = paste0(x,"_fix")
  spill_over_events_fixed <- spill_over_events %>%
    mutate(
      !!fix_colname:=0
    ) %>%
    select(any_of(c(grp_vars,x,fix_colname,"date")),-any_of(c(x,"year")))

  adm_grp_strings <- grp_vars[grp_vars!='year']
  join_strings <- c(adm_grp_strings,"date")

  # want to have the fixed value compared to last so going to left joing
  df %>%
    left_join(spill_over_events_fixed, by = join_strings) %>%
    mutate(
      !!fix_colname:=ifelse(is.na(!!sym(fix_colname)),!!sym(x),!!sym(fix_colname)),
      !!fix_colname:= replace_na(!!sym(fix_colname),0)
    )

  # if we just want to update the values rather than compare than this is better:
  # dplyr::rows_update(df, spill_over_events_fixed,by=join_strings) %>%
  #   mutate(
  #     !!x:=replace_na(!!sym(x),0)
  #   )
}


#' Title
#'
#' @param df
#' @param adm2_issue \code{character} adm2 name with issue
#' @param date_issue  \code{character} date of issue in YYYY-mm-dd format
#' @return
#' @export
#'
#' @examples

fix_spill_adm2 <- function(df,

                           adm2_issue = "east_hararge",
                           date_issue="2021-01-01"
){

  assertthat::assert_that(adm2_issue%in%df$adm2_name,msg = "mistake in adm2 issue")
  yr_issue <-  year(ymd(date_issue))
  mo_issue <-  month(ymd(date_issue))
  prev_yr = yr_issue-1
  prev_mo= 12
  if(!"pop_treated_monthly_fix" %in% colnames(df)){
    df <- df %>%
      mutate(pop_treated_monthly_fix = popn_treated_during_current_month)
  }


  month_id_fix <- df %>%
    filter(
      adm2_name==adm2_issue,
      year== yr_issue,
      month==mo_issue
    ) %>%
    mutate(pop_treated_monthly_fix = 0)

  prev_month_fix <- month_id_fix %>%
    select(adm1_name,adm2_name,adm3_name, pop_treated_monthly_fix= popn_treated_during_current_month) %>%
    mutate(year=prev_yr,
           month= prev_mo)
  df %>%
    dplyr::rows_update(month_id_fix, by= c("year","month","adm1_name","adm2_name","adm3_name")) %>%
    dplyr::rows_update(prev_month_fix, by= c("year","month","adm1_name","adm2_name","adm3_name"))
}

