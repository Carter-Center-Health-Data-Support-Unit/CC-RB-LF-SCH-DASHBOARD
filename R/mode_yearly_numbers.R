
mode_yearly_numbers <- function(df, num_cols=c("utg_2_treatment_target_for_the_whole_year",
                                               "active_villages_for_the_year",'total_population'),
                                grp_vars=c("year","adm1_name","adm2_name","adm3_name"),na.rm=T){
  grp_vars_syms <-  syms(grp_vars)
  mode_df <- df %>%
    group_by(!!!grp_vars_syms) %>%
    summarise(
      across(all_of(num_cols),\(x){round(Mode(x,na.rm=na.rm),0)}),
             .groups="drop"
    )
  df %>%
    select(- all_of(num_cols)) %>%
    left_join(mode_df,by = grp_vars)
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

Mode <- function(x,na.rm=T) {
  if(na.rm){
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
