library(targets)
library(tidyverse)

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' library(targets)
#' library(tidyverse)
#' tar_load(RB_pre_post_compiled)
#' RB_pre_post_compiled_complete <- make_rb_dates_complete(RB_pre_post_compiled)
#'
#' # check each admin should have 12 records per year (except 2023)
#' RB_pre_post_compiled_complete %>%
#'   group_by(adm1_name,
#'            adm2_name,
#'            year= year(date)
#'            ) %>%
#'            count() %>%
#'            print(n=nrow(.))
#' }
make_rb_dates_complete <-  function(df){
  rb_range_dates <- df$date %>% range()
  date_seq  <- seq(rb_range_dates[1],rb_range_dates[2], by ="month")
  df %>%
    complete(
      adm1_name,adm2_name, date= date_seq,
    ) %>%
    mutate(
      across(where(is.numeric),~replace_na(.x,0)),
      year = year(date),
      month= month(date)
    )
}

# rb_range_dates <- RB_pre_post_compiled$date %>% range()
# date_seq  <- seq(rb_range_dates[1],rb_range_dates[2], by ="month")
#
# cols_num <- RB_pre_post_compiled %>%
#   select(where(is.numeric)) %>%
#   colnames()
# RB_pre_post_compiled %>%
#   complete(
#     adm1_name,adm2_name, date= date_seq,
#   ) %>%
#   mutate(
#     across(where(is.numeric),~replace_na(.x,0)),
#     year = year(date),
#     month- month(date)
#   )
#
#
#   group_by(adm1_name,
#            adm2_name,
#            year= year(date)) %>%
#   count()
#

