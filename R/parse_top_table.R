

#' first_empty_row
#'
#' @param df dataframe
#'
#' @return return index of first completely empty row
#' @export
#'
#' @examples
first_empty_row <- function(df){

    mask_keep <- rowSums(is.na(df)) != ncol(df)

  idx_blank <- which(mask_keep==F)
  if(length(idx_blank)>0){
    res <- idx_blank <-   idx_blank |> min()
  }
  else{
    res <- idx_blank
  }
  return(res)

}

#' select_top_table
#'
#' @param df dataframe
#'
#' @return dataframe containing all rows up to first empty row
#' @export
#'
#' @examples \dontrun{
# top_tables<- new_format_datasets |>
#   purrr::map(~extract_top_table(.x))
# top_tables |>
#   map(
#     ~.x |>
#       filter(is.na(Region))
#
#   )
#
#'
#' }
extract_top_table <-  function(df,pct_thresh){
  # print(colnames(df))
  end_table_idx <- first_empty_row(df)-1
  if(length(end_table_idx)>0)
      res <- df |>
        slice(1:end_table_idx)

  if(length(end_table_idx)==0){
    res <- df
  }
  # print(colnames(res))
  # assertthat::assert_that(res %>% filter(Zone=="North Shoa-Am") %>% nrow()==0,msg = "parse top table - herest the issue")
  return(res)
}

#' parse_top_table
#'
#' @param df
#'
#' @return dataframe containing rows up to first empty row with columns parsed using `readr::parse_guess()`
#' @export
#'
#' @examples
parse_top_table <-  function(df){
  df_res <- df |>
    extract_top_table(pct_thresh)
  # assertthat::assert_that(df_res %>% filter(Zone=="North Shoa-Am") %>% nrow()==0,msg = "parse top table - herest the issue")
    res <- suppressMessages(readr::type_convert(df_res %>% mutate(across(everything(),~as.character(.x)))))
    return(res)

}




