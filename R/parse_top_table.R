

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
    idx_blank <-   idx_blank |> min()
  }
  else{
    idx_blank
  }

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
extract_top_table <-  function(df){
  end_table_idx <- first_empty_row(df)-1
  if(length(end_table_idx)>0)
    return(
      df |>
        slice(1:end_table_idx)
    )
  if(length(end_table_idx)==0){
    return(df)
  }
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
  df |>
    extract_top_table() |>
    readr::type_convert()

}




