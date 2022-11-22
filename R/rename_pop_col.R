


#' rename_pop_col_batch
#'
#' @param df_list
#' @param pop_col
#'
#' @return
#' @export
#'
#' @examples
rename_pop_col_batch <-  function(df_list, pop_col="total_popn_projected"){
  pop_col_options <- c("total_popn_projected","total_popn_census")
  replace_this <-  setdiff(pop_col_options,pop_col)
  df_list <-df_list |>
    purrr::map(
      ~{
        x_temp <-  .x
        if(replace_this %in% colnames(.x)){
          x_temp <-  .x |>
            rename(!!pop_col:=replace_this)

        }
        x_temp |>
          dplyr::select(-dplyr::any_of(matches("^x\\d*")))
      }
    )

}


