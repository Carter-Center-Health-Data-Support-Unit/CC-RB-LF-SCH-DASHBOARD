#' Title
#'
#' @param df
#' @param lookup lookup table
#' @param lookup_fixed temporary argument to "ignore" issues in lookup table so that we can continue developing function. Once fixed, this will be removed and function will accurately rename
#' @return
#' @export
#'
#' @examples

rename_cols_with_lookup <-  function(df,lookup,lookup_fixed=F){

  lookup_filtered<- lookup |>
    dplyr::filter(
      !is.na(old_format_names_clean), # rm blanks
      !is.na(new_format_names), # if there is no matching new name, but we rm these
      old_keep==1, # retain only col names designated to keep
      old_format_names_clean %in% colnames(df)
    ) |>
    dplyr::distinct(old_format_names_clean,new_format_names)



  old_names_rm <-  lookup |>
    filter(old_keep==0) |>
    pull(old_format_names_clean)

  if(!lookup_fixed){

    lookup_filtered <- lookup_filtered |>
      dplyr::group_by(new_format_names) |>
      dplyr::slice(1) |>
      ungroup()
  }


  df_renamed <- df |>
    rename_with(
      .cols = lookup_filtered$old_format_names_clean,
      .fn = ~lookup_filtered$new_format_names
    ) |>
    dplyr::select(- dplyr::any_of(c(old_names_rm)))

  return(df_renamed)
  # if(df_renamed)

}
