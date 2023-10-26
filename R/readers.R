#' detect_header_index
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
detect_header_index <- function(df,guess_max=10){
  df_candidate <- df %>%
    as.matrix() %>%
    t() %>%
    as.data.frame.matrix() %>%
    summarise(
      across(everything(),~sum(nchar(.x),na.rm=T),.names = "nchr_{.col}"),
      across(everything(),~sum(!is.na(.x),na.rm=T),.names = "na_{.col}"),
    ) %>%
    pivot_longer(everything()) %>%
    mutate(
      cat = str_extract(name,"nchr|na"),
      id = parse_number(name)
    ) %>%
    filter(id %in% 1:guess_max) %>%
    group_by(cat) %>%
    filter(
      value==max(value)
    )
  idx_unique <- min(unique(df_candidate$id))
  return(idx_unique)

}
