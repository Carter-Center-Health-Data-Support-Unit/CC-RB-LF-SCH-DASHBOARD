#' compile_tab
#'
#' @param folder_path path to directory
#' @param which_tabs tabs to compile (default = "RB_rx")
#' @return list of data.frames each data.frame containing the tab specified.
#' @export
#'
#' @examples \dontrun{
#' root_dir <- Sys.getenv("CC_RB_LF_SCH_MONTHLY")
#' data_dir <- glue::glue("{root_dir}/ETH/data_raw/")
#' dataset_list <- compile_tab(folder_path = data_dir,which_tabs = "RB_rx")
#' }
#'
compile_tab <-  function(folder_path, which_tabs = c("RB_rx")){
  if(which_tabs=="RB_rx"){
    sel_tabs <- "^RB Rx_Bi-annual$|^RB Rx$"
  }
  file_names_short<-dir(data_dir,full.names = F)
  file_names <- dir(data_dir,full.names = T)
  file_dfs<-file_names |>
    purrr::map(
      ~{
        sheet_names<- readxl::excel_sheets(.x)
        soi<- stringr::str_subset(sheet_names,pattern = sel_tabs)
        if(length(soi)>0){
          return(readxl::read_excel(.x,sheet = soi,skip = 1) )
        }
      }
    ) |>
    rlang::set_names(file_names_short)

  file_dfs |>
    purrr::discard(is.null)
}
