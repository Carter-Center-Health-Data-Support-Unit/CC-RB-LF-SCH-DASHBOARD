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
#'
# library(tidyverse)
# library(janitor)
# root_dir <- Sys.getenv("CC_RB_LF_SCH_MONTHLY")
# data_dir <- glue::glue("{root_dir}/ETH/data_raw/")
# library(targets)
# tar_source()
# df <- readxl::read_xlsx(path = file.path(data_dir, "201907_ETH_CCRBLFSCHI_Monthly.xlsx" ),sheet = "RB train & HE")
# df %>%
#   unheadr::mash_colnames(n_name_rows = 4,sliding_headers = T) %>%
#   clean_names() %>%
#   glimpse()
#   mutate(
#     uid = row_number(),
#     across(everything(),~as.character(.x))
#   ) %>%
#   pivot_longer(-uid) %>%
#   mutate(
#     name= case_when(
#       str_detect(name,"zone")~"Zone",
#       str_detect(name, "region")~"region",
#       str_detect(name,"name_of_woredas")~"name_of_woredas",
#       str_detect(name,"number_of_communities")~"number_of_communities",
#       str_detect(name,"d_ds_actual_trained_in_current_month_ato")~"cd_ato",
#       str_detect(name,"cd_ds_new ")~"cdd_new",
#       str_detect(name,"cd_ds_refresher ")~"cdd_refresher",
#       str_detect(name,"cs_actual_trained_in_current_month_ato")~"cs_ato",
#       str_detect(name,"cs_new")~"cs_new",
#       str_detect(name,"cs_refresher")~"cs_refresher",
#       str_detect(name,"cs_actual_trained_in_current_month_ato")~"cs_ato",
#       str_detect(name,"cs_new")~"cs_new",
#       str_detect(name,"cs_refresher")~"cs_refresher",
#       str_detect(name,"h_ws_actual_trained_in_current_month_ato")~"hw_ato",
#       str_detect(name,"h_ws_new")~"hw_new",
#       str_detect(name,"h_ws_refresher")~"hw_refresher"
#     )
#   ) %>%
#   pivot_wider(id_cols = uid)


compile_tab <-  function(folder_path, which_tabs = c("RB_rx"), skip=1){
  if(which_tabs=="RB_rx"){
    sel_tabs <- "^RB Rx_Bi-annual$|^RB Rx$|^RB_Bi-annual Rx$"
  }
  else if(which_tabs=="LF_rx"){
    sel_tabs <- "^LF Rx$"
  }else if(which_tabs=="RB_training"){
    sel_tabs <- "^RB train & HE$"
  }else{
    sel_tabs<- which_tabs
  }
  file_names_short<-dir(data_dir,full.names = F)
  file_names <- dir(data_dir,full.names = T)
  file_dfs<-file_names |>
    # read in file as named list of dfs.
    purrr::map(
      ~{
        sheet_names<- readxl::excel_sheets(.x)
        soi<- stringr::str_subset(sheet_names,pattern = sel_tabs)
        if(length(soi)>0){
          return( suppressMessages( readxl::read_excel(.x,sheet = soi,skip = skip)))

        }
      }
    ) |>
    rlang::set_names(file_names_short)
  file_dfs |>
    purrr::discard(is.null)
}



adm3_heat_chart_height <- function(x){
  chart_height <- switch(x,
                         "amhara" =3,
                         "benishangul_gumz"=3,
                         "gambela"=3,
                         "oromia"=10,
                         "snnp"=3,
                         "south_west_ethiopia"=10
  )
  return(chart_height)
}

read_zipped_layer <- function(path,layer){
  tmp_dir <- tempfile()
  unzip(path, exdir = tmp_dir)
  ret <- layer %>%
    map(
      ~st_read(tmp_dir,.x)
    )
  on.exit(unlink(tmp_dir), add = TRUE)
  return(ret)
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' library(sf)
#' library(tidyverse)
#' list_zipped_shape_layers(path = "/Users/zackarno/Downloads/Ethiopia_AdminBoundaries (1).zip")
#' adm4 <- read_zipped_layer(path = "/Users/zackarno/Downloads/Ethiopia_AdminBoundaries (1).zip",
#'                           layer ="Ethiopia_AdminBoundaries"  )
#' adm3 <- read_zipped_layer(path = "/Users/zackarno/Downloads/Ethiopia_AdminBoundaries.zip",
#'                           layer ="Ethiopia_AdminBoundaries"  )
#' adm4 %>%
#'   pluck(1) %>%
#'   filter(
#'   str_detect(RK_NAME,"Maj")
#'   )
#' adm3 %>%
#'   pluck(1) %>%
#'   filter(
#'   str_detect(WOREDANAME,"Maj")
#'   )
#'}
list_zipped_shape_layers <- function(path){
  tmp_dir <- tempfile()
  unzip(path, exdir = tmp_dir)
  return(sf::st_layers(tmp_dir))
  on.exit(unlink(tmp_dir), add = TRUE)

}




